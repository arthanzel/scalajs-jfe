package org.scalajs.jfe.trees

import org.scalajs.jfe.util.TypeUtils._
import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.Trees.OptimizerHints
import org.scalajs.ir.{ClassKind, OriginalName, Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.JavaCompilationException
import org.scalajs.jfe.util.TextUtils

object JDTCompiler {

  import TreeHelpers._

  private val NoOriginalName = OriginalName.NoOriginalName
  private val NoOptimizerHints = OptimizerHints.empty
  private val NoPosition = Position.NoPosition
  private val WithConstructorFlags = js.ApplyFlags.empty.withConstructor(true)
  private val WithConstructorNS = js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor)

  def apply(compilationUnit: jdt.CompilationUnit): List[js.ClassDef] =
    new JDTCompiler(compilationUnit, null).gen()

  /**
   * Terminating statements are statements that certainly cause the containing
   * block to exit prematurely. If a block contains one of these as an immediate
   * child, some of its code can be optimized.
   */
  val terminatingStatements: Set[Class[_ <: jdt.Statement]] = Set(
    classOf[jdt.ReturnStatement],
    classOf[jdt.BreakStatement],
    classOf[jdt.ContinueStatement],
  )

  def containsTerminatingStatement(stats: List[jdt.Statement]): Boolean =
    stats.exists(stat => terminatingStatements.contains(stat.getClass))

  def genMethodParams(method: jdt.MethodDeclaration)
                     (implicit pos: Position): List[js.ParamDef] =
    method.parameters.asScala[jdt.SingleVariableDeclaration].map { p =>
      js.ParamDef(
        js.LocalIdent(jsn.LocalName(p.getName.getIdentifier)),
        NoOriginalName,
        sjsType(p.getType.resolveBinding),
        mutable = isMutable(p.resolveBinding),
        rest = p.isVarargs
      )
    }

  /**
   * Convenience method to throw an NPE. Used in routines that have runtime
   * semantics, like null-checking the `synchronized` expression.
   */
  def genThrowNPE(implicit pos: Position): js.Throw = {
    js.Throw(js.New(
      jsn.ClassName("java.lang.NullPointerException"),
      js.MethodIdent(jsn.MethodName.constructor(Nil)),
      Nil
    ))
  }
}

/**
 * The JDTCompiler class compiles a single JDT type definition into SJSIR code.
 * TODO: Make the class actually do this
 */
private class JDTCompiler(compilationUnit: jdt.CompilationUnit,
                          typeDef: jdt.AbstractTypeDeclaration) {

  import JDTCompiler._
  import TreeHelpers._
  import org.scalajs.jfe.util.TextUtils.freshName

  // region State stuff

  def packageName: Option[String] = {
    if (compilationUnit.getPackage == null) return None
    Some(compilationUnit.getPackage.getName.getFullyQualifiedName)
  }

  def packageName(name: String): String = {
    Seq(packageName, Some(name)).flatten.mkString(".")
  }

  var qualifiedThis: String = ""
  def thisClassName: jsn.ClassName = jsn.ClassName(qualifiedThis)
  def thisClassType: jst.ClassType = jst.ClassType(thisClassName)

  def thisNode(implicit pos: Position): js.This = js.This()(thisClassType)

  lazy val staticInitializerIdent = js.MethodIdent(
    jsn.MethodName(TextUtils.freshName("$sjsirStaticInitializer"), Nil, jst.VoidRef)
  )(NoPosition)

  // Return scopes are created on every method or function definition.
  // Return statements always exit the nearest return scope.
  val returnScope = new ScopedStack[String]
  val breakScope = new ScopedStack[String]
  val continueScope = new ScopedStack[String]

  def withReturnScope(tpe: jst.Type)(body: => js.Tree)(implicit pos: Position) =
    returnScope.withValue(freshName("_return")) { label =>
      js.Labeled(js.LabelIdent(jsn.LabelName(label)), tpe, body)
    }

  def withBreakScope(tpe: jst.Type)(body: => js.Tree)(implicit pos: Position) =
    breakScope.withValue(freshName("_break")) { label =>
      js.Labeled(js.LabelIdent(jsn.LabelName(label)), tpe, body)
    }

  def withContinueScope(tpe: jst.Type)(body: => js.Tree)(implicit pos: Position) =
    continueScope.withValue(freshName("_continue")) { label =>
      js.Labeled(js.LabelIdent(jsn.LabelName(label)), tpe, body)
    }

  // endregion

  def gen(): List[js.ClassDef] = {

    // region Generation methods

    def genClass(cls: jdt.TypeDeclaration): Seq[js.ClassDef] = {
      implicit val position: Position = getPosition(cls)

      qualifiedThis = packageName(cls.getName.getIdentifier)
      val superClassName: String = Option(cls.getSuperclassType)
        .map(_.resolveBinding.getQualifiedName)
        .orElse(Some("java.lang.Object"))
        .get

      // Partition members
      val (jdtStaticFields, jdtInstanceFields) = cls.getFields
        .partition { f => jdt.Modifier.isStatic(f.getModifiers) }
      val jdtConstructors = cls.getMethods.filter(_.isConstructor)

      // Preprocess members to make internal counters consistent
      val sjsFields = cls.getFields.flatMap(genFields)
      val sjsMethods = cls.getMethods.filter(!_.isConstructor).map(genMethod)

      // Generate default constructor
      val ctors = if (jdtConstructors.isEmpty)
        Seq(genDefaultConstructor(jsn.ClassName(superClassName),
          jdtInstanceFields))
      else
        jdtConstructors.map(genConstructor(_, jsn.ClassName(superClassName),
          jdtInstanceFields)).toSeq

      // Generate static accessors and initializers
      val staticSynthetics: Seq[js.MemberDef] =
        if (!jdtStaticFields.isEmpty) {
          val accessors = sjsFields.flatMap(f => genStaticAccessors(f, staticInitializerIdent))

          // Initializers are called in the order by which they appear in the
          // code, so we have no choice but to consider both FieldDeclaration
          // and Initializer types at once
          val jdtInitializers: Seq[jdt.BodyDeclaration] = cls.bodyDeclarations
            .asScala[jdt.BodyDeclaration]
            .filter {
              case _: jdt.FieldDeclaration => true
              case _: jdt.Initializer => true
              case _ => false
            }

          accessors ++ genStaticInitializer(jdtInitializers, staticInitializerIdent)
        }
        else Nil

      // Generate class def
      val body: Seq[js.MemberDef] = sjsFields ++
        staticSynthetics ++
        sjsMethods ++
        ctors.toList ++
        Nil

      val classDef = js.ClassDef(
        js.ClassIdent(jsn.ClassName(qualifiedThis)),
        NoOriginalName,
        ClassKind.Class,
        None,
        Some(js.ClassIdent(jsn.ClassName(superClassName))),
        List(),
        None,
        None,
        body.toList,
        List()
      )(NoOptimizerHints)

      Seq(classDef)
    }

    /*
    JDT provides field initializers alongside the variable declaration AST node,
    but SJSIR disallows that and puts initializers in the constructor instead.
    This is method is called when generating top-level constructors to add
    initializers back in.

    JDT also allows a single declaration node to declare multiple fields of the
    same type, each with different initializers, so a Seq is returned.
     */
    def genFieldInitializers(fieldDecls: Seq[jdt.FieldDeclaration], thisName: jsn.ClassName)
                            (implicit pos: Position): Seq[js.Assign] = {
      fieldDecls.flatMap { field =>
        field.fragments.asScala[jdt.VariableDeclarationFragment]
          .flatMap { frag =>
            frag.getInitializer match {
              case null =>
                None
              case init =>
                val ident = js.FieldIdent(jsn.FieldName(frag.getName.getIdentifier))
                val tpe = sjsType(field.getType.resolveBinding)
                val select = if (jdt.Modifier.isStatic(field.getModifiers)) {
                  js.SelectStatic(thisClassName, ident)(tpe)
                } else {
                  js.Select(thisNode, thisClassName, ident)(tpe)
                }

                Some(js.Assign(
                  select,
                  cast(genExprValue(init, Some(tpe)), tpe)
                ))
            }
          }
      }
    }

    /*
    If the type is implicit constructible, this method creates a synthetic
    constructor taking no parameters that calls the super constructor and
    initializes all fields.
     */
    def genDefaultConstructor(superClassName: jsn.ClassName, fieldDecls: Seq[jdt.FieldDeclaration])
                             (implicit pos: Position): js.MethodDef = {
      val constructorIdent = js.MethodIdent(jsn.MethodName("<init>", Nil, jst.VoidRef))
      val superConstructorCall = js.ApplyStatically(
        WithConstructorFlags,
        thisNode,
        superClassName,
        constructorIdent,
        List()
      )(jst.NoType)
      val fieldInitializers = genFieldInitializers(fieldDecls, thisClassName)

      js.MethodDef(
        WithConstructorNS,
        constructorIdent,
        NoOriginalName,
        List(),
        jst.NoType,
        Some(js.Block(superConstructorCall +: fieldInitializers.toList))
      )(NoOptimizerHints, None)
    }

    def genConstructor(method: jdt.MethodDeclaration,
                       superClassName: jsn.ClassName,
                       fieldDecls: Seq[jdt.FieldDeclaration]): js.MethodDef = {
      /*
      Unlike Scala, Java allows multiple, acyclic constructor paths. However,
      at least one constructor must be the "top-level" one, which either calls
      no other constructors or calls a super constructor. Since co-constructor
      calls must be the first statement in a constructor, top-level constructors
      will always be executed first.

      We put field initializers in top-level constructors.
       */

      // TODO: Place field initializers in their own method if there are multiple
      //  top-level constructors

      implicit val position: Position = getPosition(method)

      val stats = method.getBody.statements.asScala[jdt.Statement]
      val (prelude, body) = stats match {
        case (_: jdt.ConstructorInvocation) :: _ =>
          // This constructor is not a top-level
          // Do not add any synthetic code
          (Nil, stats)

        case (sci: jdt.SuperConstructorInvocation) :: rest =>
          // Constructors calling super must call super first THEN initialize
          // instance fields
          val p = genStatement(sci) +: genFieldInitializers(fieldDecls, thisClassName)
          (p, rest)

        case _ =>
          // Constructor does not delegate. Call the zero-arg super and
          // initialize instance fields.
          val ident = js.MethodIdent(jsn.MethodName("<init>", Nil, jst.VoidRef))
          val superInvocation = js.ApplyStatically(WithConstructorFlags,
            thisNode, superClassName, ident, Nil)(jst.NoType)

          val p = superInvocation +: genFieldInitializers(fieldDecls, thisClassName)
          (p, stats)
      }

      js.MethodDef(
        WithConstructorNS,
        MethodInfo(method.resolveBinding).ident,
        NoOriginalName,
        genMethodParams(method),
        jst.NoType,
        Some(js.Block((prelude ++ body.map(genStatement)).toList))
      )(NoOptimizerHints, None)
    }

    /**
     * Generates SJS field defs for a JDT field declaration.
     * In JDT, a single field declaration can define several fields of the same
     * type.
     */
    def genFields(field: jdt.FieldDeclaration): Seq[js.FieldDef] = {
      implicit val position: Position = getPosition(field)

      def genFragment(frag: jdt.VariableDeclarationFragment): js.FieldDef = {
        js.FieldDef(
          js.MemberFlags.empty
            .withNamespace(memberNamespace(field.getModifiers))
            .withMutable(!jdt.Modifier.isFinal(field.getModifiers)),
          js.FieldIdent(jsn.FieldName(frag.getName.getIdentifier)),
          NoOriginalName,
          sjsType(field.getType.resolveBinding())
        )
      }

      field.fragments.asScala[jdt.VariableDeclarationFragment]
        .map(genFragment)
    }

    def genStaticAccessors(fieldDef: js.FieldDef, staticInitializerIdent: js.MethodIdent): Seq[js.MemberDef] = {
      implicit val pos: Position = fieldDef.pos

      if (!fieldDef.flags.namespace.isStatic) Seq(fieldDef)
      else {
        val flags = js.MemberFlags.empty.withNamespace(fieldDef.flags.namespace)
        val typeRef = sjsTypeRef(fieldDef.ftpe)
        val valueIdent = js.LocalIdent(jsn.LocalName("value"))
        val select = js.SelectStatic(thisClassName, fieldDef.name)(fieldDef.ftpe)

        def withInitializerCall(tree: js.Tree): js.Tree = js.Block(
          js.ApplyStatic(js.ApplyFlags.empty, thisClassName, staticInitializerIdent, Nil)(jst.NoType),
          tree
        )

        Seq(
          // Getter
          js.MethodDef(
            flags,
            js.MethodIdent(jsn.MethodName(
              nameString(fieldDef),
              Nil,
              typeRef)
            ),
            NoOriginalName,
            Nil,
            fieldDef.ftpe,
            Some(withInitializerCall(select))
          )(NoOptimizerHints, None),

          // Setter
          js.MethodDef(
            flags,
            js.MethodIdent(jsn.MethodName(
              s"${nameString(fieldDef)}_$$eq",
              List(typeRef),
              jst.VoidRef
            )),
            NoOriginalName,
            List(js.ParamDef(
              valueIdent, NoOriginalName, fieldDef.ftpe, mutable = false, rest = false)
            ),
            jst.NoType,
            Some(withInitializerCall(
              js.Assign(select, js.VarRef(valueIdent)(fieldDef.ftpe))
            ))
          )(NoOptimizerHints, None)
        )
      }
    }

    def genStaticInitializer(statics: Seq[jdt.BodyDeclaration],
                             staticInitializerIdent: js.MethodIdent): Seq[js.MemberDef] = {
      implicit val pos: Position = NoPosition

      // TODO: Support custom static initializers

      val flagIdent = js.FieldIdent(jsn.FieldName(freshName("$sjsirStaticCalled")))
      val flagSelect = js.SelectStatic(thisClassName, flagIdent)(jst.BooleanType)
      val memberFlags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)

      val flag = js.FieldDef(memberFlags.withMutable(true),
        flagIdent, NoOriginalName, jst.BooleanType)

      val inits: List[js.Tree] = statics.flatMap {
        case field: jdt.FieldDeclaration => genFieldInitializers(Seq(field), thisClassName)
        case init: jdt.Initializer => Seq(genBlock(init.getBody))
      }.toList

      val initializer = js.MethodDef(
        memberFlags,
        staticInitializerIdent,
        NoOriginalName,
        Nil,
        jst.NoType,
        Some(
          js.If(
            js.BinaryOp(
              js.BinaryOp.Boolean_!=,
              flagSelect,
              js.BooleanLiteral(value = true)
            ),
            js.Block(
              js.Assign(flagSelect, js.BooleanLiteral(value = true)),
              js.Block(inits)
            ),
            js.Skip()
          )(jst.NoType)
        )
      )(NoOptimizerHints, None)

      Seq(flag, initializer)
    }

    def genMethod(method: jdt.MethodDeclaration): js.MethodDef = {
      implicit val position: Position = getPosition(method)

      if (method.isConstructor) {
        // Constructors are generated separately since they need to include info
        // like field initializers.
        throw new JavaCompilationException("genMethod shouldn't be passed a constructor")
      }

      val jdtParameters = method.parameters.toArray.map(_.asInstanceOf[jdt.SingleVariableDeclaration]).toList
      val parameterTypeRefs = jdtParameters
        .map(_.getType.resolveBinding)
        .map(sjsTypeRef)
      val resultType = sjsType(method.getReturnType2.resolveBinding)
      val resultTypeRef = sjsTypeRef(method.getReturnType2.resolveBinding)

      js.MethodDef(
        js.MemberFlags.empty.withNamespace(memberNamespace(method.getModifiers)),
        js.MethodIdent(jsn.MethodName(
          method.getName.getIdentifier,
          parameterTypeRefs,
          resultTypeRef
        )),
        NoOriginalName,
        jdtParameters.map { p =>
          js.ParamDef(
            js.LocalIdent(jsn.LocalName(p.getName.getIdentifier)),
            NoOriginalName,
            sjsType(p.getType.resolveBinding),
            mutable = !jdt.Modifier.isFinal(p.getModifiers),
            rest = p.isVarargs
          )
        },
        resultType,
        Some({
          val body = withReturnScope(resultType) {
            genBlock(method.getBody)
          }
          // Unwrap a single-statement return to an expression
          body match {
            case js.Labeled(_, _, ret: js.Return) =>
              ret.expr
            case other => other
          }
        })
      )(NoOptimizerHints, None)
    }

    def genBlock(b: jdt.Block): js.Tree =
      js.Block(b.statements().toArray.map(s => genStatement(s.asInstanceOf[jdt.Statement])).toList)(getPosition(b))

    def genStatement(statement: jdt.Statement): js.Tree = {
      implicit val position: Position = getPosition(statement)

      statement match {
        case s: jdt.AssertStatement => ???

        case s: jdt.Block =>
          // TODO: Does a block create a new lexical scope? Do we need to wrap
          //  this in a labeled statement?
          genBlock(s)

        /*
        TODO: Break and continue
        Scala doesn't have break and continue keywords. SJSIR doesn't either.
        Perhaps implement this by detecting breaks inside loops and wrapping
        the loop (break) or its body (continue) in a try/catch? This looks like
        the way Scala's Breaks do it.

        Another way would be to wrap the loop and body in a labeled statement
        and return to those, but I'm not sure of the semantics of that yet. Need
        to set up functional tests first.
         */
        case s: jdt.BreakStatement => Option(s.getLabel) match {
          case Some(label) =>
            js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName(label.getIdentifier)))
          case None =>
            // Exit the nearest break scope
            js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName(breakScope.get)))
        }

        case s: jdt.ConstructorInvocation =>
          val mi = MethodInfo(s.resolveConstructorBinding)
          js.ApplyStatically(
            WithConstructorFlags,
            thisNode,
            thisClassName,
            mi.ident,
            mi.genArgs(s.arguments.asScala[jdt.Expression].map(genExprValue(_)))
          )(jst.NoType)

        case s: jdt.ContinueStatement => Option(s.getLabel) match {
          case Some(label) =>
            js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName(label.getIdentifier)))
          case None =>
            // Exit the nearest continue scope
            js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName(continueScope.get)))
        }

        case s: jdt.DoStatement =>
          js.DoWhile(genStatement(s.getBody), genExprValue(s.getExpression))

        case s: jdt.EmptyStatement =>
          js.Skip()

        case s: jdt.EnhancedForStatement => js.ForIn(
          genExprValue(s.getExpression),
          js.LocalIdent(jsn.LocalName(s.getParameter.getName.getIdentifier)),
          NoOriginalName,
          genStatement(s.getBody)
        )

        case s: jdt.ExpressionStatement => genExprStatement(s.getExpression)

        case s: jdt.ForStatement =>
          js.Block(
            // Initializers
            s.initializers.asScala[jdt.Expression]
              .map(genExprValue(_)) :+

              // Main loop
              withBreakScope(jst.NoType) {
                js.While(
                  genExprValue(s.getExpression),
                  withContinueScope(jst.NoType) {
                    js.Block(
                      List(genStatement(s.getBody)) ++

                        // Updaters
                        s.updaters.asScala[jdt.Expression]
                          .map(genExprValue(_))
                    )
                  }
                )
              }
          )

        case s: jdt.IfStatement =>
          js.If(
            genExprValue(s.getExpression),
            genStatement(s.getThenStatement),
            Option(s.getElseStatement)
              .map(genStatement)
              .getOrElse(js.Skip())
          )(jst.NoType)

        case s: jdt.LabeledStatement =>
          // TODO: Detect whether the body is a loop and construct break and
          //  continue scope automatically
          js.Labeled(
            js.LabelIdent(jsn.LabelName(s.getLabel.getIdentifier)),
            jst.NoType,
            // TODO: Create a break scope?
            genStatement(s.getBody)
          )

        case s: jdt.ReturnStatement =>
          /*
           Return from the nearest function or method.
           In SJSIR, returns break out of the corresponding labeled block, and
           so every function, method, or lambda enclose their body in a labeled
           statement as a return scope.
           */
          js.Return(
            Option(s.getExpression)
              .map(genExprValue(_))
              .getOrElse(js.Undefined()),
            js.LabelIdent(jsn.LabelName(returnScope.get))
          )

        case s: jdt.SuperConstructorInvocation =>
          val mi = MethodInfo(s.resolveConstructorBinding)
          js.ApplyStatically(
            WithConstructorFlags,
            thisNode,
            mi.declaringClassName,
            mi.ident,
            mi.genArgs(s.arguments.asScala[jdt.Expression].map(genExprValue(_)))
          )(jst.NoType)

        case s: jdt.SwitchCase =>
          throw new JavaCompilationException("Encountered case outside of a switch statement")

        case s: jdt.SwitchStatement =>
          genSwitch(s.getExpression, s.statements.asScala[jdt.Statement])

        case s: jdt.SynchronizedStatement =>
          val body = genBlock(s.getBody)

          // Process the side-effect of the expression
          val expr = genExprValue(s.getExpression)
          val seVarDef = js.VarDef(
            js.LocalIdent(jsn.LocalName(freshName("sideEffect"))),
            NoOriginalName, expr.tpe, mutable = false, expr)

          // Check for nullity. This is a runtime thing that we must do to
          // preserve semantics
          js.Block(
            seVarDef,
            js.If(
              js.BinaryOp(js.BinaryOp.===, expr, js.Null()),
              genThrowNPE,
              body
            )(jst.NoType)
          )

        case s: jdt.ThrowStatement => js.Throw(genExprValue(s.getExpression))

        case s: jdt.TryStatement => ???

        case s: jdt.TypeDeclarationStatement => ???

        case s: jdt.VariableDeclarationStatement =>
          val tpe = sjsType(s.getType.resolveBinding)

          js.Block(
            s.fragments.toArray.map(_.asInstanceOf[jdt.VariableDeclarationFragment])
              .map { frag =>
                js.VarDef(
                  js.LocalIdent(jsn.LocalName(frag.getName.getIdentifier)),
                  NoOriginalName,
                  tpe,
                  mutable = !jdt.Modifier.isFinal(s.getModifiers),
                  cast(genExprValue(frag.getInitializer, Some(tpe)), tpe)
                )
              }
              .toList
          )

        case s: jdt.WhileStatement =>
          js.While(genExprValue(s.getExpression), genStatement(s.getBody))
      }
    }

    def genExprStatement(expression: jdt.Expression): js.Tree =
      genExpr(expression, false, None)

    /**
     * Transforms a JDT Expression into SJS IR.
     *
     * @param expression The JDT expression.
     * @param tpe        An Option containing a hint of this expression's type. Sometimes, for example with number literals, a different AST node should be generated depending on the type of the variable to which the expression is being assigned.
     * @return
     */
    def genExprValue(expression: jdt.Expression, tpe: Option[jst.Type] = None): js.Tree =
      genExpr(expression, true, tpe)

    def genExpr(expression: jdt.Expression, returningValue: Boolean, tpe: Option[jst.Type] = None): js.Tree = {
      implicit val position: Position = getPosition(expression)

      def recurse(expr: jdt.Expression) =
        genExpr(expr, false, tpe)

      // TODO: Constant expressions may be optimized out into literals
      // See https://docs.oracle.com/javase/specs/jls/se12/html/jls-15.html#jls-15.28

      expression match {
        case e: jdt.Annotation => ???

        case e: jdt.ArrayAccess => js.ArraySelect(
          genExprValue(e.getArray),
          genExprValue(e.getIndex)
        )(sjsType(e.resolveTypeBinding.getElementType))

        case e: jdt.ArrayCreation =>
          if (!e.dimensions.isEmpty) {
            js.NewArray(
              sjsArrayTypeRef(e.resolveTypeBinding),
              e.dimensions.toArray
                .map(_.asInstanceOf[jdt.Expression])
                .map(genExprValue(_))
                .toList
            )
          }
          else if (e.getInitializer != null) {
            genExprValue(e.getInitializer)
          }
          else {
            throw new IllegalArgumentException("Java array must have dimensions or an initializer")
          }

        case e: jdt.ArrayInitializer =>
          val arrayType = sjsType(e.resolveTypeBinding).asInstanceOf[jst.ArrayType]
          val baseType = sjsType(e.resolveTypeBinding.getElementType)
          js.ArrayValue(
            sjsTypeRef(arrayType).asInstanceOf[jst.ArrayTypeRef],
            e.expressions.asScala[jdt.Expression]
              .map(x => cast(genExprValue(x), baseType))
          )

        case e: jdt.Assignment =>
          // TODO: Support arithmetic assignment operators
          val lhs = genExprValue(e.getLeftHandSide)
          lhs match {
            case sel: js.ApplyStatic =>
              // If the LHS is a (qualified) name of a static field, genExpr
              // will return a static call to its getter instead.
              // Static field assignments are also performed through a setter.
              genStaticSet(
                sel.method.name.simpleName.nameString,
                sel.className,
                genExprValue(e.getRightHandSide),
                sjsTypeRef(sel.tpe)
              )
            case _ => js.Assign(lhs, genExprValue(e.getRightHandSide))
          }

        case e: jdt.BooleanLiteral => js.BooleanLiteral(e.booleanValue())

        case e: jdt.CastExpression => js.AsInstanceOf(
          genExprValue(e.getExpression),
          sjsType(e.getType.resolveBinding)
        )

        case e: jdt.CharacterLiteral => js.CharLiteral(e.charValue())

        case e: jdt.ClassInstanceCreation =>
          // TODO: Support anonymous class construction
          val mb = e.resolveConstructorBinding
          val className = jsn.ClassName(e.getType.resolveBinding.getErasure.getQualifiedName)
          val paramTypes = mb.getParameterTypes.map(sjsType)
          val args = e.arguments.asScala[jdt.Expression]
            .map(genExprValue(_))
            .zip(paramTypes)
            .map { case (arg, targetType) => cast(arg, targetType) }
          val isHijacked = HijackedClasses.contains(className.nameString)
          val methodIdent = js.MethodIdent(jsn.MethodName(
            if (isHijacked) "new" else "<init>",
            mb.getParameterTypes.map(sjsTypeRef).toList,
            if (isHijacked) sjsTypeRef(mb.getDeclaringClass) else jst.VoidRef
          ))
          val tpe = jst.ClassType(className)

          if (isHijacked) {
            // Hijacked classes (primitive boxes and String) are constructed
            // with the static ::new()
            js.ApplyStatic(js.ApplyFlags.empty, className, methodIdent, args)(tpe)
          }
          else {
            // Regular classes are constructed as normal
            js.New(className, methodIdent, args)
          }


        case e: jdt.ConditionalExpression => js.If(
          genExprValue(e.getExpression),
          genExprValue(e.getThenExpression),
          genExprValue(e.getElseExpression)
        )(sjsType(e.resolveTypeBinding()))

        case e: jdt.CreationReference => ???

        case e: jdt.ExpressionMethodReference => ???

        case e: jdt.FieldAccess =>
          js.Select(
            genExprValue(e.getExpression),
            jsn.ClassName(e.getExpression.resolveTypeBinding.getQualifiedName),
            js.FieldIdent(jsn.FieldName(e.getName.getIdentifier))
          )(sjsType(e.resolveTypeBinding))

        case e: jdt.InfixExpression =>
          // TODO: Fold left extended operands
          if (e.hasExtendedOperands) ???

          val lhs = genExprValue(e.getLeftOperand)
          val rhs = genExprValue(e.getRightOperand)

          if (isStringy(lhs)) {
            // String concatenation
            js.BinaryOp(
              js.BinaryOp.String_+,
              lhs,
              cast(rhs, JDKStringType)
            )
          }
          else {
            // TODO: Shift ops
            // Arithmetic or logical op
            val expectedType = getBinaryOpResultType(lhs.tpe, rhs.tpe)
            genBinaryOperator(lhs, rhs, expectedType, e.getOperator)
          }

        case e: jdt.InstanceofExpression =>
          js.IsInstanceOf(
            genExprValue(e.getLeftOperand),
            sjsType(e.getRightOperand.resolveBinding())
          )

        case e: jdt.LambdaExpression => ???

        case e: jdt.MethodInvocation =>
          val mi = MethodInfo(e)
          val args = e.arguments().asScala[jdt.Expression]
            .map(genExprValue(_))
            .zip(mi.paramTypes)
            .map { case (arg, tpe) => cast(arg, tpe) }

          val apply =
            if (mi.isStatic) {
              js.ApplyStatic(js.ApplyFlags.empty, mi.declaringClassName, mi.ident,
                args)(mi.returnType)
            }
            else {
              // Instance call
              val receiver = Option(e.getExpression) match {
                case None =>
                  // Call to an instance method; implied `this`
                  js.This()(jst.ClassType(mi.declaringClassName))
                case Some(expr) =>
                  // Receiver is specified
                  genExprValue(expr)
              }
              js.Apply(js.ApplyFlags.empty, receiver, mi.ident,
                args)(mi.returnType)
            }

          // Method return type may be parameterized
          // Cast the result of the call to the proper type
          val expectedType = sjsType(e.resolveMethodBinding.getReturnType)
          if (expectedType != mi.returnType) js.AsInstanceOf(apply, expectedType) else apply

        case e: jdt.NullLiteral => js.Null()

        case e: jdt.NumberLiteral =>
          val expectedType = sjsType(e.resolveTypeBinding)
          val value = e.resolveConstantExpressionValue()
          expectedType match {
            case jst.DoubleType => js.DoubleLiteral(e.getToken.toDouble)
            case jst.FloatType => js.FloatLiteral(e.getToken.toFloat)
            case jst.IntType => js.IntLiteral(e.getToken.toInt)
            case jst.LongType => js.LongLiteral(value.asInstanceOf[Long])
            case other =>
              throw new IllegalArgumentException(s"Cannot bind number literal ${e.getToken} to type ${other}")
          }

        case e: jdt.ParenthesizedExpression => genExprValue(e.getExpression)

        case e: jdt.PostfixExpression =>
          val tempVar = js.LocalIdent(jsn.LocalName(TextUtils.freshName("temp")))
          val tpe = sjsType(e.getOperand.resolveTypeBinding())
          val lhs = genExprValue(e.getOperand)

          // TODO: Support postfix for more than ints
          if (tpe != jst.IntType) ???

          val assignment = js.Assign(
            lhs,
            js.BinaryOp(js.BinaryOp.Int_+, lhs, js.IntLiteral(1))
          )

          if (returningValue) {
            js.Block(
              // Store the current value locally
              js.VarDef(
                tempVar,
                NoOriginalName,
                tpe,
                mutable = false,
                lhs
              ),

              // Increment
              assignment,

              // Return the original value
              js.VarRef(tempVar)(tpe)
            )
          }
          else {
            assignment
          }

        case e: jdt.PrefixExpression => ???

        case e: jdt.QualifiedName =>
          val vb = e.resolveBinding().asInstanceOf[jdt.IVariableBinding]
          if (isStatic(vb)) {
            // Static members are accessed via a synthetic getter
            genStaticGet(
              vb.getName,
              jsn.ClassName(vb.getDeclaringClass.getQualifiedName),
              sjsType(vb.getType)
            )
          }
          else {
            // Instance members are plain selects
            js.Select(
              genExprValue(e.getQualifier),
              jsn.ClassName(vb.getDeclaringClass.getQualifiedName),
              js.FieldIdent(jsn.FieldName(e.getName.getIdentifier))
            )(sjsType(vb.getType))
          }

        case e: jdt.SimpleName =>
          implicit val position: Position = getPosition(e)

          val name = e.getFullyQualifiedName
          val tpe = sjsType(e.resolveTypeBinding)
          e.resolveBinding() match {
            case v: jdt.IVariableBinding =>
              if (v.isField && isStatic(v)) {
                // TODO: Is name ever on lhs of assignment?
                genStaticGet(v.getName, thisClassName, sjsType(v.getType))
              }
              else if (v.isField) {
                js.Select(
                  thisNode,
                  jsn.ClassName(v.getDeclaringClass.getQualifiedName),
                  js.FieldIdent(jsn.FieldName(name))
                )(tpe)
              }
              else js.VarRef(
                js.LocalIdent(jsn.LocalName(name))
              )(sjsType(v.getType))
            case p: jdt.IMethodBinding => ???
            case t: jdt.ITypeBinding =>
              ???
          }

        case e: jdt.StringLiteral => js.StringLiteral(e.getLiteralValue)

        case e: jdt.SuperFieldAccess =>
          val fb = e.resolveFieldBinding
          val declaringClass = fb.getDeclaringClass.getQualifiedName
          val declaringClassName = jsn.ClassName(declaringClass)
          js.Select(
            thisNode,
            declaringClassName,
            js.FieldIdent(jsn.FieldName(e.getName.getIdentifier))
          )(sjsType(e.resolveFieldBinding.getType))

        case e: jdt.SuperMethodInvocation =>
          // TODO: Factor out method name and args generation
          val mb = e.resolveMethodBinding

          val paramTypes = mb.getParameterTypes.map(sjsType)
          val ident = js.MethodIdent(jsn.MethodName(
            e.getName.getIdentifier,
            paramTypes.map(sjsTypeRef).toList,
            sjsTypeRef(mb.getReturnType)
          ))
          val args = e.arguments().asScala[jdt.Expression]
            .map(genExprValue(_))
            .zip(paramTypes)
            .map { case (arg, tpe) => cast(arg, tpe) }
          val tpe = sjsType(mb.getReturnType)

          // TODO: ApplyFlags for SuperMethodInvocation
          js.ApplyStatically(
            js.ApplyFlags.empty,
            thisNode,
            jsn.ClassName(mb.getDeclaringClass.getQualifiedName),
            ident,
            args
          )(tpe)

        case e: jdt.SwitchExpression => ???

        case e: jdt.ThisExpression =>
          val qualifier = Option(e.getQualifier)
            .map(_.getFullyQualifiedName)
            .getOrElse(qualifiedThis)
          js.This()(jst.ClassType(jsn.ClassName(qualifier)))

        case e: jdt.TypeLiteral => ???
        case e: jdt.TypeMethodReference => ???
        case e: jdt.VariableDeclarationExpression => ???
      }
    }

    def genBinaryOperator(lhsIn: js.Tree, rhsIn: js.Tree, outType: jst.Type,
                          jdtOp: jdt.InfixExpression.Operator)
                         (implicit pos: Position): js.Tree = {
      import jdt.InfixExpression.Operator._
      import org.scalajs.ir.Trees.BinaryOp._

      val lhs = cast(lhsIn, outType)
      val rhs = cast(rhsIn, if (isShift(jdtOp)) jst.IntType else outType)

      outType match {
        case jst.IntType =>
          val op = jdtOp match {
            case PLUS => Int_+
            case MINUS => Int_-
            case TIMES => Int_*
            case DIVIDE => Int_/
            case REMAINDER => Int_%
            case OR => Int_|
            case AND => Int_&
            case XOR => Int_^
            case LEFT_SHIFT => Int_<<
            case RIGHT_SHIFT_SIGNED => Int_>>
            case RIGHT_SHIFT_UNSIGNED => Int_>>>
            case EQUALS => Int_==
            case NOT_EQUALS => Int_!=
            case LESS => Int_<
            case LESS_EQUALS => Int_<=
            case GREATER => Int_>
            case GREATER_EQUALS => Int_>=
          }
          js.BinaryOp(op, lhs, rhs)

        case jst.LongType =>
          val op = jdtOp match {
            case PLUS => Long_+
            case MINUS => Long_-
            case TIMES => Long_*
            case DIVIDE => Long_/
            case REMAINDER => Long_%
            case OR => Long_|
            case AND => Long_&
            case XOR => Long_^
            case LEFT_SHIFT => Long_<<
            case RIGHT_SHIFT_SIGNED => Long_>>
            case RIGHT_SHIFT_UNSIGNED => Long_>>>
            case EQUALS => Long_==
            case NOT_EQUALS => Long_!=
            case LESS => Long_<
            case LESS_EQUALS => Long_<=
            case GREATER => Long_>
            case GREATER_EQUALS => Long_>=
          }
          js.BinaryOp(op, lhs, rhs)

        case jst.FloatType =>
          def withFloats(op: Int): js.Tree =
            js.BinaryOp(op, lhs, rhs)

          def toDouble(value: js.Tree): js.Tree =
            js.UnaryOp(js.UnaryOp.FloatToDouble, value)

          def withDoubles(op: Int): js.Tree =
            js.BinaryOp(op, toDouble(lhs), toDouble(rhs))

          jdtOp match {
            case PLUS => withFloats(Float_+)
            case MINUS => withFloats(Float_-)
            case TIMES => withFloats(Float_*)
            case DIVIDE => withFloats(Float_/)
            case REMAINDER => withFloats(Float_%)

            case EQUALS => withDoubles(Double_==)
            case NOT_EQUALS => withDoubles(Double_!=)
            case LESS => withDoubles(Double_<)
            case LESS_EQUALS => withDoubles(Double_<=)
            case GREATER => withDoubles(Double_>)
            case GREATER_EQUALS => withDoubles(Double_>=)
          }

        case jst.DoubleType =>
          val op = jdtOp match {
            case PLUS => Double_+
            case MINUS => Double_-
            case TIMES => Double_*
            case DIVIDE => Double_/
            case REMAINDER => Double_%
            case EQUALS => Double_==
            case NOT_EQUALS => Double_!=
            case LESS => Double_<
            case LESS_EQUALS => Double_<=
            case GREATER => Double_>
            case GREATER_EQUALS => Double_>=
          }
          js.BinaryOp(op, lhs, rhs)

        case jst.BooleanType =>
          jdtOp match {
            case AND => js.BinaryOp(Boolean_&, lhs, rhs)
            case OR => js.BinaryOp(Boolean_|, lhs, rhs)
            case EQUALS => js.BinaryOp(Boolean_==, lhs, rhs)
            case NOT_EQUALS | XOR => js.BinaryOp(Boolean_!=, lhs, rhs)
            case CONDITIONAL_AND =>
              js.If(lhs, rhs, js.BooleanLiteral(value = false))(jst.BooleanType)
            case CONDITIONAL_OR =>
              js.If(lhs, js.BooleanLiteral(value = true), rhs)(jst.BooleanType)
          }

        case _ =>
          jdtOp match {
            case EQUALS => js.BinaryOp(===, lhs, rhs)
            case NOT_EQUALS => js.BinaryOp(!==, lhs, rhs)
          }
      }
    }

    // region Static helpers

    /**
     * Generates a static method call to get the value of a static field.
     */
    def genStaticGet(fieldName: String, receiverClassName: jsn.ClassName,
                     tpe: jst.Type)
                    (implicit pos: Position): js.Tree = js.ApplyStatic(
      js.ApplyFlags.empty,
      receiverClassName,
      js.MethodIdent(jsn.MethodName(
        fieldName, Nil, sjsTypeRef(tpe)
      )),
      Nil
    )(tpe)

    /**
     * Generates a static method call to set the value of a static field.
     */
    def genStaticSet(fieldName: String, receiverClassName: jsn.ClassName,
                     value: js.Tree, typeRef: jst.TypeRef)
                    (implicit pos: Position): js.Tree = js.ApplyStatic(
      js.ApplyFlags.empty,
      receiverClassName,
      js.MethodIdent(jsn.MethodName(
        s"${fieldName}_$$eq", List(typeRef), jst.VoidRef
      )),
      List(value)
    )(jst.NoType)

    // endregion

    // region Switch statement processing

    /**
     * Generates a switch statement.
     * Switches as expressions aren't supported yet.
     */
    def genSwitch(expression: jdt.Expression, statements: List[jdt.Statement])
                 (implicit pos: Position): js.Tree = {
      // TODO: If all cases end in break, emit a match node

      // The switch expression will be evaluated once and stored in this var
      val exprIdent = js.LocalIdent(jsn.LocalName(freshName("$sjsirSwitchExpr")))
      val exprType = sjsType(expression.resolveTypeBinding)

      // Flag to fall through to the next case. This is set if a case is entered.
      val fallthroughIdent = js.LocalIdent(jsn.LocalName(freshName("$sjsirFallthrough")))

      // Short-circuit an empty switch: evaluate its expression for side-effects
      // and skip
      if (statements.isEmpty) {
        return js.VarDef(exprIdent, NoOriginalName, exprType, mutable = false,
          genExprValue(expression))
      }

      // At this point, we expect the list of statements to have the format:
      //   (SwitchCase NonSwitchCase*)+
      // This is the only valid Java syntax, but check anyway for any JDT tomfoolery
      if (!statements.head.isInstanceOf[jdt.SwitchCase])
        throw new JavaCompilationException("The body of a non-empty switch must begin with a case or default")

      // A list of CaseBlocks forms a logical representation of this switch
      case class CaseBlock(expr: Seq[js.Tree], terminating: Boolean, body: js.Tree) {
        val isDefault: Boolean = expr.isEmpty
      }

      /**
       * Given a flat list of statements, some of which may be cases, split the
       * statements into a list of CaseBlock objects that describe each case
       * individually.
       */
      def splitCases(stats: List[jdt.Statement]): List[CaseBlock] = stats match {
        case Nil => Nil
        case (caseHeader: jdt.SwitchCase) :: rest =>
          /*
          Collect the statements for this case into caseStats.
          moreCases will contain the remaining statements in the switch
          starting with the next case, or an empty list.
          If several cases appear one after another, all but the last will have
          empty bodies and will fall through to the next.
           */
          val (caseStats, moreCases) = rest.span(!_.isInstanceOf[jdt.SwitchCase])

          val terminating = /*caseHeader.isSwitchLabeledRule ||*/
            containsTerminatingStatement(caseStats)

          val caseBlock = CaseBlock(
            // Multiple expressions are a preview feature; disable for now
            // caseHeader.expressions.asScala[jdt.Expression].map(genExprValue(_)),
            if (caseHeader.isDefault) Nil else Seq(genExprValue(caseHeader.getExpression)),

            terminating,

            // Body
            js.Block({
              // If the case may reach its end, indicate fallthrough
              val setFallthrough = if (terminating) js.Skip()
              else js.Assign(
                js.VarRef(fallthroughIdent)(jst.BooleanType),
                js.BooleanLiteral(value = true)
              )

              // If the case may reach the end but is a -> case (no fallthrough)
              // then add a synthetic break
              // This is disabled for the time being since -> cases are a
              // preview feature in the most recent JDT.
              val endBreak = js.Skip() /*if (!terminating && caseHeader.isSwitchLabeledRule)
                js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName(breakScope.get)))
              else js.Skip()*/

              setFallthrough :: caseStats.map(genStatement) ::: List(endBreak)
            })
          )

          // Recurse
          caseBlock :: splitCases(moreCases)

        case _ =>
          throw new JavaCompilationException("Could not parse switch body")
      }

      // Emit IR
      // TODO: Emit a Match node if all cases in a switch terminate
      //   You'll need to remove breaks if they are the last stat in a case.
      withBreakScope(jst.NoType) {
        val cases = splitCases(statements)
        js.Block(
          // Expression local
          js.VarDef(exprIdent, NoOriginalName, exprType,
            mutable = false, genExprValue(expression)),

          // Fallthrough flag
          js.VarDef(fallthroughIdent, NoOriginalName, jst.BooleanType,
            mutable = true, js.BooleanLiteral(value = false)),

          cases.foldRight[js.Tree](js.Skip()) { (caseBlock, acc) =>
            js.If(
              js.BinaryOp(
                js.BinaryOp.Boolean_|,
                js.VarRef(fallthroughIdent)(jst.BooleanType),
                js.BooleanLiteral(value = true) /* TODO: Create equality */
              ),
              caseBlock.body,
              acc
            )(jst.NoType)
          }
        )
      }
    }

    // endregion

    // region Helper methods

    def getPosition(node: jdt.ASTNode) = Position(
      Position.SourceFile("TODO"),
      compilationUnit.getLineNumber(node.getStartPosition),
      compilationUnit.getColumnNumber(node.getStartPosition)
    )

    def nameString(f: js.FieldDef): String = f.name.name.nameString

    // endregion

    // TODO: Handle imports
    compilationUnit.types.asScala[jdt.AbstractTypeDeclaration]
      .flatMap {
        case anno: jdt.AnnotationTypeDeclaration => ???
        case enum: jdt.EnumDeclaration => ???
        case intf: jdt.TypeDeclaration if intf.isInterface => ???
        case cls: jdt.TypeDeclaration => genClass(cls)
      }
  }
}
