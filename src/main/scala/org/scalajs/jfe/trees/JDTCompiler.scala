package org.scalajs.jfe.trees

import org.scalajs.jfe.util.TypeUtils._
import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.Trees.OptimizerHints
import org.scalajs.ir.{ClassKind, OriginalName, Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.TextUtils
import org.scalajs.jfe.util.AccessCounted

import scala.reflect.ClassTag

object Helpers {

  // JDT has lots of untyped lists. Here's a helper to cast them to concrete
  // Scala lists.
  implicit class ListHasAsScala(l: java.util.List[_]) {
    def asScala[A: ClassTag]: List[A] = l.toArray.map(_.asInstanceOf[A]).toList
  }

}

object JDTCompiler {
  def apply(compilationUnit: jdt.CompilationUnit): List[js.ClassDef] =
    new JDTCompiler(compilationUnit).gen()
}

// Make the compiler a class so that it stores state information more easily
private class JDTCompiler(compilationUnit: jdt.CompilationUnit) {

  import Helpers._
  import TextUtils.freshName

  private val NoOriginalName = OriginalName.NoOriginalName
  private val NoOptimizerHints = OptimizerHints.empty
  private val NoPosition = Position.NoPosition
  private val WithConstructorFlags = js.ApplyFlags.empty.withConstructor(true)

  def gen(): List[js.ClassDef] = {
    // region State variables

    var qualifiedThis: String = ""
    lazy val thisClassName: jsn.ClassName = jsn.ClassName(qualifiedThis)
    lazy val thisClassType: jst.ClassType = jst.ClassType(thisClassName)
    lazy val staticInitializerIdent = js.MethodIdent(
      jsn.MethodName(TextUtils.freshName("$sjsirStaticInitializer"), Nil, jst.VoidRef)
    )(NoPosition)

    def thisNode(implicit pos: Position): js.This = js.This()(thisClassType)

    // Return scopes are created on every method or function definition.
    // Return statements always exit the nearest return scope.
    val returnScope = new ScopedStack[String]

    def withReturnScope(tpe: jst.Type)
                       (body: => js.Tree)
                       (implicit pos: Position): js.Tree =
      returnScope.withValue(TextUtils.freshName("_return")) { label =>
        js.Labeled(
          js.LabelIdent(jsn.LabelName(label)),
          tpe,
          body
        )
      }

    // endregion

    // Body of the gen method is at the bottom

    // region Generation methods

    def genClass(cls: jdt.TypeDeclaration): Seq[js.ClassDef] = {
      implicit val position: Position = pos(cls)

      qualifiedThis = qualifyName(cls.getName.getIdentifier)
      val superClassName: String = Option(cls.getSuperclassType)
        .map(_.resolveBinding.getQualifiedName)
        .orElse(Some("java.lang.Object"))
        .get

      // Partition members
      val jdtStaticFields = cls.getFields.filter { f => jdt.Modifier.isStatic(f.getModifiers) }
      val jdtInstanceFields = cls.getFields.diff(jdtStaticFields)
      val jdtStaticMethods = cls.getMethods.filter { m => jdt.Modifier.isStatic(m.getModifiers) }
      val jdtInstanceMethods = cls.getMethods.diff(jdtStaticMethods)

      // Preprocess members to make internal counters consistent
      val sjsFields = cls.getFields.flatMap(genFields)
      val sjsMethods = cls.getMethods.map(genMethod)

      // Generate default constructor
      val ctors = if (!jdtInstanceMethods.exists(_.isConstructor)) Seq(
        genDefaultConstructor(jsn.ClassName(superClassName), jdtInstanceFields)
      ) else Nil

      // Generate static accessors and initializers
      val staticSynthetics: Seq[js.MemberDef] = if (!jdtStaticFields.isEmpty) (
        sjsFields.flatMap(f => genStaticAccessors(f, staticInitializerIdent)) ++
          genStaticInitializer(jdtStaticFields, staticInitializerIdent)
        ) else Nil

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
        field.fragments.toArray
          .map(_.asInstanceOf[jdt.VariableDeclarationFragment])
          .map { frag =>
            val ident = js.FieldIdent(jsn.FieldName(frag.getName.getIdentifier))
            val tpe = sjsType(field.getType.resolveBinding)
            val select = if (jdt.Modifier.isStatic(field.getModifiers)) {
              js.SelectStatic(thisClassName, ident)(tpe)
            } else {
              js.Select(thisNode, thisClassName, ident)(tpe)
            }

            js.Assign(
              select,
              genExprValue(frag.getInitializer, Some(tpe))
            )
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
      val constructorIdent = js.MethodIdent(jsn.MethodName("<init>", List(), jst.VoidRef))
      val superConstructorCall = js.ApplyStatically(
        WithConstructorFlags,
        thisNode,
        superClassName,
        constructorIdent,
        List()
      )(jst.NoType)
      val fieldInitializers = genFieldInitializers(fieldDecls, thisClassName)

      js.MethodDef(
        js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor),
        constructorIdent,
        NoOriginalName,
        List(),
        jst.NoType,
        Some(js.Block(superConstructorCall +: fieldInitializers.toList))
      )(NoOptimizerHints, None)
    }

    /**
     * Generates SJS field defs for a JDT field declaration.
     * In JDT, a single field declaration can define several fields.
     */
    def genFields(field: jdt.FieldDeclaration): Seq[js.FieldDef] = {
      implicit val position: Position = pos(field)

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

    def genStaticInitializer(statics: Seq[jdt.FieldDeclaration],
                             staticInitializerIdent: js.MethodIdent): Seq[js.MemberDef] = {
      implicit val pos: Position = NoPosition

      // TODO: Support custom static initializers

      val flagIdent = js.FieldIdent(jsn.FieldName(freshName("$sjsirStaticCalled")))
      val flagSelect = js.SelectStatic(thisClassName, flagIdent)(jst.BooleanType)
      val memberFlags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)

      val flag = js.FieldDef(memberFlags.withMutable(true),
        flagIdent, NoOriginalName, jst.BooleanType)

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
//              js.Assign(flagSelect, js.BooleanLiteral(value = true)),
              js.Block(genFieldInitializers(statics, thisClassName).toList)
            ),
            js.Skip()
          )(jst.NoType)
        )
      )(NoOptimizerHints, None)

      Seq(flag, initializer)
    }

    def genMethod(method: jdt.MethodDeclaration): js.MethodDef = {
      implicit val position: Position = pos(method)

      val jdtParameters = method.parameters.toArray.map(_.asInstanceOf[jdt.SingleVariableDeclaration]).toList
      val parameterTypeRefs = jdtParameters
        .map(_.getType)
        .map(toSJSTypeRef)
      val resultType = toSJSType(method.getReturnType2)
      val resultTypeRef = toSJSTypeRef(method.getReturnType2)

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
            toSJSType(p.getType),
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
      js.Block(b.statements().toArray.map(s => genStatement(s.asInstanceOf[jdt.Statement])).toList)(pos(b))

    def genStatement(statement: jdt.Statement): js.Tree = {
      implicit val position: Position = pos(statement)

      statement match {
        case s: jdt.AssertStatement => ???

        case s: jdt.Block => genBlock(s)

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
        case s: jdt.BreakStatement => ???
        case s: jdt.ConstructorInvocation => ???
        case s: jdt.ContinueStatement => ???

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
            // TODO: JDT uses a lot of untyped lists. Maybe provide an implicit method to convert and type them?
            s.initializers.asScala[jdt.Expression]
              .map(genExprValue(_)) :+

              // Main loop
              js.While(
                genExprValue(s.getExpression),
                js.Block(
                  List(genStatement(s.getBody)) ++

                    // Updaters
                    s.updaters.asScala.map(_.asInstanceOf[jdt.Expression])
                      .map(genExprValue(_))
                      .toList
                )
              )
          )

        case s: jdt.IfStatement =>
          js.If(
            genExprValue(s.getExpression),
            genStatement(s.getThenStatement),
            genStatement(s.getElseStatement)
          )(jst.NoType)

        case s: jdt.LabeledStatement =>
          js.Labeled(
            js.LabelIdent(jsn.LabelName(s.getLabel.getIdentifier)),
            jst.NoType,
            genStatement(s.getBody)
          )

        case s: jdt.ReturnStatement =>
          /*
           Unlike Scala, `return` returns from the nearest function or method.
           Every method, function, or lambda definition must therefore create a
           new return scope around its body.
           */
          js.Return(
            Option(s.getExpression)
              .map(genExprValue(_))
              .getOrElse(js.Undefined()),
            js.LabelIdent(jsn.LabelName(returnScope.get))
          )

        case s: jdt.SuperConstructorInvocation => ???
        case s: jdt.SwitchCase => ???
        case s: jdt.SwitchStatement => ???
        case s: jdt.SynchronizedStatement => ???

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
                  genExprValue(frag.getInitializer, Some(tpe))
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
      implicit val position: Position = pos(expression)

      def recurse(expr: jdt.Expression) =
        genExpr(expr, false, tpe)

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

        case e: jdt.ArrayInitializer => js.ArrayValue(
          sjsArrayTypeRef(e.resolveTypeBinding),
          e.expressions.toArray
            .map(_.asInstanceOf[jdt.Expression])
            .map(genExprValue(_))
            .toList
        )

        case e: jdt.Assignment =>
          js.Assign(
            genExprValue(e.getLeftHandSide),
            genExprValue(e.getRightHandSide)
          )

        case e: jdt.BooleanLiteral => js.BooleanLiteral(e.booleanValue())

        case e: jdt.CastExpression => js.AsInstanceOf(
          genExprValue(e.getExpression),
          toSJSType(e.getType)
        )

        case e: jdt.CharacterLiteral => js.CharLiteral(e.charValue())

        case e: jdt.ClassInstanceCreation => ???

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
          println(e.resolveTypeBinding())
          ???

        case e: jdt.InstanceofExpression =>
          js.IsInstanceOf(genExprValue(e.getLeftOperand), toSJSType(e.getRightOperand))

        case e: jdt.LambdaExpression => ???

        case e: jdt.MethodInvocation =>
          val mb = e.resolveMethodBinding
          val receiver = Option(e.getExpression) match {
            //            case None if isInStaticScope(e) =>
            //              // Call to a static method on this from the module class
            //              js.This()(jst.ClassType(genClassName(qualifiedThis, static = true)))
            //            case None if isStatic(mb) =>
            //              // Call to a static method on this from the instance class
            //              val m = js.LoadModule(genClassName(qualifiedThis, static = true))
            //              m
            case None =>
              // Call to an instance method on this
              js.This()(jst.ClassType(jsn.ClassName(mb.getDeclaringClass.getQualifiedName)))
            case Some(expr) =>
              // Receiver is specified
              genExprValue(expr)
          }

          val ident = js.MethodIdent(jsn.MethodName(
            e.getName.getIdentifier,
            mb.getParameterTypes.map(sjsTypeRef).toList,
            sjsTypeRef(mb.getReturnType)
          ))
          val args = e.arguments().asScala[jdt.Expression]
            .map(genExprValue(_))
          val tpe = sjsType(e.resolveTypeBinding)

          if (isStatic(mb)) {
            js.ApplyStatic(js.ApplyFlags.empty,
              jsn.ClassName(mb.getDeclaringClass.getQualifiedName),
              ident, args)(tpe)
          }
          else {
            js.Apply(js.ApplyFlags.empty, receiver, ident, args)(tpe)
          }

        case e: jdt.MethodReference => ???

        case e: jdt.QualifiedName =>
          val vb = e.resolveBinding().asInstanceOf[jdt.IVariableBinding]
          if (isStatic(vb)) {
            // Static members are accessed via a synthetic getter
            js.ApplyStatic(
              js.ApplyFlags.empty,
              jsn.ClassName(vb.getDeclaringClass.getQualifiedName),
              js.MethodIdent(jsn.MethodName(
                vb.getName,
                Nil,
                sjsTypeRef(vb.getType)
              )),
              Nil
            )(sjsType(vb.getType))
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
          implicit val position: Position = pos(e)

          //          println("=== NAME", e.getFullyQualifiedName, e.isQualifiedName, e.resolveBinding.getClass)
          val name = e.getFullyQualifiedName
          val tpe = sjsType(e.resolveTypeBinding())
          e.resolveBinding() match {
            case v: jdt.IVariableBinding =>
              if (v.isField && isStatic(v)) {
                // TODO: Is name ever on lhs of assignment?
                returnStatic(v.getName, sjsType(v.getType))
              }
              else if (v.isField) {
                js.Select(
                  js.This()(tpe),
                  jsn.ClassName(v.getDeclaringClass.getQualifiedName),
                  js.FieldIdent(jsn.FieldName(name))
                )(tpe)
              }
              else js.VarRef(
                js.LocalIdent(jsn.LocalName(name))
              )(tpe)
            case p: jdt.IMethodBinding => ???
            case t: jdt.ITypeBinding =>
              ???
          }

        case e: jdt.NullLiteral => js.Null()

        case e: jdt.NumberLiteral =>
          val expectedType = tpe.getOrElse(sjsType(e.resolveTypeBinding))
          expectedType match {
            case jst.ByteType => js.ByteLiteral(e.getToken.toByte)
            case jst.DoubleType => js.DoubleLiteral(e.getToken.toDouble)
            case jst.FloatType => js.FloatLiteral(e.getToken.toFloat)
            case jst.IntType => js.IntLiteral(e.getToken.toInt)
            case jst.LongType => js.LongLiteral(e.getToken.toLong)
            case jst.ShortType => js.ShortLiteral(e.getToken.toShort)
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

        case e: jdt.StringLiteral => js.StringLiteral(e.getLiteralValue)

        case e: jdt.SuperFieldAccess => ???
        case e: jdt.SuperMethodInvocation => ???
        case e: jdt.SwitchExpression => ???

        case e: jdt.ThisExpression => thisNode

        case e: jdt.TypeLiteral => ???
        case e: jdt.TypeMethodReference => ???
        case e: jdt.VariableDeclarationExpression => ???
      }
    }

    def returnStatic(name: String, tpe: jst.Type)
                    (implicit pos: Position): js.Tree = js.ApplyStatic(
      js.ApplyFlags.empty,
      thisClassName,
      js.MethodIdent(jsn.MethodName(
        name, Nil, sjsTypeRef(tpe)
      )),
      Nil
    )(tpe)

    // endregion

    // region Helper methods

    def pos(node: jdt.ASTNode) = Position(
      Position.SourceFile("TODO"),
      compilationUnit.getLineNumber(node.getStartPosition),
      compilationUnit.getColumnNumber(node.getStartPosition)
    )

    def qualifyName(name: String): String = {
      Seq(packageName, Some(name)).flatten.mkString(".")
    }

    def packageName: Option[String] = {
      if (compilationUnit.getPackage == null) return None
      Some(compilationUnit.getPackage.getName.getFullyQualifiedName)
    }

    // endregion

    def toSJSType(tpe: jdt.Type): jst.Type = tpe match {
      case t: jdt.PrimitiveType => t.getPrimitiveTypeCode match {
        case jdt.PrimitiveType.BOOLEAN => jst.BooleanType
        case jdt.PrimitiveType.BYTE => jst.ByteType
        case jdt.PrimitiveType.CHAR => jst.CharType
        case jdt.PrimitiveType.DOUBLE => jst.DoubleType
        case jdt.PrimitiveType.FLOAT => jst.FloatType
        case jdt.PrimitiveType.INT => jst.IntType
        case jdt.PrimitiveType.LONG => jst.LongType
        case jdt.PrimitiveType.SHORT => jst.ShortType
        case jdt.PrimitiveType.VOID => jst.NoType
      }
      case t: jdt.ArrayType =>
        jst.ArrayType(jst.ArrayTypeRef(toSJSTypeRef(t.getElementType).asInstanceOf[jst.NonArrayTypeRef], t.getDimensions))
      case t: jdt.SimpleType =>
        jst.ClassType(jsn.ClassName(t.resolveBinding().getQualifiedName))
    }

    def toSJSTypeRef(tpe: jdt.Type): jst.TypeRef = tpe match {
      case t: jdt.PrimitiveType => toSJSType(t).asInstanceOf[jst.PrimTypeWithRef].primRef
      case t: jdt.ArrayType => jst.ArrayTypeRef(
        toSJSTypeRef(t.getElementType).asInstanceOf[jst.NonArrayTypeRef],
        t.getDimensions
      )
      case t: jdt.SimpleType => jst.ClassRef(jsn.ClassName(t.resolveBinding().getQualifiedName))
    }

    def nameString(f: js.FieldDef): String = f.name.name.nameString

    // TODO: Handle imports
    compilationUnit.types
      .toArray
      .flatMap {
        case anno: jdt.AnnotationTypeDeclaration => ???
        case enum: jdt.EnumDeclaration => ???
        case intf: jdt.TypeDeclaration if intf.isInterface => ???
        case cls: jdt.TypeDeclaration => genClass(cls)
      }.toList
  }
}
