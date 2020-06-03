package org.scalajs.jfe.trees

import org.scalajs.jfe.util.TypeUtils._
import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.Trees.OptimizerHints
import org.scalajs.ir.{ClassKind, OriginalName, Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.JavaCompilationException
import org.scalajs.jfe.util.TextUtils

object JDTCompiler {

  import TreeHelpers._

  private[jfe] val NoOriginalName = OriginalName.NoOriginalName
  private[jfe] val NoOptimizerHints = OptimizerHints.empty
  private[jfe] val NoPosition = Position.NoPosition
  private[jfe] val WithConstructorFlags = js.ApplyFlags.empty.withConstructor(true)
  private[jfe] val WithConstructorNS = js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor)
  private[jfe] val OuterFieldIdent = js.FieldIdent(jsn.FieldName("$sjsirOuter"))(NoPosition)
  private[jfe] val OuterLocalIdent = js.LocalIdent(jsn.LocalName("$sjsirOuter"))(NoPosition)

  def apply(compilationUnit: jdt.CompilationUnit): List[js.ClassDef] =
    compilationUnit.types().asScala[jdt.AbstractTypeDeclaration]
      .flatMap { t => new JDTCompiler(compilationUnit, t).gen() }

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

  /**
   * Ident for a synthetic method that initializes all statics.
   */
  val staticInitializerIdent: js.MethodIdent = js.MethodIdent(
    jsn.MethodName(TextUtils.freshName("$sjsirStaticInitializer"), Nil,
      jst.VoidRef)
  )(NoPosition)
}

/**
 * The JDTCompiler class compiles a single JDT type definition into SJSIR code.
 */
private class JDTCompiler(compilationUnit: jdt.CompilationUnit,
                          topLevel: jdt.AbstractTypeDeclaration) {

  import JDTCompiler._
  import TreeHelpers._
  import org.scalajs.jfe.util.TextUtils.freshName

  // region State stuff

  def packageName: Option[String] = {
    if (compilationUnit.getPackage == null) return None
    Some(compilationUnit.getPackage.getName.getFullyQualifiedName)
  }

  val qualifiedThis: String = topLevel.resolveBinding.getBinaryName
  val thisClassName: jsn.ClassName = jsn.ClassName(qualifiedThis)
  val thisClassType: jst.ClassType = jst.ClassType(thisClassName)

  def thisNode(implicit pos: Position): js.This = js.This()(thisClassType)

  // Return scopes are created on every method or function definition.
  // Return statements always exit the nearest return scope.
  val returnScope = new ScopedStack[String]
  val breakScope = new ScopedStack[String]
  val continueScope = new ScopedStack[String]

  def withReturnScope(tpe: jst.Type)(body: => js.Tree)
                     (implicit pos: Position): js.Labeled =
    returnScope.withValue(freshName("_return")) { label =>
      js.Labeled(js.LabelIdent(jsn.LabelName(label)), tpe, body)
    }

  def withBreakScope(tpe: jst.Type)(body: => js.Tree)
                    (implicit pos: Position): js.Labeled =
    breakScope.withValue(freshName("_break")) { label =>
      js.Labeled(js.LabelIdent(jsn.LabelName(label)), tpe, body)
    }

  def withContinueScope(tpe: jst.Type)(body: => js.Tree)
                       (implicit pos: Position): js.Labeled =
    continueScope.withValue(freshName("_continue")) { label =>
      js.Labeled(js.LabelIdent(jsn.LabelName(label)), tpe, body)
    }

  private var outerClassNames: List[jsn.ClassName] = List()
  private val isNested: Boolean = topLevel.resolveBinding.isNested
  private lazy val enclosingName = outerClassNames.head
  private lazy val enclosingType = jst.ClassType(enclosingName)

  // endregion

  /**
   * Generates the SJSIR tree. This is the main entry point for clients.
   */
  def gen(): Seq[js.ClassDef] = {
    topLevel match {
      case _: jdt.AnnotationTypeDeclaration =>
        throw new NotImplementedError("Compiling annotations is not supported yet")
      case _: jdt.EnumDeclaration =>
        throw new NotImplementedError("Compiling enums is not supported yet")
      case intf: jdt.TypeDeclaration if intf.isInterface =>
        throw new NotImplementedError("Compiling interfaces is not supported yet")
      case cls: jdt.TypeDeclaration => genClass(cls)
    }
  }

  /*
  Generation methods are roughly split into 3 categories:
    - Member generation methods, which compile top-level definitions like
      classes, fields, methods, and handle infrastructure like constructors
      and static initializers.
    - Inner generation methods, which compile statements and expressions that
      occur inside members.
    - Helper generation methods, which are factored-out bits of commonly-used
      code and are typically pure. They appear in the JDTCompiler object as
      well.
   */

  // region Member generation methods

  private def genClass(cls: jdt.TypeDeclaration): Seq[js.ClassDef] = {
    implicit val position: Position = getPosition(cls)

    val superClassName = jsn.ClassName(
      Option(cls.getSuperclassType)
        .map(_.resolveBinding.getBinaryName)
        .getOrElse(JDKObjectType.className.nameString)
    )

    // Partition members
    // Attention! The getFields(), getMethods(), and getTypes() methods are just
    // filters over bodyDeclarations().
    // Lists beginning with `jdt` contain JDT AST nodes.
    // Lists beginning with `sjs` contain processed SJSIR nodes.
    // JDT nodes contain some binding information we need later, so that's why
    // they're kept around.
    val (_, jdtInstanceFields) = cls.getFields
      .partition { f => jdt.Modifier.isStatic(f.getModifiers) }
    val jdtConstructors = cls.getMethods.filter(_.isConstructor)

    // Preprocess members to make internal counters consistent
    val sjsFieldDefs = cls.getFields.flatMap(genFields)
    val sjsStaticFieldDefs = sjsFieldDefs.filter(_.flags.namespace.isStatic)
    val sjsMethods = cls.getMethods.filter(!_.isConstructor).map(genMethod)

    // Generate constructors, or make the default one
    val ctors =
      if (jdtConstructors.isEmpty)
        Seq(genDefaultConstructor(superClassName,
          jdtInstanceFields))
      else
        jdtConstructors.map(genConstructor(_, superClassName,
          jdtInstanceFields.toList)).toSeq

    val outerFieldSynthetic =
      if (isNested)
        Some(js.FieldDef(js.MemberFlags.empty, OuterFieldIdent, NoOriginalName,
          jst.ClassType(outerClassNames.head)))
      else
        None

    // Generate static accessors and initializers
    val staticSynthetics: Seq[js.MemberDef] =
      if (!sjsStaticFieldDefs.isEmpty) {
        // Static fields have synthetic getters and setters, so we can hook onto
        // them and statically initialize the class when needed.
        // SJS doesn't do automatic static initialization like Java does.
        val accessors = sjsStaticFieldDefs.flatMap(f => genStaticAccessors(f))

        accessors ++ genStaticInitializer(cls)
      }
      else Nil

    val body: Seq[js.MemberDef] =
      sjsFieldDefs ++
        outerFieldSynthetic ++
        staticSynthetics ++
        sjsMethods ++
        ctors

    // Generate class def
    val classDef = js.ClassDef(
      js.ClassIdent(thisClassName),
      NoOriginalName,
      ClassKind.Class,
      None,
      Some(js.ClassIdent(superClassName)),
      List(),
      None,
      None,
      body.toList,
      List()
    )(NoOptimizerHints)

    // Generate class defs for inner types
    val innerTypes = cls.getTypes
      .flatMap { t =>
        val compiler = new JDTCompiler(compilationUnit, t)
        compiler.outerClassNames = thisClassName :: outerClassNames
        compiler.gen()
      }

    classDef +: innerTypes
  }

  /**
   * Generates a list of assignment nodes for the given JDT field declaration.
   *
   * <p>
   * JDT provides field initializers alongside the declaration, whereas
   * ScalaJS expects fields to be initialized in the constructor. This method
   * is called when generating constructors and static initializers to add
   * field initialization back in.
   * </p>
   *
   * <p>
   * JDT allows a single field declaration node to define multiple variables
   * of the same type, each with their own initializer, so the returned list
   * may be of greater length than the input.
   * </p>
   *
   * @param field JDT field declaration node.
   */
  def genFieldInitializers(field: jdt.FieldDeclaration): Seq[js.Tree] = {
    implicit val pos: Position = getPosition(field)

    def genFragment(frag: jdt.VariableDeclarationFragment) =
      frag.getInitializer match {
        case null =>
          // TODO: Explicitly set zero value for uninitialized field?
          js.Skip()
        case init =>
          val ident = js.FieldIdent(jsn.FieldName(frag.getName.getIdentifier))
          val tpe = sjsType(field.getType.resolveBinding)
          val select =
            if (jdt.Modifier.isStatic(field.getModifiers))
              js.SelectStatic(thisClassName, ident)(tpe)
            else
              js.Select(thisNode, thisClassName, ident)(tpe)

          js.Assign(select, cast(genExprValue(init), tpe))
      }

    field.fragments.asScala[jdt.VariableDeclarationFragment].map(genFragment)
  }

  /*
  If the type is implicit constructable, this method creates a synthetic
  constructor taking no parameters that calls the super constructor and
  initializes all fields.
   */
  /**
   * Generates a default constructor for the class. This method is only called
   * if the class def does not contain any explicit constructors.
   *
   * @param superClassName Name of the super class.
   * @param fieldDecls     List of JDT field declaration nodes.
   */
  def genDefaultConstructor(superClassName: jsn.ClassName, fieldDecls: Seq[jdt.FieldDeclaration])
                           (implicit pos: Position): js.MethodDef = {
    // TODO: Use MethodInfo
    val params = if (isNested) List(sjsTypeRef(enclosingType)) else Nil
    val constructorIdent = js.MethodIdent(jsn.MethodName.constructor(params))
    val paramDefs =
      if (isNested)
        List(js.ParamDef(js.LocalIdent(jsn.LocalName(OuterFieldIdent.name.nameString)),
          NoOriginalName, enclosingType, mutable = false, rest = false))
      else Nil

    val superC = MethodInfo.defaultConstructor(topLevel.resolveBinding.getSuperclass)
    val superConstructorOuterSelect = superC.outerClassName.map(genOuterSelect)
    val superConstructorCall = js.ApplyStatically(
      WithConstructorFlags,
      thisNode,
      superC.declaringClassName,
      superC.ident,
      superConstructorOuterSelect.toList
    )(jst.NoType)

    // TODO: Call static initializer
    val fieldInitializers = fieldDecls.flatMap(genFieldInitializers)
    val body = genSetOuter() :: superConstructorCall :: fieldInitializers.toList

    js.MethodDef(
      WithConstructorNS,
      constructorIdent,
      NoOriginalName,
      paramDefs,
      jst.NoType,
      Some(js.Block(body))
    )(NoOptimizerHints, None)
  }

  /**
   * Generates a constructor method declaration from a JDT constructor
   * declaration.
   *
   * <p>
   * Unlike normal methods, constructors in SJS must do some additional
   * work:
   * <ul>
   * <li>If the constructor calls no other constructors, call the implicit super
   * constructor;
   * <li>If the constructor calls no other constructors on its own class,
   * initialize instance fields. Unlike Scala, Java allows multiple constructor
   * paths, so there is at least one "top-level" constructor that must
   * initialize all the fields.
   * </ul>
   * </p>
   *
   * @param method         JDT method declaration corresponding to the constructor.
   * @param superClassName Super class name.
   * @param fieldDecls     JDT field declaration nodes in this class to be
   *                       initialized.
   */
  def genConstructor(method: jdt.MethodDeclaration,
                     superClassName: jsn.ClassName,
                     fieldDecls: List[jdt.FieldDeclaration]): js.MethodDef = {
    // TODO: Place field initializers in their own method if there are multiple
    //  top-level constructors
    // TODO: Call static initializer

    implicit val position: Position = getPosition(method)

    val mi = MethodInfo(method.resolveBinding)
    val stats = method.getBody.statements.asScala[jdt.Statement]
    val (prelude, body) = stats match {
      case (_: jdt.ConstructorInvocation) :: _ =>
        // This constructor calls a co-constructor; it is not a top-level
        // Do not add any synthetic code
        (genSetOuter :: Nil, stats)

      case (sci: jdt.SuperConstructorInvocation) :: rest =>
        // This constructor explicitly calls a super-constructor
        // Call super first, THEN initialize instance fields.
        (genSetOuter :: genStatement(sci) :: fieldDecls.flatMap(genFieldInitializers), rest)

      case _ =>
        // This constructor does not delegate. Call the implicit super and
        // initialize instance fields.
        val superMI = MethodInfo.defaultConstructor(topLevel.resolveBinding.getSuperclass)
        val superInvocation = js.ApplyStatically(
          WithConstructorFlags,
          thisNode,
          superMI.declaringClassName,
          superMI.ident,
          superMI.genArgsForConstructor(
            mi.outerClassName.map(genOuterSelect), Nil
          )
        )(jst.NoType)

        (genSetOuter :: superInvocation :: fieldDecls.flatMap(genFieldInitializers), stats)
    }

    js.MethodDef(
      WithConstructorNS,
      MethodInfo(method.resolveBinding).ident,
      NoOriginalName,
      mi.genParamDefs(method.parameters),
      jst.NoType,
      Some(js.Block(prelude ::: body.map(genStatement)))
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
          .withMutable(isMutable(frag.resolveBinding)),
        js.FieldIdent(jsn.FieldName(frag.getName.getIdentifier)),
        NoOriginalName,
        sjsType(field.getType.resolveBinding())
      )
    }

    field.fragments.asScala[jdt.VariableDeclarationFragment]
      .map(genFragment)
  }

  def genStaticAccessors(fieldDef: js.FieldDef): Seq[js.MemberDef] = {
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

      val getterIdent = js.MethodIdent(jsn.MethodName(nameString(fieldDef), Nil,
        typeRef))
      val setterIdent = js.MethodIdent(jsn.MethodName(
        s"${nameString(fieldDef)}_$$eq", List(typeRef), jst.VoidRef))

      Seq(
        // Getter
        js.MethodDef(flags, getterIdent, NoOriginalName, Nil, fieldDef.ftpe,
          Some(withInitializerCall(select)))(NoOptimizerHints, None),

        // Setter
        js.MethodDef(flags, setterIdent, NoOriginalName,
          List(js.ParamDef(
            valueIdent, NoOriginalName, fieldDef.ftpe, mutable = false,
            rest = false)
          ),
          jst.NoType,
          Some(withInitializerCall(
            js.Assign(select, js.VarRef(valueIdent)(fieldDef.ftpe))
          ))
        )(NoOptimizerHints, None)
      )
    }
  }

  /**
   * Generates a synthetic method that runs all static initializers and sets
   * static fields.
   *
   * @param cls A JDT class definition.
   */
  def genStaticInitializer(cls: jdt.TypeDeclaration): Seq[js.MemberDef] = {
    implicit val pos: Position = NoPosition

    // This method is unique in that it takes the whole TypeDeclaration as an
    // argument rather than a single AST node. The reason is that it must
    // consider all static fields AND static initializers, in the correct order.
    // We don't want to give this burden to the caller.

    // Initializers are called in the order by which they appear in the
    // code, so we have no choice but to consider both FieldDeclaration
    // and Initializer types at once.
    val statics = cls.bodyDeclarations
      .asScala[jdt.BodyDeclaration]
      .filter {
        case f: jdt.FieldDeclaration
          if jdt.Modifier.isStatic(f.getModifiers) => true
        case i: jdt.Initializer
          if jdt.Modifier.isStatic(i.getModifiers) => true
        case _ => false
      }

    if (statics.isEmpty) return Nil

    val flagIdent = js.FieldIdent(jsn.FieldName(freshName("$sjsirStaticCalled")))
    val flagSelect = js.SelectStatic(thisClassName, flagIdent)(jst.BooleanType)
    val flagTest = js.BinaryOp(js.BinaryOp.Boolean_!=, flagSelect,
      js.BooleanLiteral(value = true))
    val flagSet = js.Assign(flagSelect, js.BooleanLiteral(value = true))
    val memberFlags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)

    val flag = js.FieldDef(memberFlags.withMutable(true),
      flagIdent, NoOriginalName, jst.BooleanType)

    val inits: List[js.Tree] = statics.flatMap {
      case field: jdt.FieldDeclaration => genFieldInitializers(field)
      case init: jdt.Initializer => Seq(genBlock(init.getBody))
    }

    val initializer = js.MethodDef(memberFlags, staticInitializerIdent,
      NoOriginalName, Nil, jst.NoType,
      Some(
        js.If(flagTest, js.Block(flagSet, js.Block(inits)),
          js.Skip())(jst.NoType)
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

    // TODO: Call static initializer when calling a static method

    val mi = MethodInfo(method.resolveBinding)
    val returnType = mi.returnType

    js.MethodDef(
      js.MemberFlags.empty.withNamespace(memberNamespace(method.getModifiers)),
      mi.ident, NoOriginalName, mi.genParamDefs(method.parameters), returnType,
      Some({
        val body = withReturnScope(returnType) {
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

  // endregion

  // region Inner generation methods

  def genBlock(b: jdt.Block): js.Tree =
    js.Block(b.statements.asScala[jdt.Statement].map(genStatement))(getPosition(b))

  def genStatement(statement: jdt.Statement): js.Tree = {
    implicit val position: Position = getPosition(statement)

    statement match {
      case s: jdt.AssertStatement => js.Skip()
      // TODO: If assertions are enabled, check condition and raise AssertionError

      case s: jdt.Block =>
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
          mi.genArgsForCoConstructor(
            s.arguments.asScala[jdt.Expression].map(genExprValue(_))
          )
        )(jst.NoType)

      case s: jdt.ContinueStatement => Option(s.getLabel) match {
        case Some(label) =>
          js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName(label.getIdentifier)))
        case None =>
          // Exit the nearest continue scope
          js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName(continueScope.get)))
      }

      case s: jdt.DoStatement =>
        withBreakScope(jst.NoType) {
          js.DoWhile(
            withContinueScope(jst.NoType) {
              genStatement(s.getBody)
            },
            genExprValue(s.getExpression)
          )
        }

      case _: jdt.EmptyStatement =>
        js.Skip()

      case s: jdt.EnhancedForStatement => ???
      // TODO: Test enhanced for
      // ForIn makes variable typed to any? Need to store and cast?
      //        js.ForIn(
      //          genExprValue(s.getExpression),
      //          js.LocalIdent(jsn.LocalName(s.getParameter.getName.getIdentifier)),
      //          NoOriginalName,
      //          js.Block(
      //            js.VarDef(
      //              js.LocalIdent(jsn.LocalName(s.getParameter.getName.getIdentifier)),
      //              NoOriginalName,
      //              sjsType(s.getParameter.resolveBinding.getType),
      //              mutable = false,
      //
      //            ),
      //            genStatement(s.getBody)
      //          )
      //        )

      case s: jdt.ExpressionStatement => genExprStatement(s.getExpression)

      case s: jdt.ForStatement =>
        // TODO: Create a new scope to hide name collisions
        js.Block(
          // Initializers
          s.initializers.asScala[jdt.Expression]
            .map(genExprValue(_)) :+

            // Main loop
            withBreakScope(jst.NoType) {
              js.While(
                genExprValue(s.getExpression),
                js.Block(
                  withContinueScope(jst.NoType) {
                    genStatement(s.getBody)
                  } +:

                    // Updaters
                    s.updaters.asScala[jdt.Expression].map(genExprStatement)
                )
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
          mi.genArgsForConstructor(
            mi.outerClassName.map(genOuterSelect),
            s.arguments.asScala[jdt.Expression].map(genExprValue(_))
          )
        )(jst.NoType)

      case _: jdt.SwitchCase =>
        throw new JavaCompilationException("Encountered case outside of a switch statement")

      case s: jdt.SwitchStatement =>
        genSwitch(s.getExpression, s.statements.asScala[jdt.Statement])

      case s: jdt.SynchronizedStatement =>
        // Process the side-effect of the expression
        val expr = genExprValue(s.getExpression)
        js.Block(
          js.VarDef(js.LocalIdent(jsn.LocalName(freshName("sideEffect"))),
            NoOriginalName, expr.tpe, mutable = false, expr),
          js.If(
            js.BinaryOp(js.BinaryOp.===, expr, js.Null()),
            genThrowNPE,
            genBlock(s.getBody)
          )(jst.NoType)
        )

      case s: jdt.ThrowStatement =>
        val expr = genExprValue(s.getExpression)
        js.Block(
          js.VarDef(js.LocalIdent(jsn.LocalName(freshName("sideEffect"))),
            NoOriginalName, expr.tpe, mutable = false, expr),
          js.If(
            js.BinaryOp(js.BinaryOp.===, expr, js.Null()),
            genThrowNPE,
            js.Throw(expr)
          )(jst.NoType)
        )

      case _: jdt.TryStatement => ???

      case t: jdt.TypeDeclarationStatement => ???

      case s: jdt.VariableDeclarationStatement =>
        val tpe = sjsType(s.getType.resolveBinding)

        js.Block(
          s.fragments.asScala[jdt.VariableDeclarationFragment]
            .map { frag =>
              js.VarDef(
                js.LocalIdent(jsn.LocalName(frag.getName.getIdentifier)),
                NoOriginalName,
                tpe,
                mutable = !jdt.Modifier.isFinal(s.getModifiers),
                cast(genExprValue(frag.getInitializer, Some(tpe)), tpe)
              )
            }
        )

      case s: jdt.WhileStatement =>
        withBreakScope(jst.NoType) {
          js.While(genExprValue(s.getExpression), withContinueScope(jst.NoType) {
            genStatement(s.getBody)
          })
        }

      case y: jdt.YieldStatement => ???
    }
  }

  def genExprStatement(expression: jdt.Expression): js.Tree =
    genExpr(expression, returningValue = false, None)

  /**
   * Transforms a JDT Expression into SJS IR.
   *
   * @param expression The JDT expression.
   * @param tpe        An Option containing a hint of this expression's type. Sometimes, for example with number literals, a different AST node should be generated depending on the type of the variable to which the expression is being assigned.
   * @return
   */
  def genExprValue(expression: jdt.Expression, tpe: Option[jst.Type] = None): js.Tree =
    genExpr(expression, returningValue = true, tpe)

  def genExpr(expression: jdt.Expression, returningValue: Boolean, tpe: Option[jst.Type] = None): js.Tree = {
    implicit val position: Position = getPosition(expression)

    // TODO: Constant expressions may be optimized out into literals
    // See https://docs.oracle.com/javase/specs/jls/se12/html/jls-15.html#jls-15.28

    expression match {
      case _: jdt.Annotation => js.Skip()

      case e: jdt.ArrayAccess =>
        js.ArraySelect(
          genExprValue(e.getArray),
          genExprValue(e.getIndex)
        )(sjsType(e.resolveTypeBinding))

      case e: jdt.ArrayCreation =>
        if (!e.dimensions.isEmpty) {
          js.NewArray(
            sjsArrayTypeRef(e.resolveTypeBinding),
            e.dimensions.asScala[jdt.Expression]
              .map(genExprValue(_))
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
        import jdt.Assignment.{Operator => A}
        import jdt.InfixExpression.{Operator => I}

        // TODO: Support arithmetic assignment operators
        val lhs = genExprValue(e.getLeftHandSide)

        val opMap = Map(A.BIT_AND_ASSIGN -> I.AND, A.BIT_OR_ASSIGN -> I.OR,
          A.BIT_XOR_ASSIGN -> I.XOR, A.DIVIDE_ASSIGN -> I.DIVIDE,
          A.LEFT_SHIFT_ASSIGN -> I.LEFT_SHIFT, A.MINUS_ASSIGN -> I.MINUS,
          A.PLUS_ASSIGN -> I.PLUS, A.REMAINDER_ASSIGN -> I.REMAINDER,
          A.RIGHT_SHIFT_SIGNED_ASSIGN -> I.RIGHT_SHIFT_SIGNED,
          A.RIGHT_SHIFT_UNSIGNED_ASSIGN -> I.RIGHT_SHIFT_UNSIGNED,
          A.TIMES_ASSIGN -> I.TIMES)

        val rhs = e.getOperator match {
          case A.ASSIGN => genExprValue(e.getRightHandSide)
          case other => genBinaryOperator(lhs, genExprValue(e.getRightHandSide),
            opMap.apply(other))
        }

        val assignment = lhs match {
          case sel: js.ApplyStatic =>
            // If the LHS is a (qualified) name of a static field, genExpr
            // will return a static call to its getter instead.
            // Static field assignments are also performed through a setter.
            genStaticSet(
              sel.method.name.simpleName.nameString,
              sel.className,
              rhs,
              sjsTypeRef(sel.tpe)
            )
          case _ => js.Assign(lhs, rhs)
        }

        if (returningValue) js.Block(assignment, lhs)
        else assignment

      case e: jdt.BooleanLiteral => js.BooleanLiteral(e.booleanValue())

      case e: jdt.CastExpression => js.AsInstanceOf(
        genExprValue(e.getExpression),
        sjsType(e.getType.resolveBinding)
      )

      case e: jdt.CharacterLiteral => js.CharLiteral(e.charValue())

      case e: jdt.ClassInstanceCreation =>
        // TODO: Support anonymous class construction
        val mb = e.resolveConstructorBinding
        val mi = MethodInfo(mb)
        val className = jsn.ClassName(e.getType.resolveBinding.getErasure.getBinaryName)
        val args = mi.genArgsForConstructor(Some(thisNode),
          e.arguments.asScala[jdt.Expression].map(genExprValue(_)))
        val isHijacked = HijackedClasses.contains(className.nameString)
        val methodIdent =
          if (isHijacked)
            js.MethodIdent(jsn.MethodName(
              "new", mi.paramTypes.map(sjsTypeRef), sjsTypeRef(mb.getDeclaringClass)
            ))
          else
            mi.ident
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

      case e: jdt.CreationReference =>
        // TODO: Implement creation references with lambdas
        throw new JavaCompilationException("Creation references aren't supported yet")

      case e: jdt.ExpressionMethodReference =>
        // TODO: Implement method references with lambdas
        throw new JavaCompilationException("Method references aren't supported yet")

      case e: jdt.FieldAccess =>
        // TODO: Outer scoping with Outer.this.field on FieldAccess
        genSelect(genExprValue(e.getExpression), e.resolveFieldBinding)

      case e: jdt.InfixExpression =>
        import jdt.InfixExpression.Operator._

        val lhs = genExprValue(e.getLeftOperand)
        val rhs = genExprValue(e.getRightOperand)

        val rights =
          if (e.hasExtendedOperands)
            rhs :: e.extendedOperands.asScala[jdt.Expression]
              .map(genExprValue(_))
          else List(rhs)

        rights.foldLeft(lhs) { case (lhs, rhs) =>
          if (isStringy(lhs) && e.getOperator == PLUS) {
            // String concatenation
            js.BinaryOp(
              js.BinaryOp.String_+,
              lhs,
              cast(rhs, JDKStringType)
            )
          }
          else {
            // Arithmetic or logical op
            genBinaryOperator(lhs, rhs, e.getOperator)
          }
        }

      case e: jdt.InstanceofExpression =>
        js.IsInstanceOf(
          genExprValue(e.getLeftOperand),
          sjsType(e.getRightOperand.resolveBinding)
        )

      case _: jdt.LambdaExpression =>
        // TODO: implement lambdas
        throw new JavaCompilationException("Lambdas aren't supported yet")

      case e: jdt.MethodInvocation =>
        val mi = MethodInfo(e.resolveMethodBinding)
        val args = mi.genArgs(e.arguments.asScala[jdt.Expression].map(genExprValue(_)))

        val tree =
          if (mi.isStatic) {
            js.ApplyStatic(js.ApplyFlags.empty, mi.declaringClassName, mi.ident,
              args)(mi.returnType)
          }
          else {
            // Instance call
            val receiver = Option(e.getExpression) match {
              case None =>
                if (outerClassNames.contains(mi.declaringClassName)) {
                  // Method is defined in an enclosing type
                  // Select the chain of outers as the receiver
                  genOuterSelect(mi.declaringClassName)
                }
                else {
                  // Implicit this receiver and method is not overridden
                  thisNode
                }
              case Some(expr) =>
                // Receiver is specified
                genExprValue(expr)
            }

            js.Apply(js.ApplyFlags.empty, receiver, mi.ident,
              args)(mi.returnType)
          }

        // Method return type may be erased
        // Cast the result of the call to the proper type
        staticCast(tree, mi.invocationReturnType)

      case _: jdt.NullLiteral => js.Null()

      case e: jdt.NumberLiteral =>
        val expectedType = sjsType(e.resolveTypeBinding)
        val value = e.resolveConstantExpressionValue()

        // Keep the literal type. It shall be casted to the correct type at the
        // usage site.
        val v = e.resolveConstantExpressionValue
        expectedType match {
          case jst.DoubleType => js.DoubleLiteral(v.asInstanceOf[Double])
          case jst.FloatType => js.FloatLiteral(v.asInstanceOf[Float])
          case jst.IntType => js.IntLiteral(v.asInstanceOf[Int])
          case jst.LongType => js.LongLiteral(v.asInstanceOf[Long])
          case other =>
            throw new IllegalArgumentException(s"Cannot bind number literal ${e.getToken} to type ${other}")
        }

      case e: jdt.ParenthesizedExpression => genExprValue(e.getExpression)

      case e: jdt.PostfixExpression =>
        import jdt.PostfixExpression.Operator._
        val expr = genExprValue(e.getOperand)

        val operation = e.getOperator match {
          case INCREMENT =>
            genBinaryOperator(
              genExprValue(e.getOperand),
              js.IntLiteral(value = 1),
              jdt.InfixExpression.Operator.PLUS
            )
          case DECREMENT =>
            genBinaryOperator(
              genExprValue(e.getOperand),
              js.IntLiteral(value = 1),
              jdt.InfixExpression.Operator.MINUS
            )
        }

        val assignment = js.Assign(expr, cast(operation, expr.tpe))

        if (returningValue) {
          val tempVar = js.LocalIdent(jsn.LocalName(TextUtils.freshName("temp")))

          // Store the current value locally, do the operator, return original
          // value
          js.Block(
            js.VarDef(tempVar, NoOriginalName, expr.tpe, mutable = false, expr),
            assignment,
            js.VarRef(tempVar)(expr.tpe)
          )
        }
        else {
          assignment
        }

      case e: jdt.PrefixExpression =>
        import jdt.PrefixExpression.Operator._
        val expr = genExprValue(e.getOperand)

        e.getOperator match {
          case COMPLEMENT =>
            // JLS14 15.5.5: In all cases, ~x equals (-x)-1.
            genBinaryOperator(
              genBinaryOperator(
                js.IntLiteral(value = 0),
                unboxed(expr),
                jdt.InfixExpression.Operator.MINUS
              ),
              js.IntLiteral(value = 1),
              jdt.InfixExpression.Operator.MINUS
            )
          case INCREMENT =>
            val operation = genBinaryOperator(
              unboxed(expr),
              js.IntLiteral(value = 1),
              jdt.InfixExpression.Operator.PLUS
            )
            js.Block(
              js.Assign(expr, cast(operation, expr.tpe)),
              if (returningValue) expr else js.Skip()
            )
          case DECREMENT =>
            val operation = genBinaryOperator(
              unboxed(expr),
              js.IntLiteral(value = 1),
              jdt.InfixExpression.Operator.MINUS
            )
            js.Block(
              js.Assign(expr, cast(operation, expr.tpe)),
              if (returningValue) expr else js.Skip()
            )
          case MINUS =>
            genBinaryOperator(
              js.IntLiteral(value = 0),
              unboxed(expr),
              jdt.InfixExpression.Operator.MINUS
            )
          case NOT => js.UnaryOp(
            js.UnaryOp.Boolean_!, unboxed(expr),
          )
          case PLUS =>
            // Do a plus binary op to reliably convert to the appropriate
            // numeric type
            genBinaryOperator(
              js.IntLiteral(value = 0),
              unboxed(expr),
              jdt.InfixExpression.Operator.PLUS
            )
        }

      case e: jdt.QualifiedName =>
        // TODO: Can qualified names refer to methods or types?
        val vb = e.resolveBinding().asInstanceOf[jdt.IVariableBinding]
        if (isStatic(vb)) {
          // Static members are accessed via a synthetic getter
          genStaticGet(
            vb.getName,
            jsn.ClassName(vb.getDeclaringClass.getBinaryName),
            sjsType(vb.getVariableDeclaration.getType)
          )
        }
        else {
          // Instance members are plain selects
          genSelect(genExprValue(e.getQualifier), vb)
        }

      case e: jdt.SimpleName =>
        implicit val position: Position = getPosition(e)

        val tpe = sjsType(e.resolveTypeBinding)
        e.resolveBinding() match {
          case vb: jdt.IVariableBinding =>
            if (vb.isField && isStatic(vb)) {
              // TODO: outer scope static
              genStaticGet(vb.getName, thisClassName, tpe)
            }
            else if (vb.isField) {
              genSelect(
                genOuterSelect(jsn.ClassName(vb.getDeclaringClass.getBinaryName)),
                vb
              )
            }
            else {
              // Local variable or method parameter
              val tree = js.VarRef(
                js.LocalIdent(jsn.LocalName(vb.getName))
              )(sjsType(vb.getVariableDeclaration.getType))
              staticCast(tree, tpe)
            }
          case mb: jdt.IMethodBinding => ???
          case tb: jdt.ITypeBinding => ???
        }

      case e: jdt.StringLiteral => js.StringLiteral(e.getLiteralValue)

      case e: jdt.SuperFieldAccess =>
        val fb = e.resolveFieldBinding
        val declaringClass = fb.getDeclaringClass.getBinaryName
        val declaringClassName = jsn.ClassName(declaringClass)
        js.Select(
          thisNode,
          declaringClassName,
          js.FieldIdent(jsn.FieldName(e.getName.getIdentifier))
        )(sjsType(e.resolveFieldBinding.getType))

      case e: jdt.SuperMethodInvocation =>
        val mi = MethodInfo(e.resolveMethodBinding)
        val args = mi.genArgs(e.arguments.asScala[jdt.Expression]
          .map(genExprValue(_)))

        // TODO: ApplyFlags for SuperMethodInvocation
        staticCast(
          js.ApplyStatically(js.ApplyFlags.empty, thisNode,
            mi.declaringClassName, mi.ident, args)(mi.returnType),
          mi.invocationReturnType
        )

      case _: jdt.SwitchExpression => ???

      case e: jdt.ThisExpression =>
        if (e.getQualifier == null) {
          js.This()(thisClassType)
        }
        else {
          genOuterSelect(jsn.ClassName(
            e.getQualifier.resolveTypeBinding.getBinaryName
          ))
        }

      case e: jdt.TypeLiteral =>
        js.ClassOf(sjsTypeRef(e.getType.resolveBinding))

      case e: jdt.TypeMethodReference => ???

      case e: jdt.VariableDeclarationExpression =>
        // TODO: Consolidate with VariableDeclarationStatement
        val tpe = sjsType(e.getType.resolveBinding)
        js.Block(
          e.fragments.asScala[jdt.VariableDeclarationFragment]
            .map { frag =>
              js.VarDef(
                js.LocalIdent(jsn.LocalName(frag.getName.getIdentifier)),
                NoOriginalName,
                tpe,
                mutable = !jdt.Modifier.isFinal(e.getModifiers),
                cast(genExprValue(frag.getInitializer, Some(tpe)), tpe)
              )
            }
        )
    }
  }

  def genBinaryOperator(lhsIn: js.Tree, rhsIn: js.Tree,
                        jdtOp: jdt.InfixExpression.Operator)
                       (implicit pos: Position): js.Tree = {
    import jdt.InfixExpression.Operator._
    import org.scalajs.ir.Trees.BinaryOp._

    val outType = getBinaryOpResultType(
      tryUnbox(lhsIn).getOrElse(lhsIn).tpe,
      tryUnbox(rhsIn).getOrElse(rhsIn).tpe
    )
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
      // Construct an if block for every case
      // This needs to be inside the withBreakScope
      val caseIfs = splitCases(statements).map { caseBlock =>
        val caseCondition =
          if (caseBlock.isDefault) js.BooleanLiteral(value = true)
          else
            caseBlock.expr.map { expr =>
              genBinaryOperator(
                js.VarRef(exprIdent)(exprType),
                expr,
                jdt.InfixExpression.Operator.EQUALS
              )
            }.reduce { (acc, equality) =>
              js.BinaryOp(js.BinaryOp.Boolean_|, acc, equality)
            }

        js.If(
          js.BinaryOp(
            js.BinaryOp.Boolean_|,
            js.VarRef(fallthroughIdent)(jst.BooleanType),
            caseCondition
          ),
          caseBlock.body,
          js.Skip()
        )(jst.NoType)
      }

      js.Block(
        // Expression local
        js.VarDef(exprIdent, NoOriginalName, exprType,
          mutable = false, genExprValue(expression)),

        // Fallthrough flag
        js.VarDef(fallthroughIdent, NoOriginalName, jst.BooleanType,
          mutable = true, js.BooleanLiteral(value = false)),

        js.Block(caseIfs)
      )
    }
  }

  // endregion

  def genOuterSelect(name: jsn.ClassName)
                    (implicit pos: Position): js.Tree = {
    if (name.equals(thisClassName)) {
      thisNode
    }
    else {
      if (!outerClassNames.exists(_.equals(name))) {
        throw new JavaCompilationException(s"Access to outer class $name but it is not in scope")
      }
      outerClassNames.takeUntil(!_.equals(name))
        .foldLeft[js.Tree](js.This()(thisClassType)) { case (inner, name) =>
          js.Select(
            inner,
            inner.tpe.asInstanceOf[jst.ClassType].className,
            OuterFieldIdent
          )(jst.ClassType(name))
        }
    }
  }

  def genSelect(qualifier: js.Tree, vb: jdt.IVariableBinding)
               (implicit pos: Position): js.Tree = {
    val tree = js.Select(
      qualifier,
      jsn.ClassName(vb.getDeclaringClass.getBinaryName),
      js.FieldIdent(jsn.FieldName(vb.getName))
    )(sjsType(vb.getVariableDeclaration.getType))
    staticCast(tree, sjsType(vb.getType))
  }

  def genSetOuter()(implicit pos: Position): js.Tree =
    if (isNested)
      js.Assign(
        js.Select(thisNode, thisClassName, OuterFieldIdent)(enclosingType),
        js.VarRef(OuterLocalIdent)(enclosingType)
      )
    else js.Skip()

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

  // region Helper methods

  def getPosition(node: jdt.ASTNode): Position = Position(
    Position.SourceFile("TODO"),
    compilationUnit.getLineNumber(node.getStartPosition),
    compilationUnit.getColumnNumber(node.getStartPosition)
  )

  def nameString(f: js.FieldDef): String = f.name.name.nameString

  // endregion
}
