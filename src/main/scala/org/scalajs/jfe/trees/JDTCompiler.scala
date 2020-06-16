package org.scalajs.jfe.trees

import org.scalajs.jfe.util.TypeUtils._
import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.Trees.OptimizerHints
import org.scalajs.ir.{ClassKind, OriginalName, Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.JavaCompilationException
import org.scalajs.jfe.trees.TreesSupport._
import org.scalajs.jfe.util.TextUtils

import scala.collection.mutable

object JDTCompiler {

  import TreeHelpers._

  private[jfe] val NoOriginalName = OriginalName.NoOriginalName
  private[jfe] val NoOptimizerHints = OptimizerHints.empty
  private[jfe] val NoPosition = Position.NoPosition
  private[jfe] val ConstructorApplyFlags = js.ApplyFlags.empty.withConstructor(true)
  private[jfe] val ConstructorMemberFlags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor)
  private[jfe] val OuterFieldIdent = js.FieldIdent(jsn.FieldName("$sjsirOuter"))(NoPosition)
  private[jfe] val OuterLocalIdent = js.LocalIdent(jsn.LocalName("$sjsirOuter"))(NoPosition)

  def apply(compilationUnit: jdt.CompilationUnit): List[js.ClassDef] =
    compilationUnit.types().asScala[jdt.AbstractTypeDeclaration]
      .flatMap { t => new JDTCompiler(compilationUnit, Compileable(t)).gen() }

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
    jsn.MethodName("$sjsirStaticInitializer", Nil,
      jst.VoidRef)
  )(NoPosition)

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

  def genCaptureName(name: String): String =
    s"$$sjsirCapture__$name"

  def genStaticSetterName(name: String): String =
    s"${name}_$$eq"
}

object Compileable {
  def apply(ast: jdt.AbstractTypeDeclaration): Compileable =
    Compileable(ast, ast.resolveBinding)

  def apply(ast: jdt.AnonymousClassDeclaration): Compileable =
    Compileable(ast, ast.resolveBinding)
}

case class Compileable(ast: jdt.ASTNode, binding: jdt.ITypeBinding)

/**
 * The JDTCompiler class compiles a single JDT type definition into SJSIR code.
 */
private class JDTCompiler(compilationUnit: jdt.CompilationUnit,
                          topLevel: Compileable) {

  import JDTCompiler._
  import TreeHelpers._
  import org.scalajs.jfe.util.TextUtils.freshName

  // region State stuff

  val thisClassName: jsn.ClassName = jsn.ClassName(topLevel.binding.getBinaryName)
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

  def withBreakScope(tpe: jst.Type, name: Option[String] = None)(body: => js.Tree)
                    (implicit pos: Position): js.Labeled = {
    val labelName = name.map("_namedbreak_" + _)
      .getOrElse(freshName("_break"))
    breakScope.withValue(labelName) { label =>
      js.Labeled(js.LabelIdent(jsn.LabelName(label)), tpe, body)
    }
  }

  def withContinueScope(tpe: jst.Type, name: Option[String] = None)(body: => js.Tree)
                       (implicit pos: Position): js.Labeled = {
    val labelName = name.map("_namedcontinue_" + _)
      .getOrElse(freshName("continue"))
    continueScope.withValue(labelName) { label =>
      js.Labeled(js.LabelIdent(jsn.LabelName(label)), tpe, body)
    }
  }

  private var outerClassNames: List[jsn.ClassName] = List()
  private val isNested: Boolean = topLevel.binding.isNested
  private lazy val enclosingName = outerClassNames.head // TODO: Refactor to Option
  private lazy val enclosingType = jst.ClassType(enclosingName)
  private var hasStatics = false
  private val recInnerTypes = mutable.Set[js.ClassDef]()
  private val captureVBs = mutable.ListBuffer[jdt.IVariableBinding]()

  // endregion

  /**
   * Generates the SJSIR tree. This is the main entry point for clients.
   */
  def gen(): List[js.ClassDef] = {
    topLevel.ast match {
      case _: jdt.AnnotationTypeDeclaration =>
        println("Annotation declaration is a no-op")
        Nil
      case _: jdt.EnumDeclaration =>
        throw new NotImplementedError("Compiling enums is not supported yet")
      case intf: jdt.TypeDeclaration if intf.isInterface =>
        throw new NotImplementedError("Compiling interfaces is not supported yet")
      case cls: jdt.TypeDeclaration => genClass(cls)
      case anon: jdt.AnonymousClassDeclaration => genAnonymousClass2(anon)
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

  private def genClass(cls: jdt.TypeDeclaration): List[js.ClassDef] = {
    implicit val position: Position = getPosition(cls)

    val superClassName = jsn.ClassName(
      Option(cls.getSuperclassType)
        .map(_.resolveBinding.getBinaryName)
        .getOrElse(JDKObjectType.className.nameString)
    )

    // TODO: Generate empty constructor in the AST before handling constructors

    // Partition members
    // Attention! The getFields(), getMethods(), and getTypes() methods are just
    // filters over bodyDeclarations().
    val jdtConstructors = cls.getMethods.filter(_.isConstructor).toList

    // Preprocess members to make internal counters consistent
    val sjsMethods = cls.getMethods.filter(!_.isConstructor).map(genMethod)
    val instanceFieldInfos = cls.getFields
      .filter(f => !jdt.Modifier.isStatic(f.getModifiers))
      .flatMap(genFieldInfo)
      .toList

    // Generate static fields, accessors, and initializers
    val staticSupport = genStaticSupport(cls)
    this.hasStatics = staticSupport.nonEmpty

    // Generate constructors, or make the default one
    // Note: constructor generation must follow member generation so that
    // captures can be identified in one pass!
    val instanceInitializers = instanceFieldInfos
      .flatMap(_.initializer(thisClassName))
    val ctors =
      if (jdtConstructors.isEmpty)
        List(genDefaultConstructor(instanceInitializers))
      else
        jdtConstructors.map { method =>
          //          genConstructor(_, superClassName, instanceInitializers)
          genExplicitConstructor(method, instanceInitializers)
        }

    val outerFieldSynthetic =
      if (isNested)
        Some(js.FieldDef(js.MemberFlags.empty, OuterFieldIdent, NoOriginalName,
          jst.ClassType(outerClassNames.head)))
      else
        None

    val body: Seq[js.MemberDef] =
      Nil ++ // Guard, for convenient formatting
        outerFieldSynthetic ++
        instanceFieldInfos.map(_.fieldDef) ++
        staticSupport ++
        ctors ++
        sjsMethods

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
    val innerTypes = cls.getTypes.toList
      .flatMap { t =>
        val compiler = new JDTCompiler(compilationUnit, Compileable(t))
        compiler.outerClassNames = thisClassName :: outerClassNames
        compiler.gen()
      }

    classDef :: innerTypes ::: recInnerTypes.toList
  }

  private def genAnonymousClass2(ast: jdt.AnonymousClassDeclaration): List[js.ClassDef] = {
    implicit val pos: Position = getPosition(ast)

    val binding = ast.resolveBinding
    val superClassName = jsn.ClassName(binding.getSuperclass.getBinaryName)

    val decls = ast.bodyDeclarations.asScala[jdt.BodyDeclaration]
    val fields = decls.collect { case x: jdt.FieldDeclaration => x }
      .flatMap(genFieldInfo)
    val methods = decls.collect { case x: jdt.MethodDeclaration => x }
      .map(genMethod)
    val outerFieldSynthetic = js.FieldDef(js.MemberFlags.empty, OuterFieldIdent,
      NoOriginalName, jst.ClassType(enclosingName))

    // Capture processing
    val captureFields = captureVBs.toList.map { capture =>
      js.FieldDef(
        js.MemberFlags.empty,
        js.FieldIdent(jsn.FieldName(genCaptureName(capture.getName))),
        OriginalName(capture.getName),
        sjsType(capture.getType)
      )
    }
    val captureAssigns = captureFields.map { field =>
      js.Assign(
        js.Select(thisNode, thisClassName, field.name)(field.ftpe),
        js.VarRef(
          js.LocalIdent(jsn.LocalName(field.name.name.nameString))
        )(field.ftpe)
      )
    }

    // Generate default constructor
    // This must come after field/method processing to ensure that all captures
    // are found
    val cInfo = ConstructorCallInfo(binding, Nil, captureVBs.toList)
    val ctor = genConstructor(cInfo, None, Nil, captureAssigns)

    val body =
      outerFieldSynthetic ::
        captureFields :::
        fields.map(_.fieldDef) :::
        List(ctor) :::
        methods

    val classDef = js.ClassDef(
      js.ClassIdent(thisClassName),
      NoOriginalName,
      ClassKind.Class,
      None,
      Some(js.ClassIdent(superClassName)),
      binding.getInterfaces.toList.map { iface =>
        js.ClassIdent(jsn.ClassName(iface.getBinaryName))
      },
      None,
      None,
      body,
      Nil
    )(NoOptimizerHints)

    val innerTypes = decls.collect { case x: jdt.AbstractTypeDeclaration => x }
      .flatMap { t =>
        val compiler = new JDTCompiler(compilationUnit, Compileable(t))
        compiler.outerClassNames = thisClassName :: outerClassNames
        compiler.gen()
      }

    classDef :: innerTypes ::: recInnerTypes.toList
  }

  def genFieldInfo(field: jdt.FieldDeclaration): List[FieldInfo] = {
    val tpe = sjsType(field.getType)
    val static = jdt.Modifier.isStatic(field.getModifiers)
    val pvt = jdt.Modifier.isPrivate(field.getModifiers)
    field.fragments.asScala[jdt.VariableDeclarationFragment].map { frag =>
      implicit val pos: Position = getPosition(frag)

      FieldInfo(
        frag.getName.getIdentifier,
        tpe,
        Option(frag.getInitializer).map { init =>
          cast(genExprValue(init), tpe)
        },
        static, isMutable(frag.resolveBinding), pvt
      )
    }
  }

  // region Statics

  /**
   * Generates field definitions and supporting synthetics for static fields
   * and initializers.
   *
   * This one, big method encapsulates all logic for treating static field s
   * and initializers.
   */
  def genStaticSupport(cls: jdt.AbstractTypeDeclaration): List[js.MemberDef] = {
    val inits = mutable.ListBuffer[js.Tree]()

    /**
     * Given a declaration for one variable, generates a FieldDef along with
     * synthetic getters and setters.
     */
    def genField(info: FieldInfo): List[js.MemberDef] = {
      implicit val pos: Position = info.pos

      val fieldDef = info.fieldDef

      val typeRef = sjsTypeRef(info.tpe)
      val callInit = js.ApplyStatic(js.ApplyFlags.empty, thisClassName,
        staticInitializerIdent, Nil)(jst.NoType)
      val accessorFlags = js.MemberFlags.empty
        .withNamespace(fieldDef.flags.namespace)

      // The getter
      // TODO: Inline the getter?
      val getterIdent = js.MethodIdent(jsn.MethodName(info.name, Nil, typeRef))
      val getter = js.MethodDef(
        accessorFlags, getterIdent, NoOriginalName, Nil, info.tpe,
        Some(js.Block(
          callInit,
          js.SelectStatic(thisClassName, info.ident)(info.tpe)
        ))
      )(NoOptimizerHints, None)

      // The setter
      // TODO: Inline the setter?
      val setterName = genStaticSetterName(info.name)
      val setterIdent = js.MethodIdent(jsn.MethodName(setterName, List(typeRef),
        jst.VoidRef))
      val paramIdent = js.LocalIdent(jsn.LocalName("value"))
      val param = js.ParamDef(paramIdent, NoOriginalName, info.tpe,
        mutable = false, rest = false)
      val setter = js.MethodDef(
        accessorFlags, setterIdent, NoOriginalName, List(param), jst.NoType,
        Some(js.Block(
          callInit,
          js.Assign(
            js.SelectStatic(thisClassName, info.ident)(info.tpe),
            js.VarRef(paramIdent)(info.tpe)
          )
        ))
      )(NoOptimizerHints, None)

      // The initializer, which is put in a synthetic method later
      inits ++= info.initializer(thisClassName).toList

      List(fieldDef, getter, setter)
    }

    /**
     * Generates a synthetic method that statically initializes the class.
     *
     * @param inits List of statements to run during initialization, including
     *              assignments to static fields.
     */
    def genStaticInitializer(inits: List[js.Tree]): List[js.MemberDef] = {
      // TODO: private static initializer?
      implicit val pos: Position = NoPosition
      val initializedFlag = js.FieldDef(
        js.MemberFlags.empty.withMutable(true)
          .withNamespace(js.MemberNamespace.PublicStatic),
        js.FieldIdent(jsn.FieldName("$sjsirStaticCalled")),
        NoOriginalName,
        jst.BooleanType
      )
      val initializer = js.MethodDef(
        js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic),
        staticInitializerIdent, NoOriginalName, Nil, jst.NoType,
        Some(
          js.If(
            js.UnaryOp( // !$sjsirStaticCalled
              js.UnaryOp.Boolean_!,
              js.SelectStatic(thisClassName, initializedFlag.name)(jst.BooleanType),
            ),
            js.Block( // Then
              js.Assign(
                js.SelectStatic(thisClassName, initializedFlag.name)(jst.BooleanType),
                js.BooleanLiteral(value = true)
              ),
              js.Block(inits.toList)
            ),
            js.Skip() // Else
          )(jst.NoType)
        )
      )(NoOptimizerHints, None)(NoPosition)

      List(initializedFlag, initializer)
    }

    /*
    We need to go through all the body declarations in order instead of treating
    fields and initializers separately, because field initializers and static
    initializers are evaluated in lexical order.
     */
    val staticFieldsOrInitializers = cls.bodyDeclarations.asScala[jdt.BodyDeclaration]
      .filter(x => jdt.Modifier.isStatic(x.getModifiers))
      .collect {
        case x: jdt.FieldDeclaration => x
        case x: jdt.Initializer => x
      }

    // No static fields or initializers. Do not emit synthetic supports
    if (staticFieldsOrInitializers.isEmpty) return Nil

    // Generate static fields and their accessors
    val members = staticFieldsOrInitializers.flatMap {
      case field: jdt.FieldDeclaration =>
        genFieldInfo(field).flatMap(genField)
      case init: jdt.Initializer =>
        inits += genBlock(init.getBody)
        Nil
    }

    members ::: genStaticInitializer(inits.toList)
  }

  def genStaticInitCall()(implicit pos: Position): js.Tree = {
    if (this.hasStatics) js.ApplyStatic(js.ApplyFlags.empty, thisClassName,
      staticInitializerIdent, Nil)(jst.NoType)
    else js.Skip()
  }

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

  // region Constructors

  def genDefaultSuperConstructorCall(tb: jdt.ITypeBinding)
                                    (implicit pos: Position): js.Tree = {
    val info = CInfo.default(tb.getSuperclass)
    val args = info.enclosingClassName.map(genOuterSelect)
    js.ApplyStatically(
      ConstructorApplyFlags,
      thisNode,
      info.className,
      info.ident,
      args.toList
    )(jst.NoType)
  }

  def genDefaultConstructor(initializers: List[js.Tree]): js.MethodDef = {
    implicit val pos: Position = NoPosition

    val thisBinding = topLevel.binding
    genConstructor(CInfo.default(thisBinding), None, initializers, Nil)
  }

  def genExplicitConstructor(method: jdt.MethodDeclaration,
                             initializers: List[js.Tree]): js.MethodDef = {
    implicit val pos: Position = getPosition(method)

    // Extract the call to a super constructor or co-constructor, if it exists
    val (jdtFirstCall, stats) = method.getBody.statements.asScala[jdt.Statement]
      .span { s =>
        s.isInstanceOf[jdt.SuperConstructorInvocation] ||
          s.isInstanceOf[jdt.ConstructorInvocation]
      }
    val headCall = jdtFirstCall.headOption.map(genStatement)

    // Only initialize instance variables if the constructor explicitly or
    // implicitly calls a super constructor. This guarantees that
    // initializations are run exactly once.
    val shouldInitialize = jdtFirstCall.isEmpty ||
      jdtFirstCall.head.isInstanceOf[jdt.SuperConstructorInvocation]

    genConstructor(
      CInfo.forMethod(method),
      headCall,
      if (shouldInitialize) initializers else Nil,
      stats.map(genStatement)
    )
  }

  def genConstructor(info: ConstructorCallInfo,
                     headCall: Option[js.Tree],
                     initializers: List[js.Tree],
                     body: List[js.Tree]): js.MethodDef = {
    implicit val pos: Position = info.pos

    // TODO: Set captures

    val setEnclosing = info.enclosingClassName match {
      case None => js.Skip()
      case Some(enclosing) => js.Assign(
        js.Select(thisNode, thisClassName, OuterFieldIdent)(jst.ClassType(enclosing)),
        js.VarRef(OuterLocalIdent)(jst.ClassType(enclosing))
      )
    }

    val statements =
      setEnclosing ::
        headCall.getOrElse(genDefaultSuperConstructorCall(info.cls)) ::
        genStaticInitCall() ::
        js.Block(initializers) ::
        body

    js.MethodDef(
      ConstructorMemberFlags,
      info.ident,
      NoOriginalName,
      info.params,
      jst.NoType,
      Some(js.Block(statements))
    )(NoOptimizerHints, None)
  }

  // endregion

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
      js.MemberFlags.empty.withNamespace(methodNamespace(method.getModifiers)),
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
          js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName("_namedbreak_" + label.getIdentifier)))
        case None =>
          // Exit the nearest break scope
          js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName(breakScope.get)))
      }

      case s: jdt.ConstructorInvocation =>
        val mi = MethodInfo(s.resolveConstructorBinding)
        js.ApplyStatically(
          ConstructorApplyFlags,
          thisNode,
          thisClassName,
          mi.ident,
          mi.genArgsForCoConstructor(
            s.arguments.asScala[jdt.Expression].map(genExprValue(_))
          )
        )(jst.NoType)

      case s: jdt.ContinueStatement => Option(s.getLabel) match {
        case Some(label) =>
          js.Return(js.Undefined(), js.LabelIdent(jsn.LabelName("_namedcontinue_" + label.getIdentifier)))
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

      case s: jdt.ExpressionStatement => genExprStatement(s.getExpression)

      case s: jdt.ForStatement =>
        genForLoop(s, None)

      case s: jdt.IfStatement =>
        js.If(
          genExprValue(s.getExpression),
          genStatement(s.getThenStatement),
          Option(s.getElseStatement)
            .map(genStatement)
            .getOrElse(js.Skip())
        )(jst.NoType)

      case s: jdt.LabeledStatement =>
        s.getBody match {
          case forStat: jdt.ForStatement =>
            genForLoop(forStat, Some(s.getLabel.getIdentifier))
          case _ =>
            js.Labeled(
              js.LabelIdent(jsn.LabelName(s.getLabel.getIdentifier)),
              jst.NoType,
              // TODO: Create a break scope?
              genStatement(s.getBody)
            )
        }

      // TODO: Detect whether the body is a loop and construct break and
      //  continue scope automatically


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
          ConstructorApplyFlags,
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
        val tempName = js.LocalIdent(jsn.LocalName(freshName("sideEffect")))
        js.Block(
          js.VarDef(tempName, NoOriginalName, expr.tpe, mutable = false, expr),
          js.If(
            js.BinaryOp(js.BinaryOp.===, js.VarRef(tempName)(expr.tpe), js.Null()),
            genThrowNPE,
            genBlock(s.getBody)
          )(jst.NoType)
        )

      case s: jdt.ThrowStatement =>
        val expr = genExprValue(s.getExpression)
        val tempName = js.LocalIdent(jsn.LocalName(freshName("sideEffect")))
        js.Block(
          js.VarDef(tempName, NoOriginalName, expr.tpe, mutable = false, expr),
          js.If(
            js.BinaryOp(js.BinaryOp.===, js.VarRef(tempName)(expr.tpe), js.Null()),
            genThrowNPE,
            js.Throw(js.VarRef(tempName)(expr.tpe))
          )(jst.NoType)
        )

      case s: jdt.TryStatement => ???

      case s: jdt.TypeDeclarationStatement =>
        val compiler = new JDTCompiler(compilationUnit, Compileable(s.getDeclaration))
        compiler.outerClassNames = thisClassName :: outerClassNames
        recInnerTypes ++= compiler.gen()

        js.Skip()

      case s: jdt.VariableDeclarationStatement =>
        val tpe = sjsType(s.getType.resolveBinding)

        js.Block(
          s.fragments.asScala[jdt.VariableDeclarationFragment]
            .map {
              frag =>
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
        if (e.getAnonymousClassDeclaration != null) {
          val decl = e.getAnonymousClassDeclaration
          val binding = decl.resolveBinding

          val compiler = new JDTCompiler(compilationUnit, Compileable(decl))
          compiler.outerClassNames = thisClassName :: outerClassNames
          recInnerTypes ++= compiler.gen()

          val info = ConstructorCallInfo(
            binding, Nil,
            compiler.captureVBs.toList)
          val enclosingRef =
            if (jdt.Modifier.isStatic(binding.getDeclaringMember.getModifiers))
              js.Null()
            else thisNode
          val args = compiler.captureVBs.toList.map { capture =>
            genVarOrFieldSelect(None, capture)
          } ::: List(enclosingRef)

          return js.New(compiler.thisClassName, info.ident, args)
        }

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
        genVarOrFieldSelect(Some(genExprValue(e.getExpression)),
          e.resolveFieldBinding)

      case e: jdt.InfixExpression =>

        import jdt.InfixExpression.Operator._

        val lhs = genExprValue(e.getLeftOperand)
        val rhs = genExprValue(e.getRightOperand)

        val rights =
          if (e.hasExtendedOperands)
            rhs :: e.extendedOperands.asScala[jdt.Expression]
              .map(genExprValue(_))
          else List(rhs)

        rights.foldLeft(lhs) {
          case (lhs, rhs) =>
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

      case s: jdt.LambdaExpression =>
        // TODO: implement lambdas
        throw new JavaCompilationException("Lambdas aren't supported yet")

      case e: jdt.MethodInvocation =>
        val mi = MethodInfo(e.resolveMethodBinding)
        val args = mi.genArgs(e.arguments.asScala[jdt.Expression].map(genExprValue(_)))
        val flags = js.ApplyFlags.empty.withPrivate(mi.isPrivate)

        val tree =
          if (mi.isStatic) {
            js.ApplyStatic(flags, mi.declaringClassName, mi.ident,
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

            if (mi.isPrivate)
              js.ApplyStatically(flags, receiver, mi.declaringClassName,
                mi.ident, args)(mi.returnType)
            else
              js.Apply(flags.withPrivate(false), receiver, mi.ident,
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
            throw new IllegalArgumentException(s"Cannot bind number literal ${
              e.getToken
            } to type ${
              other
            }")
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
              js.IntLiteral(value = -1),
              unboxed(expr),
              jdt.InfixExpression.Operator.XOR
            )
          //            genBinaryOperator(
          //              genBinaryOperator(
          //                js.IntLiteral(value = 0),
          //                unboxed(expr),
          //                jdt.InfixExpression.Operator.MINUS
          //              ),
          //              js.IntLiteral(value = 1),
          //              jdt.InfixExpression.Operator.MINUS
          //            )
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
        val vb = e.resolveBinding().asInstanceOf[jdt.IVariableBinding]
        if (isStatic(vb)) {
          // Qualifiers of static names are types, which shouldn't be
          // recursively qualified.
          genVarOrFieldSelect(None, e.resolveBinding)
        }
        else {
          genVarOrFieldSelect(Some(genExprValue(e.getQualifier)), vb)
        }

      case e: jdt.SimpleName =>
        genVarOrFieldSelect(None, e.resolveBinding)

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
        // TODO: .getClass() -> GetClass(expr)?
        js.ClassOf(sjsTypeRef(e.getType.resolveBinding))

      case e: jdt.TypeMethodReference => ???

      case e: jdt.VariableDeclarationExpression =>
        // TODO: Consolidate with VariableDeclarationStatement
        val tpe = sjsType(e.getType.resolveBinding)
        js.Block(
          e.fragments.asScala[jdt.VariableDeclarationFragment]
            .map {
              frag =>
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

  // region Loop helpers

  def genForLoop(ast: jdt.ForStatement, labelName: Option[String])
                (implicit pos: Position): js.Tree = {
    js.Block(
      // Initializers
      ast.initializers.asScala[jdt.Expression]
        .map(genExprValue(_)) :+

        // Main loop
        withBreakScope(jst.NoType, labelName) {
          js.While(
            genExprValue(ast.getExpression),
            js.Block(
              withContinueScope(jst.NoType, labelName) {
                genStatement(ast.getBody)
              } +:

                // Updaters
                ast.updaters.asScala[jdt.Expression].map(genExprStatement)
            )
          )
        }
    )
  }

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

  /**
   * Generates an IR tree that returns the enclosing instance of type `name`.
   * Every nested class has a field that contains a handle to the nearest
   * lexically-enclosing instance. By traversing these fields, it is possible
   * to access enclosing instances at any depth.
   *
   * <p>If `name` refers to `this` instance, an IR tree referring to `this` is
   * returned. If `name` is not a lexically-enclosing class, a
   * JavaCompilationException is raised.
   *
   * @param name ClassName of the enclosing instance to retrieve.
   */
  def genOuterSelect(name: jsn.ClassName)
                    (implicit pos: Position): js.Tree = {
    if (name.equals(thisClassName)) {
      thisNode
    }
    else {
      if (!outerClassNames.exists(_.equals(name))) {
        throw new JavaCompilationException(s"Access to outer class $name but it is not in scope")
      }
      val depth = outerClassNames.indexOf(name) + 1
      outerClassNames.take(depth)
        .foldLeft[js.Tree](thisNode) { case (selector, nextEnclosingScopeName) =>
          js.Select(
            selector,
            selector.tpe.asInstanceOf[jst.ClassType].className,
            OuterFieldIdent
          )(jst.ClassType(nextEnclosingScopeName))
        }
    }
  }

  /**
   * Generates an IR tree that selects a field. If the field is parameterized,
   * it is upcasted to the usage type. The usage type is provided in the
   * variable binding.
   *
   * @param qualifier IR tree referring to the qualifier.
   * @param vb        JDT variable binding describing the field.
   */
  def genSelectWithCast(qualifier: js.Tree, vb: jdt.IVariableBinding)
                       (implicit pos: Position): js.Tree = {
    staticCast( // Upcast if generic
      js.Select(
        qualifier,
        jsn.ClassName(vb.getDeclaringClass.getBinaryName),
        js.FieldIdent(jsn.FieldName(vb.getName))
      )(sjsType(vb.getVariableDeclaration.getType)),
      sjsType(vb.getType))
  }

  /**
   * Generates an IR tree referring to a field, variable, or parameter. If the
   * variable is parameterized, it is upcasted to the desired type.
   *
   * @param qualifier IR of an instance on which the variable is declared. If
   *                  `None`, the qualifier should be inferred lexically.
   * @param binding   The JDT varaible binding.
   */
  def genVarOrFieldSelect(qualifier: Option[js.Tree], binding: jdt.IBinding)
                         (implicit pos: Position): js.Tree = {
    binding match {
      case vb: jdt.IVariableBinding =>
        val tpe = sjsType(vb.getType)

        if (vb.isField && isStatic(vb)) {
          // Static field on any class
          // The qualifier doesn't matter, since the declaring class can be
          // retrieved from the variable binding.
          return genStaticGet(vb.getName,
            jsn.ClassName(vb.getDeclaringClass.getBinaryName), tpe)
        }

        qualifier match {
          case None =>
            // Qualifier was not specified.
            if (vb.isField) {
              val declClassName = jsn.ClassName(vb.getDeclaringClass.getBinaryName)
              if (declClassName == thisClassName) {
                // Implicit `this` qualifier
                genSelectWithCast(thisNode, vb)
              }
              else if (outerClassNames.contains(declClassName)) {
                // Field is defined in a lexically enclosing scope
                genSelectWithCast(genOuterSelect(declClassName), vb)
              }
              else {
                // This shouldn't happen
                throw new JavaCompilationException(s"Don't know how to get variable or field ${vb.getName}")
              }
            }
            else {
              // Capture, parameter, or local variable

              def getDeclaringClass(vb: jdt.IVariableBinding): jdt.ITypeBinding = {
                Option(vb.getDeclaringClass)
                  .getOrElse(vb.getDeclaringMethod.getDeclaringClass)
              }

              if (getDeclaringClass(vb).getBinaryName != thisClassName.nameString) {
                // Capture
                this.captureVBs += vb
                staticCast(
                  js.Select(
                    thisNode, thisClassName,
                    js.FieldIdent(jsn.FieldName(genCaptureName(vb.getName)))
                  )(sjsType(vb.getVariableDeclaration.getType)),
                  sjsType(vb.getType)
                )
              }
              else {
                // Parameter or local variable
                staticCast( // Upcast if generic
                  js.VarRef(
                    js.LocalIdent(jsn.LocalName(vb.getName))
                  )(sjsType(vb.getVariableDeclaration.getType)),
                  tpe)
              }
            }

          case Some(qualifier) =>
            if (qualifier.tpe.isInstanceOf[jst.ArrayType] &&
              vb.getName == "length") {
              // Special case: array length
              js.ArrayLength(qualifier)
            }
            else {
              // Instance field
              // TODO: Can qualified names refer to methods or types?
              genSelectWithCast(qualifier, vb)
            }
        }
    }
  }

  def genSetOuter()(implicit pos: Position): js.Tree =
    if (isNested)
      js.Assign(
        js.Select(thisNode, thisClassName, OuterFieldIdent)(enclosingType),
        js.VarRef(OuterLocalIdent)(enclosingType)
      )
    else js.Skip()

  def getPosition(node: jdt.ASTNode): Position = Position(
    Position.SourceFile("TODO"),
    compilationUnit.getLineNumber(node.getStartPosition),
    compilationUnit.getColumnNumber(node.getStartPosition)
  )
}
