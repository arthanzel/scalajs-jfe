package org.scalajs.jfe.trees

import org.scalajs.jfe.util.TypeUtils._
import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.Trees.OptimizerHints
import org.scalajs.ir.{ClassKind, OriginalName, Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.TextUtils

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object JDTCompiler {
  private val NoOriginalName = OriginalName.NoOriginalName
  private val NoOptimizerHints = OptimizerHints.empty
  private val WithConstructorFlags = js.ApplyFlags.empty.withConstructor(true)

  def gen(compilationUnit: jdt.CompilationUnit): List[js.ClassDef] = {
    // region State variables

    var qualifiedThis: String = ""
    lazy val qualifiedClassName: jsn.ClassName = jsn.ClassName(qualifiedThis)
    lazy val qualifiedClassType: jst.ClassType = jst.ClassType(qualifiedClassName)

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

    def thisClassType(implicit pos: Position): js.This = js.This()(qualifiedClassType)

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
      val moduleClassName = jsn.ClassName(s"${qualifiedThis}$$")

      // Partition members
      val jdtStaticFields = cls.getFields.filter { f => jdt.Modifier.isStatic(f.getModifiers) }
      val jdtInstanceFields = cls.getFields.diff(jdtStaticFields)
      val jdtStaticMethods = cls.getMethods.filter { m => jdt.Modifier.isStatic(m.getModifiers) }
      val jdtInstanceMethods = cls.getMethods.diff(jdtStaticMethods)

      // Preprocess members to make internal counters consistent
      val sjsStaticFields = jdtStaticFields.flatMap(genFields)
      val sjsInstanceFields = jdtInstanceFields.flatMap(genFields)
      val sjsStaticMethods = jdtStaticMethods.map(genMethod)
      val sjsInstanceMethods = jdtInstanceMethods.map(genMethod)

      // Generate default constructor
      val ctors = if (!jdtInstanceMethods.exists(_.isConstructor)) Seq(
        genDefaultConstructor(jsn.ClassName(superClassName), jdtInstanceFields)
      ) else Nil

      // Generate class def
      val body: Seq[js.MemberDef] = sjsInstanceFields ++
        sjsInstanceMethods ++
        ctors.toList ++
        genStaticMethodStubs(moduleClassName, sjsStaticMethods) ++
        genStaticFieldStubs(moduleClassName, sjsStaticFields)
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

      // Return the module class if there exist statics
      if (jdtStaticFields.isEmpty && jdtStaticMethods.isEmpty) Seq(classDef)
      else {
        val body = sjsStaticFields ++
          sjsStaticMethods ++
          Seq(genStaticConstructor(moduleClassName, jsn.ClassName(superClassName), jdtStaticFields))
        val moduleDef = js.ClassDef(
          js.ClassIdent(moduleClassName), classDef.originalName, ClassKind.ModuleClass, None,
          classDef.superClass, List(), None, None, body.toList, List()
        )(NoOptimizerHints)
        Seq(moduleDef, classDef)
      }
    }

    def genFieldInitializers(fieldDecls: Seq[jdt.FieldDeclaration], thisName: jsn.ClassName)
                            (implicit pos: Position): Seq[js.Assign] = {
      fieldDecls.flatMap { field =>
        field.fragments.toArray
          .map(_.asInstanceOf[jdt.VariableDeclarationFragment])
          .map { frag =>
            val tpe = sjsType(field.getType.resolveBinding)
            js.Assign(
              js.Select(
                js.This()(jst.ClassType(thisName)),
                thisName,
                js.FieldIdent(jsn.FieldName(frag.getName.getIdentifier))
              )(tpe),
              genExprValue(frag.getInitializer, Some(tpe))
            )
          }
      }
    }

    def genDefaultConstructor(superClassName: jsn.ClassName, fieldDecls: Seq[jdt.FieldDeclaration])
                             (implicit pos: Position): js.MethodDef = {
      val constructorIdent = js.MethodIdent(jsn.MethodName("<init>", List(), jst.VoidRef))
      val superConstructorCall = js.ApplyStatically(
        WithConstructorFlags,
        thisClassType,
        superClassName,
        constructorIdent,
        List()
      )(jst.NoType)
      val fieldInitializers = genFieldInitializers(fieldDecls, qualifiedClassName)

      js.MethodDef(
        js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor),
        constructorIdent,
        NoOriginalName,
        List(),
        jst.NoType,
        Some(js.Block(superConstructorCall +: fieldInitializers.toList))
      )(NoOptimizerHints, None)
    }

    def genStaticConstructor(moduleClassName: jsn.ClassName,
                             superClassName: jsn.ClassName,
                             fieldDecls: Seq[jdt.FieldDeclaration])
                            (implicit pos: Position): js.MethodDef = {
      val constructorIdent = js.MethodIdent(jsn.MethodName("<init>", List(), jst.VoidRef))
      val superConstructorCall = js.ApplyStatically(
        WithConstructorFlags,
        thisClassType,
        superClassName,
        constructorIdent,
        List()
      )(jst.NoType)
      val storeModule = js.StoreModule(
        moduleClassName,
        js.This()(jst.ClassType(moduleClassName))
      )
      val fieldInitializers = genFieldInitializers(fieldDecls, moduleClassName)

      js.MethodDef(
        js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor),
        constructorIdent,
        NoOriginalName,
        List(),
        jst.NoType,
        Some(js.Block(superConstructorCall :: storeModule :: fieldInitializers.toList))
      )(NoOptimizerHints, None)
    }

    def genStaticFieldStubs(moduleClassName: jsn.ClassName, defs: Array[js.FieldDef])
                           (implicit pos: Position): Seq[js.MethodDef] = {
      defs.flatMap { f =>
        // TODO: Field public/private

        val getter = js.MethodDef(
          js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic),
          js.MethodIdent(jsn.MethodName(
            f.name.name.nameString,
            Nil,
            sjsTypeRef(f.ftpe)
          )),
          NoOriginalName,
          Nil,
          f.ftpe,
          Some(
            js.Select(
              js.LoadModule(moduleClassName),
              moduleClassName,
              f.name
            )(f.ftpe)
          )
        )(NoOptimizerHints, None)

        // TODO: Static field mutable?
        val param = js.LocalIdent(jsn.LocalName("x"))
        val setter = js.MethodDef(
          js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic),
          js.MethodIdent(jsn.MethodName(
            s"${f.name.name.nameString}_$$eq",
            List(sjsTypeRef(f.ftpe)),
            jst.NothingRef
          )),
          NoOriginalName,
          List(js.ParamDef(
            param,
            NoOriginalName,
            f.ftpe,
            mutable = false,
            rest = false
          )),
          f.ftpe,
          Some(
            js.Assign(
              js.Select(
                js.LoadModule(moduleClassName),
                moduleClassName,
                f.name
              )(f.ftpe),
              js.VarRef(param)(f.ftpe)
            )
          )
        )(NoOptimizerHints, None)

        Seq(getter, setter)
      }
    }

    def genStaticMethodStubs(moduleClassName: jsn.ClassName, defs: Seq[js.MethodDef])
                            (implicit pos: Position): Seq[js.MethodDef] = {
      defs.map { m =>
        js.MethodDef(
          js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic),
          js.MethodIdent(m.methodName),
          NoOriginalName,
          m.args,
          m.resultType,
          Some(
            js.Apply(
              js.ApplyFlags.empty,
              js.LoadModule(moduleClassName),
              js.MethodIdent(m.methodName),
              m.args.map { arg =>
                js.VarRef(arg.name)(arg.ptpe)
              }
            )(m.resultType)
          )
        )(NoOptimizerHints, None)
      }
    }

    def genFields(field: jdt.FieldDeclaration): Seq[js.FieldDef] = {
      implicit val position: Position = pos(field)

      def processFragment(frag: jdt.VariableDeclarationFragment): js.FieldDef = {
        js.FieldDef(
          js.MemberFlags.empty
            .withNamespace(memberNamespace(field.getModifiers))
            .withMutable(!jdt.Modifier.isFinal(field.getModifiers)),
          js.FieldIdent(jsn.FieldName(frag.getName.getIdentifier)),
          NoOriginalName,
          sjsType(field.getType.resolveBinding())
        )
      }

      field.fragments.toArray
        .map(_.asInstanceOf[jdt.VariableDeclarationFragment])
        .map(processFragment)
    }

    def genMethod(method: jdt.MethodDeclaration): js.MethodDef = {
      implicit val position: Position = pos(method)

      val jdtParameters = method.parameters.toArray.map(_.asInstanceOf[jdt.SingleVariableDeclaration]).toList
      val parameterTypeRefs = jdtParameters
        .map(_.getType)
        .map(toSJSTypeRef)
      val resultType = toSJSType(method.getReturnType2)
      val resultTypeRef = toSJSTypeRef(method.getReturnType2)

      val ns = memberNamespace(method.getModifiers)

      js.MethodDef(
        js.MemberFlags.empty,
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
        Some(
          withReturnScope(resultType) {
            genBlock(method.getBody)
          }
        )
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
            s.initializers.asScala.map(_.asInstanceOf[jdt.Expression])
              .map(genExprValue(_))
              .toList :+

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
          val binding = e.resolveMethodBinding
          val static = isStatic(binding)
          // TODO: If the method is static, convert the receiver to the module class name
          val receiver = Option(e.getExpression)
            .map(genExprValue(_))
            .getOrElse(js.This()(sjsType(binding.getDeclaringClass)))

          js.Apply(
            js.ApplyFlags.empty,
            receiver,
            js.MethodIdent(jsn.MethodName(
              e.getName.getIdentifier,
              binding.getParameterTypes.map(sjsTypeRef).toList,
              sjsTypeRef(binding.getReturnType)
            )),
            e.arguments().toArray
              .map(_.asInstanceOf[jdt.Expression])
              .map(genExprValue(_))
              .toList
          )(sjsType(e.resolveTypeBinding))

        case e: jdt.MethodReference => ???

        case e: jdt.Name =>
          implicit val position: Position = pos(e)
          js.VarRef(
            js.LocalIdent(jsn.LocalName(e.getFullyQualifiedName))
          )(sjsType(e.resolveTypeBinding()))

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

        case e: jdt.ThisExpression => thisClassType

        case e: jdt.TypeLiteral => ???
        case e: jdt.TypeMethodReference => ???
        case e: jdt.VariableDeclarationExpression => ???
      }
    }

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
