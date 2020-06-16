package org.scalajs.jfe.trees

import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.{Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.trees.JDTCompiler.{NoOriginalName, NoPosition, OuterLocalIdent, genCaptureName}

object TreesSupport {

  import org.scalajs.jfe.trees.TreeHelpers._
  import org.scalajs.jfe.util.TypeUtils._
  import org.scalajs.ir.OriginalName.NoOriginalName

  object FieldInfo

  case class FieldInfo(name: String, tpe: jst.Type, init: Option[js.Tree],
                       static: Boolean, mutable: Boolean, pvt: Boolean)
                      (implicit val pos: Position) {
    lazy val ident: js.FieldIdent = js.FieldIdent(jsn.FieldName(name))

    lazy val fieldDef: js.FieldDef = js.FieldDef(
      js.MemberFlags.empty
        .withMutable(mutable || static) // Statics must be mutable to be initialized
        .withNamespace((static, pvt) match {
          case (false, false) => js.MemberNamespace.Public
          case (false, true) => js.MemberNamespace.Private
          case (true, false) => js.MemberNamespace.PublicStatic
          case (false, false) => js.MemberNamespace.PrivateStatic
        }),
      ident,
      NoOriginalName,
      tpe
    )

    def initializer(className: jsn.ClassName): Option[js.Assign] =
      init.map { v =>
        if (static) js.Assign(js.SelectStatic(className, ident)(tpe), v)
        else js.Assign(
          js.Select(js.This()(jst.ClassType(className)), className, ident)(tpe),
          v
        )
      }
  }


  object CInfo {
    def default(tb: jdt.ITypeBinding): ConstructorCallInfo = {
      ConstructorCallInfo(
        tb,
        Nil,
        Nil, // TODO: Captures
      )(NoPosition)
    }

    def forMethod(method: jdt.MethodDeclaration)
                 (implicit pos: Position): ConstructorCallInfo = {
      val mb = method.resolveBinding
      ConstructorCallInfo(
        mb.getDeclaringClass,
        method.parameters.asScala[jdt.SingleVariableDeclaration].map { p =>
          js.ParamDef(
            js.LocalIdent(jsn.LocalName(p.getName.getIdentifier)),
            NoOriginalName,
            sjsType(p.getType),
            isMutable(p.resolveBinding),
            rest = p.isVarargs
          )
        },
        Nil
      )
    }
  }

  trait ConstructorInfo {
    val cls: jdt.ITypeBinding
    val formalParams: List[js.ParamDef]
    val captures: List[jdt.IVariableBinding]
    implicit val pos: Position

    lazy val className: jsn.ClassName = jsn.ClassName(cls.getBinaryName)
    lazy val enclosingClassName: Option[jsn.ClassName] =
      Option(cls.getDeclaringClass).map {
        enclosing => jsn.ClassName(enclosing.getBinaryName)
      }

    lazy val params: List[js.ParamDef] = {
      val captureParams = captures.map { cap =>
        js.ParamDef(
          js.LocalIdent(jsn.LocalName(genCaptureName(cap.getName))),
          NoOriginalName,
          sjsType(cap.getType),
          mutable = false, rest = false
        )
      }

      val enclosingParams = enclosingClassName.map { name =>
        js.ParamDef(
          OuterLocalIdent,
          NoOriginalName,
          jst.ClassType(name),
          mutable = false, rest = false
        )
      }.toList

      captureParams ::: enclosingParams ::: formalParams
    }

    lazy val ident: js.MethodIdent = js.MethodIdent(
      jsn.MethodName.constructor(params.map(p => sjsTypeRef(p.ptpe)))
    )
  }

  // TODO: Replace usages of MethodInfo with ConstructorCallInfo where
  //  appropriate

  case class ConstructorCallInfo(cls: jdt.ITypeBinding,
                                 formalParams: List[js.ParamDef],
                                 captures: List[jdt.IVariableBinding])
                                (implicit val pos: Position)
    extends ConstructorInfo
}
