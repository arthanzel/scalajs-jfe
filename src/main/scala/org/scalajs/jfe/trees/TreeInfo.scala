package org.scalajs.jfe.trees

import org.eclipse.jdt.core.{dom => jdt}

import org.scalajs.ir.{Names => jsn, Trees => js, Types => jst}
import org.scalajs.ir.Position

import org.scalajs.jfe.util.TypeUtils._

case class MethodInfo private(originalBinding: jdt.IMethodBinding,
                              paramTypes: List[jst.Type],
                              returnType: jst.Type,
                              ident: js.MethodIdent,
                              declaringClassName: jsn.ClassName) {
  def declaringClassType: jst.ClassType = jst.ClassType(declaringClassName)

  def isStatic: Boolean = jdt.Modifier.isStatic(originalBinding.getModifiers)

  def genArgs(args: Seq[js.Tree])
             (implicit pos: Position): List[js.Tree] =
    args.zip(paramTypes)
    .map { case (arg, tpe) => cast(arg, tpe) }
    .toList
}

object MethodInfo {
  def apply(mb: jdt.IMethodBinding)
           (implicit pos: Position): MethodInfo = {
    val decl = mb.getMethodDeclaration
    val paramTypes = decl.getParameterTypes
      .map(t => t.getErasure)
      .map(sjsType)
      .toList
    val returnType = sjsType(decl.getReturnType.getErasure)
    val ident = if (mb.isConstructor)
      js.MethodIdent(jsn.MethodName.constructor(paramTypes.map(sjsTypeRef)))
    else
      js.MethodIdent(jsn.MethodName(
        mb.getName, paramTypes.map(sjsTypeRef), sjsTypeRef(returnType)
      ))
    val declaringClassName = jsn.ClassName(mb.getDeclaringClass.getQualifiedName)

    MethodInfo(mb, paramTypes, returnType, ident, declaringClassName)
  }

  def apply(method: jdt.MethodInvocation)(implicit pos: Position): MethodInfo =
    MethodInfo(method.resolveMethodBinding)
}
