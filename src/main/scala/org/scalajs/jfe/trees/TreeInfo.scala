package org.scalajs.jfe.trees

import org.scalajs.jfe.trees.TreeHelpers._

import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.{OriginalName, Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.util.TypeUtils._

/**
 * MethodInfo encapsulates several commonly-used bits of data about a method,
 * including namespace, type, and identity info. It bridges the gap between the
 * data structures of JDT and SJS.
 */
case class MethodInfo(binding: jdt.IMethodBinding) {
  private val decl = binding.getMethodDeclaration

  val declaringClassName: jsn.ClassName = jsn.ClassName(binding.getDeclaringClass.getQualifiedName)
  val declaringClassType: jst.ClassType = jst.ClassType(declaringClassName)
  val isStatic: Boolean = jdt.Modifier.isStatic(binding.getModifiers)
  val paramTypes: List[jst.Type] = decl.getParameterTypes
    .map(t => t.getErasure)
    .map(sjsType)
    .toList
  val returnType: jst.Type = sjsType(decl.getReturnType.getErasure)
  val invocationReturnType: jst.Type = sjsType(binding.getReturnType)

  def genArgs(args: Seq[js.Tree])
             (implicit pos: Position): List[js.Tree] =
    args.zip(paramTypes)
      .map { case (arg, tpe) => cast(arg, tpe) }
      .toList


  def genParamDefs(params: List[jdt.SingleVariableDeclaration])
                  (implicit pos: Position): List[js.ParamDef] = {
    // It would be cool if the method binding gave us the parameter names, but
    // alas, we have to ask the client to pass them explicitly
    params.map { p =>
      js.ParamDef(
        js.LocalIdent(jsn.LocalName(p.getName.getIdentifier)),
        OriginalName.NoOriginalName,
        sjsType(p.getType.resolveBinding),
        mutable = isMutable(p.resolveBinding),
        rest = p.isVarargs
      )
    }
  }

  def genParamDefs(params: java.util.List[_])
                  (implicit pos: Position): List[js.ParamDef] =
    genParamDefs(params.asScala[jdt.SingleVariableDeclaration])

  def ident(implicit pos: Position): js.MethodIdent =
    if (binding.isConstructor)
      js.MethodIdent(jsn.MethodName.constructor(paramTypes.map(sjsTypeRef)))
    else
      js.MethodIdent(jsn.MethodName(
        binding.getName, paramTypes.map(sjsTypeRef), sjsTypeRef(returnType)
      ))
}
