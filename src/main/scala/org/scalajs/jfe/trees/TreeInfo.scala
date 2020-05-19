package org.scalajs.jfe.trees

import org.scalajs.jfe.trees.JDTCompiler._
import org.scalajs.jfe.trees.TreeHelpers._

import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.{OriginalName, Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.util.TypeUtils._

object MethodInfo {
  def apply(binding: jdt.IMethodBinding): MethodInfo = new MethodInfo(binding)
}

/**
 * MethodInfo encapsulates several commonly-used bits of data about a method,
 * including namespace, type, and identity info. It bridges the gap between the
 * data structures of JDT and SJS.
 */
class MethodInfo(val binding: jdt.IMethodBinding) {
  private val decl = binding.getMethodDeclaration

  val declaringClassName: jsn.ClassName = jsn.ClassName(binding.getDeclaringClass.getBinaryName)
  val declaringClassType: jst.ClassType = jst.ClassType(declaringClassName)
  val isStatic: Boolean = jdt.Modifier.isStatic(binding.getModifiers)
  val isConstructor: Boolean = binding.isConstructor
  val isNestedConstructor: Boolean = isConstructor && binding.getDeclaringClass.isNested

  val declaredParamTypes = decl.getParameterTypes
    .map(t => t.getErasure)
    .map(sjsType)
    .toList
  val paramTypes: List[jst.Type] = {
    val outerParamType = if (isNestedConstructor)
      List(jst.ClassType(jsn.ClassName(
        binding.getDeclaringClass.getDeclaringClass.getBinaryName
      )))
    else Nil
    outerParamType ::: declaredParamTypes
  }
  val returnType: jst.Type = sjsType(decl.getReturnType.getErasure)
  val invocationReturnType: jst.Type = sjsType(binding.getReturnType)

  def genArgs(args: Seq[js.Tree])
             (implicit pos: Position): List[js.Tree] =
    args.zip(paramTypes)
      .map { case (arg, tpe) => cast(arg, tpe) }
      .toList

  def genArgsForConstructor(thisNode: js.This, args: Seq[js.Tree])
                           (implicit pos: Position): List[js.Tree] = {
    val declaredArgs = genArgs(args)
    if (isNestedConstructor) thisNode :: declaredArgs
    else declaredArgs
  }


  def genParamDefs(decls: List[jdt.SingleVariableDeclaration])
                  (implicit pos: Position): List[js.ParamDef] = {
    // It would be cool if the method binding gave us the parameter names, but
    // alas, we have to ask the client to pass them explicitly
    val declaredParams = declaredParamTypes.zip(decls)
      .map { case (tpe, decl) =>
        js.ParamDef(
          js.LocalIdent(jsn.LocalName(decl.getName.getIdentifier)),
          OriginalName.NoOriginalName,
          tpe,
          mutable = isMutable(decl.resolveBinding),
          rest = decl.isVarargs
        )
      }
    val outerParam =
      if (isNestedConstructor)
        List(js.ParamDef(
          OuterLocalIdent,
          NoOriginalName, paramTypes.head, mutable = false, rest = false
        ))
      else Nil
      outerParam ::: declaredParams
  }

  def genParamDefs(params: java.util.List[_])
                  (implicit pos: Position): List[js.ParamDef] =
    genParamDefs(params.asScala[jdt.SingleVariableDeclaration])

  def ident(implicit pos: Position): js.MethodIdent =
    if (isConstructor)
      js.MethodIdent(jsn.MethodName.constructor(paramTypes.map(sjsTypeRef)))
    else
      js.MethodIdent(jsn.MethodName(
        binding.getName, paramTypes.map(sjsTypeRef), sjsTypeRef(returnType)
      ))
}
