package org.scalajs.jfe.trees

import org.scalajs.jfe.trees.JDTCompiler._
import org.scalajs.jfe.trees.TreeHelpers._
import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.{Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.util.TypeUtils._

object MethodInfo {
  def apply(mb: jdt.IMethodBinding): MethodInfo = {
    new MethodInfo {
      override def declaringClassName: jsn.ClassName =
        jsn.ClassName(mb.getDeclaringClass.getBinaryName)
      override def isConstructor: Boolean = mb.isConstructor
      override def isStatic: Boolean =
        jdt.Modifier.isStatic(mb.getModifiers)
      override def declaredParamTypes: List[jst.Type] =
        mb.getMethodDeclaration.getParameterTypes.map(sjsType).toList
      override def name: String = mb.getName
      override def returnType: jst.Type =
        sjsType(mb.getMethodDeclaration.getReturnType)
      override def invocationReturnType: jst.Type =
        sjsType(mb.getReturnType)
      override def outerClassName: Option[jsn.ClassName] =
        Option(mb.getDeclaringClass.getDeclaringClass)
          .map(cls => jsn.ClassName(cls.getBinaryName))
    }
  }

  def defaultConstructor(jdtType: jdt.ITypeBinding): MethodInfo =
    new MethodInfo {
      override def declaringClassName: jsn.ClassName =
        jsn.ClassName(jdtType.getBinaryName)
      override def isConstructor = true
      override def isStatic: Boolean = false
      override def declaredParamTypes: List[jst.Type] = Nil
      override def name: String = ""
      override def returnType: jst.Type = jst.NoType
      override def invocationReturnType: jst.Type = jst.NoType
      override def outerClassName: Option[jsn.ClassName] =
        Option(jdtType.getDeclaringClass)
          .map(cls => jsn.ClassName(cls.getBinaryName))
    }
}

/**
 * MethodInfo encapsulates several commonly-used bits of data about a method,
 * including namespace, type, and identity info. It bridges the gap between the
 * data structures of JDT and SJS.
 */
trait MethodInfo {
  /**
   * Qualified ClassName of the class that defines this method.
   */
  def declaringClassName: jsn.ClassName

  def isConstructor: Boolean

  /**
   * Whether this method was declared with the `static` modifier.
   */
  def isStatic: Boolean

  /**
   * List of erased parameter types in this method's formal declaration. The
   * compiler may add some synthetic parameters; these are not included in
   * `declaredParamTypes`.
   */
  def declaredParamTypes: List[jst.Type]

  /**
   * Simple name of the method
   */
  def name: String

  /**
   * The declared and erased return type of this method. If the return type
   * is parameterized, `returnType` gives the erasure.
   */
  def returnType: jst.Type

  /**
   * The return type for this particular invocation of the method. If the
   * return type is parameterized, `invocationReturnType` gives the concrete
   * type that this invocation returns.
   */
  def invocationReturnType: jst.Type

  /**
   * Qualified ClassName of the outer class, or None if this class is top-level.
   */
  def outerClassName: Option[jsn.ClassName]

  // Derived members

  //  val declaringClassType: jst.ClassType = jst.ClassType(declaringClassName)

  /**
   * List of erased parameter types of the method, including any synthetic
   * parameters inserted by the compiler.
   */
  lazy val paramTypes: List[jst.Type] = {
    if (!isConstructor) declaredParamTypes
    else outerClassName match {
      case Some(name) => jst.ClassType(name) :: declaredParamTypes
      case None => declaredParamTypes
    }
  }

  // Methods

  /**
   * Constructs an argument list to invoke this method by casting provided trees
   * to the appropriate parameter types.
   */
  def genArgs(args: List[js.Tree])
             (implicit pos: Position): List[js.Tree] =
    args.zip(declaredParamTypes)
      .map { case (arg, tpe) => cast(arg, tpe) }

  /**
   * Constructs an argument list to invoke a constructor from another
   * constructor on the same class. If the class is an inner class, the
   * reference to the outer class is automatically passed through.
   */
  def genArgsForCoConstructor(args: List[js.Tree])
                             (implicit pos: Position): List[js.Tree] = {
    outerClassName match {
      case Some(name) =>
        js.VarRef(OuterLocalIdent)(jst.ClassType(name)) :: genArgs(args)
      case None => genArgs(args)
    }
  }

  /**
   * Constructs an argument list to invoke a constructor for creating a new
   * instance. If the class is an inner class, a reference to the outer class
   * is automatically inserted.
   */
  def genArgsForConstructor(outerRef: Option[js.Tree], args: List[js.Tree])
                           (implicit pos: Position): List[js.Tree] = {
    outerClassName match {
      case Some(_) => outerRef.toList ::: genArgs(args)
      case None => genArgs(args)
    }
  }

  def ident(implicit pos: Position): js.MethodIdent =
    if (isConstructor)
      js.MethodIdent(jsn.MethodName.constructor(paramTypes.map(sjsTypeRef)))
    else
      js.MethodIdent(jsn.MethodName(
        name, paramTypes.map(sjsTypeRef), sjsTypeRef(returnType)
      ))

  /**
   * Generates parameter definitions for MethodDefs.
   */
  def genParamDefs(decls: List[jdt.SingleVariableDeclaration])
                  (implicit pos: Position): List[js.ParamDef] = {
    // It would be cool if the method binding gave us the parameter names, but
    // alas, we have to ask the client to pass them explicitly
    val declaredParams = declaredParamTypes.zip(decls)
      .map { case (tpe, decl) =>
        js.ParamDef(
          js.LocalIdent(jsn.LocalName(decl.getName.getIdentifier)),
          NoOriginalName,
          tpe,
          mutable = isMutable(decl.resolveBinding),
          rest = decl.isVarargs
        )
      }
    val outerParam =
      if (!isConstructor) Nil
      else outerClassName match {
        case Some(name) => List(js.ParamDef(
          OuterLocalIdent, NoOriginalName, jst.ClassType(name), mutable = false,
          rest = false
        ))
        case None => Nil
      }

    outerParam ::: declaredParams
  }

  def genParamDefs(decls: java.util.List[_])
                  (implicit pos: Position): List[js.ParamDef] =
    genParamDefs(decls.asScala[jdt.SingleVariableDeclaration])
}

//class MethodInfo(val binding: jdt.IMethodBinding) {
//  private val decl = binding.getMethodDeclaration
//
//  val declaringClassName: jsn.ClassName = jsn.ClassName(binding.getDeclaringClass.getBinaryName)
//  val declaringClassType: jst.ClassType = jst.ClassType(declaringClassName)
//  val isStatic: Boolean = jdt.Modifier.isStatic(binding.getModifiers)
//  val isConstructor: Boolean = binding.isConstructor
//  val isNestedConstructor: Boolean = isConstructor && binding.getDeclaringClass.isNested
//
//  val declaredParamTypes = decl.getParameterTypes
//    .map(t => t.getErasure)
//    .map(sjsType)
//    .toList
//  val paramTypes: List[jst.Type] = {
//    val outerParamType = if (isNestedConstructor)
//      List(jst.ClassType(jsn.ClassName(
//        binding.getDeclaringClass.getDeclaringClass.getBinaryName
//      )))
//    else Nil
//    outerParamType ::: declaredParamTypes
//  }
//  val returnType: jst.Type = sjsType(decl.getReturnType.getErasure)
//  val invocationReturnType: jst.Type = sjsType(binding.getReturnType)
//
//  def genArgs(args: Seq[js.Tree])
//             (implicit pos: Position): List[js.Tree] =
//    args.zip(paramTypes)
//      .map { case (arg, tpe) => cast(arg, tpe) }
//      .toList
//
//  def genArgsForConstructor(thisNode: js.This, args: Seq[js.Tree])
//                           (implicit pos: Position): List[js.Tree] = {
//    val declaredArgs = genArgs(args)
//    if (isNestedConstructor) thisNode :: declaredArgs
//    else declaredArgs
//  }
//
//
//  def genParamDefs(decls: List[jdt.SingleVariableDeclaration])
//                  (implicit pos: Position): List[js.ParamDef] = {
//    // It would be cool if the method binding gave us the parameter names, but
//    // alas, we have to ask the client to pass them explicitly
//    val declaredParams = declaredParamTypes.zip(decls)
//      .map { case (tpe, decl) =>
//        js.ParamDef(
//          js.LocalIdent(jsn.LocalName(decl.getName.getIdentifier)),
//          NoOriginalName,
//          tpe,
//          mutable = isMutable(decl.resolveBinding),
//          rest = decl.isVarargs
//        )
//      }
//    val outerParam =
//      if (isNestedConstructor)
//        List(js.ParamDef(
//          OuterLocalIdent,
//          NoOriginalName, paramTypes.head, mutable = false, rest = false
//        ))
//      else Nil
//    outerParam ::: declaredParams
//  }
//
//  def genParamDefs(params: java.util.List[_])
//                  (implicit pos: Position): List[js.ParamDef] =
//    genParamDefs(params.asScala[jdt.SingleVariableDeclaration])
//
//  def ident(implicit pos: Position): js.MethodIdent =
//    if (isConstructor)
//      js.MethodIdent(jsn.MethodName.constructor(paramTypes.map(sjsTypeRef)))
//    else
//      js.MethodIdent(jsn.MethodName(
//        binding.getName, paramTypes.map(sjsTypeRef), sjsTypeRef(returnType)
//      ))
//}
