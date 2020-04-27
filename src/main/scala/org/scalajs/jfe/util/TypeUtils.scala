package org.scalajs.jfe.util

import org.eclipse.jdt.core.{dom => jdt}

import org.scalajs.ir.{Trees => js, Names => jsn, Types => jst}

object TypeUtils {
  def isStatic(m: jdt.IBinding): Boolean =
    jdt.Modifier.isStatic(m.getModifiers)

  def memberNamespace(m: Int): js.MemberNamespace = {
    import jdt.Modifier

    if (Modifier.isPublic(m) && Modifier.isStatic(m))
      js.MemberNamespace.PublicStatic
    else if (Modifier.isPublic(m))
      js.MemberNamespace.Public
    else if (Modifier.isStatic(m))
      js.MemberNamespace.PrivateStatic
    else
      js.MemberNamespace.Private

    // TODO: Protected scope?
  }

  def nsAccess(m: Int): js.MemberNamespace = {
    import jdt.Modifier
    if (Modifier.isPublic(m)) js.MemberNamespace.Public
    else js.MemberNamespace.Public
  }

  def sjsType(t: jdt.ITypeBinding): jst.Type = {
    if (t.isPrimitive) {
      t.getName match {
        case "boolean" => jst.BooleanType
        case "byte" => jst.ByteType
        case "char" => jst.CharType
        case "double" => jst.DoubleType
        case "float" => jst.FloatType
        case "int" => jst.IntType
        case "long" => jst.LongType
        case "short" => jst.ShortType
        case "void" => jst.NoType
      }
    }
    else if (t.isArray) {
      jst.ArrayType(sjsArrayTypeRef(t))
    }
    else if (t.isClass && t.getQualifiedName == "java.lang.Object") jst.AnyType
    else if (t.isClass) jst.ClassType(jsn.ClassName(t.getQualifiedName))
    else {
      ???
    }
  }

  def sjsArrayTypeRef(t: jdt.ITypeBinding): jst.ArrayTypeRef = {
    jst.ArrayTypeRef(
      sjsTypeRef(t.getElementType).asInstanceOf[jst.NonArrayTypeRef],
      t.getDimensions
    )
  }

  def sjsTypeRef(t: jdt.ITypeBinding): jst.TypeRef = {
    if (t.isPrimitive) sjsType(t).asInstanceOf[jst.PrimTypeWithRef].primRef
    else if (t.isArray) sjsArrayTypeRef(t)
    else if (t.isClass) jst.ClassRef(jsn.ClassName(t.getQualifiedName))
    else {
      ???
    }
  }

  def sjsTypeRef(t: jst.Type): jst.TypeRef = t match {
    case t: jst.PrimTypeWithRef => t.primRef
    case t: jst.ClassType => jst.ClassRef(t.className)
    case _ => ???
  }
}
