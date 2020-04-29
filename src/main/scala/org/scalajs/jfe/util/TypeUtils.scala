package org.scalajs.jfe.util

import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.{Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.JavaCompilationException

object TypeUtils {
  def isStatic(m: jdt.IBinding): Boolean =
    jdt.Modifier.isStatic(m.getModifiers)

  def memberNamespace(m: Int): js.MemberNamespace = {
    import jdt.Modifier

    // TODO: Support private methods? JDT already does access checking and private/public are just namespacing details in SJSIR

    if (Modifier.isPublic(m) && Modifier.isStatic(m))
      js.MemberNamespace.PublicStatic
    else if (Modifier.isPublic(m))
      js.MemberNamespace.Public
    else if (Modifier.isStatic(m))
      js.MemberNamespace.PublicStatic
    else
      js.MemberNamespace.Public

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

  def cast(tree: js.Tree, to: jst.Type, isAssignment: Boolean = false)
          (implicit pos: Position): js.Tree = {
    // TODO: Optimize, type-check, and bounds-check casts
    // Right now, this method inserts runtime casts, but many of those,
    // especially casts on constant expressions, can be optimized out.
    // See https://docs.oracle.com/javase/specs/jls/se12/html/jls-5.html
    // for Java's value conversion rules.

    val from = tree.tpe
    if (from == to) return tree

    if (to.isInstanceOf[jst.PrimType]) return genConversion(tree.tpe, to, tree)

    tree
  }

  private def literalLongValue(lit: js.Literal): Long = lit match {
    case x: js.ByteLiteral => x.value
    case x: js.CharLiteral => x.value
    case x: js.ShortLiteral => x.value
    case x: js.IntLiteral => x.value
    case x: js.LongLiteral => x.value
    case _ => throw new JavaCompilationException(s"Literal $lit of type ${lit.tpe} does not have a fixed-width representation")
  }

  private def staticNarrowingCast(lit: js.Literal, to: jst.Type)
                         (implicit pos: Position): js.Literal = {
    val longValue = literalLongValue(lit)
    def ensureBound(value: Long, min: Long, max: Long): Unit = {
      if (value < min || value > max) {
        throw new JavaCompilationException(s"Lossy conversion: $value does not fit in [$min, $max]")
      }
    }

    to match {
      case jst.ByteType =>
        ensureBound(longValue, Byte.MinValue, Byte.MaxValue)
        js.ByteLiteral(longValue.toByte)
      case jst.CharType =>
        ensureBound(longValue, Char.MinValue, Char.MaxValue)
        js.CharLiteral(longValue.toChar)
      case jst.ShortType =>
        ensureBound(longValue, Short.MinValue, Short.MaxValue)
        js.ShortLiteral(longValue.toShort)
    }
  }

  private def staticWideningCast(t: js.Literal, to: jst.Type)
                        (implicit pos: Position): js.Literal = (t, to) match {
    case (x: js.FloatLiteral, jst.DoubleType) => js.DoubleLiteral(x.value)

    case (x: js.LongLiteral, jst.DoubleType) => js.DoubleLiteral(x.value)
    case (x: js.LongLiteral, jst.FloatType) => js.FloatLiteral(x.value)

    case (x: js.IntLiteral, jst.DoubleType) => js.DoubleLiteral(x.value)
    case (x: js.IntLiteral, jst.FloatType) => js.FloatLiteral(x.value)
    case (x: js.IntLiteral, jst.LongType) => js.LongLiteral(x.value)

    case (x: js.ShortLiteral, jst.DoubleType) => js.DoubleLiteral(x.value)
    case (x: js.ShortLiteral, jst.FloatType) => js.FloatLiteral(x.value)
    case (x: js.ShortLiteral, jst.LongType) => js.LongLiteral(x.value)
    case (x: js.ShortLiteral, jst.IntType) => js.IntLiteral(x.value)

    case (x: js.CharLiteral, jst.DoubleType) => js.DoubleLiteral(x.value)
    case (x: js.CharLiteral, jst.FloatType) => js.FloatLiteral(x.value)
    case (x: js.CharLiteral, jst.LongType) => js.LongLiteral(x.value)
    case (x: js.CharLiteral, jst.IntType) => js.IntLiteral(x.value)

    case (x: js.ByteLiteral, jst.DoubleType) => js.DoubleLiteral(x.value)
    case (x: js.ByteLiteral, jst.FloatType) => js.FloatLiteral(x.value)
    case (x: js.ByteLiteral, jst.LongType) => js.LongLiteral(x.value)
    case (x: js.ByteLiteral, jst.IntType) => js.IntLiteral(x.value)
    case (x: js.ByteLiteral, jst.ShortType) => js.ShortLiteral(x.value)
  }

  // https://docs.oracle.com/javase/specs/jls/se12/html/jls-5.html
  val wideningMap: Map[jst.Type, Set[jst.Type]] = Map(
    jst.FloatType -> Set(jst.DoubleType),
    jst.LongType -> Set(jst.DoubleType, jst.FloatType),
    jst.IntType -> Set(jst.DoubleType, jst.FloatType, jst.LongType),
    jst.ShortType -> Set(jst.DoubleType, jst.FloatType, jst.LongType, jst.IntType),
    jst.CharType -> Set(jst.DoubleType, jst.FloatType, jst.LongType, jst.IntType),
    jst.ByteType -> Set(jst.DoubleType, jst.FloatType, jst.LongType, jst.IntType, jst.ShortType),
  )

  def isWidening(from: jst.Type, to: jst.Type): Boolean = wideningMap.contains(from) && wideningMap(from).contains(to)

  // region Code from Scala.js

  private def genConversion(from: jst.Type, to: jst.Type, value: js.Tree)(
    implicit pos: Position): js.Tree = {
    import js.UnaryOp._

    if (from == to || from == jst.NothingType) {
      value
    } else if (from == jst.BooleanType || to == jst.BooleanType) {
      throw new AssertionError(s"Invalid genConversion from $from to $to")
    } else {
      def intValue = (from: @unchecked) match {
        case jst.IntType    => value
        case jst.CharType   => js.UnaryOp(CharToInt, value)
        case jst.ByteType   => js.UnaryOp(ByteToInt, value)
        case jst.ShortType  => js.UnaryOp(ShortToInt, value)
        case jst.LongType   => js.UnaryOp(LongToInt, value)
        case jst.FloatType  => js.UnaryOp(DoubleToInt, js.UnaryOp(FloatToDouble, value))
        case jst.DoubleType => js.UnaryOp(DoubleToInt, value)
      }

      def doubleValue = from match {
        case jst.DoubleType => value
        case jst.FloatType  => js.UnaryOp(FloatToDouble, value)
        case jst.LongType   => js.UnaryOp(LongToDouble, value)
        case _                => js.UnaryOp(IntToDouble, intValue)
      }

      (to: @unchecked) match {
        case jst.CharType =>
          js.UnaryOp(IntToChar, intValue)
        case jst.ByteType =>
          js.UnaryOp(IntToByte, intValue)
        case jst.ShortType =>
          js.UnaryOp(IntToShort, intValue)
        case jst.IntType =>
          intValue
        case jst.LongType =>
          from match {
            case jst.FloatType | jst.DoubleType =>
              js.UnaryOp(DoubleToLong, doubleValue)
            case _ =>
              js.UnaryOp(IntToLong, intValue)
          }
        case jst.FloatType =>
          js.UnaryOp(js.UnaryOp.DoubleToFloat, doubleValue)
        case jst.DoubleType =>
          doubleValue
      }
    }
  }

  // endregion
}
