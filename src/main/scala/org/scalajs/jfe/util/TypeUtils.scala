package org.scalajs.jfe.util

import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.{Position, Names => jsn, Trees => js, Types => jst}
import org.scalajs.jfe.JavaCompilationException

object TypeUtils {
  val HijackedClasses: Set[String] = Set("java.lang.Boolean",
    "java.lang.Character", "java.lang.Byte", "java.lang.Short",
    "java.lang.Integer", "java.lang.Long", "java.lang.Float",
    "java.lang.Double", "java.lang.String")
  val JDKObjectType: jst.ClassType = jst.ClassType(jsn.ClassName("java.lang.Object"))
  val JDKStringType: jst.ClassType = jst.ClassType(jsn.ClassName("java.util.String"))
  val JDKStringRef: jst.TypeRef = sjsTypeRef(JDKStringType)

  def isStringy(tree: js.Tree): Boolean = tree.tpe == JDKStringType || tree.tpe == jst.StringType

  def isShift(op: jdt.InfixExpression.Operator): Boolean = {
    import jdt.InfixExpression.Operator._
    op match {
      case LEFT_SHIFT | RIGHT_SHIFT_SIGNED | RIGHT_SHIFT_UNSIGNED => true
      case _ => false
    }
  }

  private val BoxedByteType = jst.ClassType(jsn.ClassName("java.lang.Byte"))
  private val BoxedCharType = jst.ClassType(jsn.ClassName("java.lang.Character"))
  private val BoxedShortType = jst.ClassType(jsn.ClassName("java.lang.Short"))
  private val BoxedIntType = jst.ClassType(jsn.ClassName("java.lang.Integer"))
  private val BoxedLongType = jst.ClassType(jsn.ClassName("java.lang.Long"))
  private val BoxedFloatType = jst.ClassType(jsn.ClassName("java.lang.Float"))
  private val BoxedDoubleType = jst.ClassType(jsn.ClassName("java.lang.Double"))
  private val BoxedBooleanType = jst.ClassType(jsn.ClassName("java.lang.Boolean"))

  private val unboxingMap: Map[jst.Type, jst.PrimTypeWithRef] = Map(
    "Boolean" -> jst.BooleanType,
    "Byte" -> jst.ByteType,
    "Character" -> jst.CharType,
    "Short" -> jst.ShortType,
    "Integer" -> jst.IntType,
    "Long" -> jst.LongType,
    "Float" -> jst.FloatType,
    "Double" -> jst.DoubleType,
  ).map { case (cls, tpe) =>
    jst.ClassType(jsn.ClassName(s"java.lang.$cls")) -> tpe
  }
  private val boxingMap: Map[jst.Type, jst.ClassType] =
    unboxingMap.map(x => (x._2, x._1.asInstanceOf[jst.ClassType]))
  private val boxableTypes = boxingMap.keySet
  private val unboxableTypes = unboxingMap.keySet

  def isStatic(m: jdt.IBinding): Boolean =
    jdt.Modifier.isStatic(m.getModifiers)

  def isMutable(vb: jdt.IVariableBinding): Boolean =
    if (vb.isField && isStatic(vb)) {
      // Static fields cannot be 'val' because they must be assigned in a
      // synthetic static initializer
      true
    }
    else {
      !jdt.Modifier.isFinal(vb.getModifiers) && !vb.isEffectivelyFinal
    }

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

  def sjsType(_t: jdt.ITypeBinding): jst.Type = {
    val t = _t.getErasure
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

  def sjsTypeRef(_t: jdt.ITypeBinding): jst.TypeRef = {
    val t = _t.getErasure
    if (t.isPrimitive) sjsType(t).asInstanceOf[jst.PrimTypeWithRef].primRef
    else if (t.isArray) sjsArrayTypeRef(t)
    else if (t.isClass) jst.ClassRef(jsn.ClassName(t.getQualifiedName))
    else {
      ???
    }
  }

  def sjsTypeRef(tpe: jst.Type): jst.TypeRef = tpe match {
    case t: jst.ArrayType => t.arrayTypeRef
    case t: jst.PrimTypeWithRef => t.primRef
    case t: jst.ClassType => jst.ClassRef(t.className)
    case jst.AnyType => jst.ClassRef(jsn.ClassName("java.lang.Object"))
    case _ =>
      throw new JavaCompilationException(s"Can't compute a typeref for $tpe")
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
    if (from == jst.StringType && to == jst.ClassType(jsn.ClassName("java.lang.String"))) return tree

    if (from.isInstanceOf[jst.PrimType] && to.isInstanceOf[jst.PrimType]) return adaptPrimitive(tree, to)

    // Stringify
//    if (to == jst.StringType || to == JDKStringType) return from match {
//      case _: jst.ClassType =>
//        js.Apply(
//          js.ApplyFlags.empty,
//          tree,
//          js.MethodIdent(jsn.MethodName("toString", Nil, JDKStringRef)),
//          Nil
//        )(JDKStringType)
//      case _: jst.PrimType =>
//        // TODO: Stringify primitives
//        tree
//    }

    /*
    Boxing:
    ScalaJS automatically boxes primitive types, but we need to make sure that
    they are cast to the right primitive first.
    TODO: This doesn't match Java's semantics exactly. E.g. it allows narrowing
      conversions in assignments or method calls.
     */
    if (boxableTypes.contains(from) && unboxableTypes.contains(to))
      return adaptPrimitive(tree, unboxingMap(to))

    /*
    Unboxing needs to be done explicitly.
     */
    if (boxableTypes.contains(to) && unboxableTypes.contains(from))
      return adaptPrimitive(js.AsInstanceOf(tree, unboxingMap(from)), to)

    //    ???
    // TODO: Specify explicitly casting cases
    tree
  }

  // region Boxing

  def tryBox(tree: js.Tree, to: jst.Type)
            (implicit pos: Position): Option[js.Tree] = {
    try {
      // ScalaJS boxes automatically, but primitives must be coerced to the
      // proper type first.
      val primType = unboxingMap(to)
      Some(adaptPrimitive(tree, primType))
    } catch {
      // Tree can't be boxed
      case _: NoSuchElementException => None
    }
  }

  // endregion

  def tryUnbox(tree: js.Tree, to: jst.Type)
              (implicit pos: Position): Option[js.Tree] =
    unboxingMap.get(tree.tpe) match {
      case Some(to) => Some(js.AsInstanceOf(tree, to))
      case _ => None
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

  private def adaptPrimitive(value: js.Tree, to: jst.Type)(
    implicit pos: Position): js.Tree = {
    import js.UnaryOp._

    val from = value.tpe

    if (from == to || from == jst.NothingType) {
      value
    } else if (from == jst.BooleanType || to == jst.BooleanType) {
      throw new AssertionError(s"Invalid genConversion from $from to $to")
    } else {
      def intValue = (from: @unchecked) match {
        case jst.IntType => value
        case jst.CharType => js.UnaryOp(CharToInt, value)
        case jst.ByteType => js.UnaryOp(ByteToInt, value)
        case jst.ShortType => js.UnaryOp(ShortToInt, value)
        case jst.LongType => js.UnaryOp(LongToInt, value)
        case jst.FloatType => js.UnaryOp(DoubleToInt, js.UnaryOp(FloatToDouble, value))
        case jst.DoubleType => js.UnaryOp(DoubleToInt, value)
      }

      def doubleValue = from match {
        case jst.DoubleType => value
        case jst.FloatType => js.UnaryOp(FloatToDouble, value)
        case jst.LongType => js.UnaryOp(LongToDouble, value)
        case _ => js.UnaryOp(IntToDouble, intValue)
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
  
  def getBinaryOpResultType(left: jst.Type, right: jst.Type, 
                            isShift: Boolean = false): jst.Type = {
    (left, right) match {
      case (jst.LongType, _) if isShift => jst.LongType
      case _ if isShift => jst.IntType

      case (jst.DoubleType, _) | (_, jst.DoubleType) =>
        jst.DoubleType
      case (jst.FloatType, _) | (_, jst.FloatType) =>
        jst.FloatType
      case (jst.LongType, _) | (_, jst.LongType) =>
        jst.LongType
      case (jst.IntType | jst.CharType | jst.ByteType | jst.ShortType, _) |
           (_, jst.IntType | jst.CharType | jst.ByteType | jst.ShortType) =>
        jst.IntType
      case (jst.BooleanType, _) | (_, jst.BooleanType) =>
        jst.BooleanType
      case _ =>
        jst.AnyType
    }
  }

  // endregion
}
