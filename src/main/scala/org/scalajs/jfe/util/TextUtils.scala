package org.scalajs.jfe.util

import scala.collection.mutable

object TextUtils {
  private val freshNameMap = mutable.Map[String, Int]()

  def freshName(prefix: String = "unnamed", separator: String = "_"): String = {
    val Some(v) = freshNameMap.updateWith(prefix) { cur => Some(cur.map(_ + 1).getOrElse(0) ) }
    s"$prefix${separator}${v}"
  }

  def clearFreshNames(): Unit = freshNameMap.clear()
}
