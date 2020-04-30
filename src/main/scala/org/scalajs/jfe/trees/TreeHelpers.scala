package org.scalajs.jfe.trees

import scala.reflect.ClassTag

object TreeHelpers {

  // JDT has lots of untyped lists. Here's a helper to cast them to concrete
  // Scala lists.
  implicit class ListHasAsScala(l: java.util.List[_]) {
    def asScala[A: ClassTag]: List[A] = l.toArray.map(_.asInstanceOf[A]).toList
  }
}
