package org.scalajs.jfe.trees

import scala.collection.mutable

class ScopedStack[T] {
  private val stack: mutable.Stack[T] = mutable.Stack()

  def get: T = stack.top

  def getOption: Option[T] = stack.lastOption

  def withValue[R](key: T)(body: T => R): R = {
    stack.push(key)
    try body(key)
    finally stack.pop()
  }
}
