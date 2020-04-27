package org.scalajs.jfe.util

object AccessCounted {
  def apply[A](value: A): AccessCounted[A] = new AccessCounted[A](value)
}

class AccessCounted[A](private val value: A) {
  private var counter: Int = 0

  def count: Int = counter

  def get: A = {
    counter += 1; value
  }

  def isLive: Boolean = count > 0
}
