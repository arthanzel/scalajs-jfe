package org.scalajs.jfe

import org.scalajs.jfe.util.{TextUtils => tu}

import org.scalatest.BeforeAndAfter
import org.scalatest.funspec.AnyFunSpec

class TextUtilsTest extends AnyFunSpec with BeforeAndAfter {

  before {
    tu.clearFreshNames()
  }

  describe("freshName method") {
    it("should generate fresh names") {
      assert(tu.freshName() == "unnamed_0")
      assert(tu.freshName() == "unnamed_1")
      assert(tu.freshName() == "unnamed_2")
    }

    it("should generate fresh names with a custom prefix") {
      assert(tu.freshName("foo") == "foo_0")
      assert(tu.freshName("foo") == "foo_1")
      assert(tu.freshName("bar") == "bar_0")
      assert(tu.freshName("bar") == "bar_1")
      assert(tu.freshName("unnamed") == "unnamed_0")
      assert(tu.freshName() == "unnamed_1")
    }

    it("should generate fresh names with a separator") {
      assert(tu.freshName("foo", ".") == "foo.0")
      assert(tu.freshName("foo", "?") == "foo?1")
      assert(tu.freshName("bar", ":") == "bar:0")
      assert(tu.freshName() == "unnamed_0")
    }
  }
}
