package org.scalajs.jfe.functional

import org.scalajs.ir.{Types => jst}
import org.eclipse.jdt.core.dom.{ASTVisitor, TypeDeclaration}
import org.scalajs.jfe.ASTUtils
import org.scalajs.jfe.trees.{Capture, CaptureVisitor}
import org.scalatest.funspec.AnyFunSpec

class LocalClassTests extends AnyFunSpec {
  import org.scalajs.jfe.util.TypeUtils._
  import org.scalajs.jfe.TestUtils._

  def mkMain(src: String): String =
    s"""class Main {
       |    public static void main() {
       |        ${src}
       |    }
       |}
      |""".stripMargin

  describe("For local classes:") {
    it("emits local classes as top-level elements") {
      val src = mkMain(
        """class Inner {};
          |""".stripMargin)
      val ir = ASTUtils.javaToSJS(src)
      assert(ir.length == 2)
    }

    it("identifies captured variables") {
      val src = mkMain(
        """int capture1 = 10;
           |String capture2 = "capture";
           |Object capture3 = new Object();
           |Object captureUnused = new Object();
           |class Inner{
           |    void foo() {
           |        int local1 = 20;
           |        String local2 = "local";
           |        Object local3 = new Object();
           |        System.out.printf("", capture1, capture2, capture3,
           |            local1, local2, local3);
           |    }
           |}""".stripMargin)
      val ir = ASTUtils.compileJavaString(src)
      val visitor = new CaptureVisitor()
      ir.types.get(0).asInstanceOf[TypeDeclaration].accept(visitor)
      val expected = Set(
        Capture("capture1", jst.IntType),
        Capture("capture2", JDKStringType),
        Capture("capture3", jst.AnyType),
      )
      ASTUtils.javaToSJS(src)
//      assert(visitor.captures == expected)
    }

    it("sandbox") {
      val src =
        """class Holder {
           |    public Holder() {}
           |}
           |
           |class Main {
           |    public static void main() {
           |        String s = "capture";
           |        Holder h = new Holder() {
           |            public String toString() {
           |                return s;
           |            }
           |        };
           |        System.out.println(h.toString());
           |    }
           |}
          |""".stripMargin

      ASTUtils.javaToSJS(src).foreach(ASTUtils.printAST)
      assertRun(src, "capture")
    }
  }
}
