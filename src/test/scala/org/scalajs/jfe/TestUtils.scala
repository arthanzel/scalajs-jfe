package org.scalajs.jfe

import org.scalajs.ir.{Trees => js}
import org.scalajs.jfe.trees.JDTCompiler
import org.scalatest.Assertion
import org.scalatest.Assertions.assert

object TestUtils {
  def assertJava(javaCode: String, expectedIR: String): Unit = {
    val jdt = ASTUtils.compileJavaString(javaCode)
    val sjs = JDTCompiler.gen(jdt)
    assert(sjs.length == 1)
    assertIREquals(sjs.head.show, expectedIR)
  }

  def assertIR(javaCode: String, scalaCode: String): Unit = {
    assertJava(javaCode, ASTUtils.astToString(ASTUtils.compileString(scalaCode).head))
  }

  def assertStatements(javaStatements: String, sjsirStatements: String): Unit = {
    val ast = ASTUtils.javaToSJS(
      s"""
         |package test
         |class Test {
         |  void test() {
         |    ${javaStatements}
         |  }
         |}
         |""".stripMargin)
    assert(ast.length == 1, "Java code didn't compile")
    val testStatements = ASTUtils.astToString(
      ast.head
        .memberDefs
        .collect { case x: js.MethodDef => x }
        .find {
          _.name.name.toString == "MethodName<test;V>"
        }
        .get
        .body
        .map {
          // Filter out return scope statement
          case x: js.Labeled => x.body
          case other => other
        }
        .get
    )
    assertIREquals(testStatements, sjsirStatements)
  }

  def trimWhitespace(s: String): String = s
    .split("\n")
//    .map(_.trim)
    .diff(Seq(""))
    .mkString("\n")

  def assertIREquals(ir: js.IRNode, expected: String): Assertion =
    assertIREquals(ir.show, expected)

  def assertIREquals(ir: String, expected: String): Assertion = {
    assert(trimWhitespace(ir) == trimWhitespace(expected))
  }
}
