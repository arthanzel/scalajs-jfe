package org.scalajs.jfe

import java.util.concurrent.TimeUnit

import org.scalajs.ir.{Trees => js}
import org.scalajs.jfe.trees.JDTCompiler
import org.scalajs.jsenv.Input
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.test.kit.TestKit
import org.scalajs.logging.{Level, ScalaConsoleLogger}
import org.scalatest.Assertion
import org.scalatest.Assertions.assert

import scala.concurrent.duration.FiniteDuration

object TestUtils {
  def assertJava(javaCode: String, expectedIR: String): Unit = {
    val jdt = ASTUtils.compileJavaString(javaCode)
    val sjs = JDTCompiler(jdt)
    assert(sjs.length == 1)
    assertIREquals(sjs.head.show, expectedIR)
  }

  def assertIR(javaCode: String, scalaCode: String): Unit = {
    assertJava(javaCode, ASTUtils.astToString(ASTUtils.compileString(scalaCode).head))
  }

  def assertStatements(javaStatements: String, sjsirStatements: String, msg: String = ""): Unit = {
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
    assertIREquals(testStatements, sjsirStatements, msg)
  }

  def assertRun(javaCode: String, expectedOut: String): Unit = {
    // Remember to add \n to expectedOut
    val ast = ASTUtils.javaToSJS(javaCode).head
    val linked = Runner.link(ast, new ScalaConsoleLogger(Level.Error))
    val kit = new TestKit(new NodeJSEnv(), FiniteDuration(5L, TimeUnit.SECONDS))
    kit.withRun(Seq(Input.Script(linked))) { run =>
      run.expectOut(expectedOut + "\n").closeRun()
    }
  }

  def assertRun(javaCode: String, expectedOut: Seq[String]): Unit =
    assertRun(javaCode, expectedOut.mkString("\n"))

  def trimWhitespace(s: String): String = s
    .split("\n")
//    .map(_.trim)
    .diff(Seq(""))
    .mkString("\n")

  def assertIREquals(ir: js.IRNode, expected: String, msg: String): Assertion =
    assertIREquals(ir.show, expected, msg)

  def assertIREquals(ir: String, expected: String, msg: String = ""): Assertion = {
    assert(trimWhitespace(ir) == trimWhitespace(expected), msg)
  }
}
