package org.scalajs.jfe

import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import org.scalajs.ir.{Trees => js}
import org.scalajs.jfe.trees.JDTCompiler
import org.scalajs.jsenv.{Input, RunConfig, nodejs}
import org.scalajs.logging.{Level, ScalaConsoleLogger}
import org.scalatest.Assertion
import org.scalatest.Assertions.assert

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object TestUtils {
  private val MAX_RUNTIME = Duration(5, TimeUnit.SECONDS)

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
      s"""package test;
         |class Test {
         |  void test() {
         |    $javaStatements
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

  def assertRun(_javaCode: String, expectedOut: String): Unit = {
    // Convenience: put everything under the "test" package
    val javaCode =
      if (!_javaCode.startsWith("package test;\n"))
        "package test;\n" + _javaCode
      else _javaCode

    // Remember to add \n to expectedOut
    val asts = ASTUtils.javaToSJS(javaCode)
    val linked = Runner.link(asts, new ScalaConsoleLogger(Level.Error), "test.Main")

    val jsEnv = new nodejs.NodeJSEnv()
    val input = Seq(Input.Script(linked))
    var out: Option[InputStream] = None
    val runConfig = RunConfig()
      .withInheritOut(false)
      .withOnOutputStream { (o, _) => out = o }
    Await.result(jsEnv.start(input, runConfig).future, MAX_RUNTIME)

    val actualOut = out match {
      case Some(o) => new String(o.readAllBytes(), StandardCharsets.UTF_8)
      case None => "Could not capture JS output stream!"
    }
    assert(actualOut == expectedOut + "\n")
  }

  def assertRun(javaCode: String, expectedOut: Seq[Any]): Unit =
    assertRun(javaCode, expectedOut.map(_.toString).mkString("\n"))

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

class StringLogger extends org.scalajs.logging.Logger {
  val buffer: StringBuilder = new StringBuilder()

  def mkString: String = buffer.mkString

  override def log(level: Level, message: => String): Unit = buffer ++= message

  override def trace(t: => Throwable): Unit = t.printStackTrace()
}
