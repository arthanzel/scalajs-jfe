package org.scalajs.jfe

import java.io.PrintWriter
import java.nio.file.Paths

import org.eclipse.jdt.core.{JavaCore, dom => jdt}
import org.scalajs.ir.{Printers, Trees}
import org.scalajs.nscplugin.ScalaJSPlugin

import scala.io.{Codec, Source}
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.File
import scala.tools.nsc
import scala.tools.nsc.reporters.ConsoleReporter

import scala.jdk.CollectionConverters._

object Main extends App {
  val resources = Paths.get(".", "src/test/resources").toAbsolutePath.toString
  val parser = jdt.ASTParser.newParser(jdt.AST.JLS13)
  parser.setResolveBindings(true)
  parser.setEnvironment(
    Array(resources),
    Array(resources),
    null,
    true
  )
  parser.setSource(
    """class Main {
    public static void main() {
        Boolean bool = true;
        Byte b = 10;
        Short s = 11;
        Integer i = 12;
        Long l = 13L;
        System.out.println(bool);
        System.out.println(b);
        System.out.println(s);

        byte b2 = b;
        System.out.println(b2);
    }
}""".stripMargin.toCharArray)
  parser.setUnitName("unit")
  parser.setCompilerOptions(Map(
    JavaCore.COMPILER_PB_AUTOBOXING -> "warning",
    JavaCore.COMPILER_SOURCE -> "11",
  ).asJava)
  val ast = parser.createAST(null).asInstanceOf[jdt.CompilationUnit]

  ast.getProblems.map { p =>
    if (p.isError) println(s"ERROR: ${p.getMessage}")
    else if (p.isWarning) println(s"Warning: ${p.getMessage}")
    else println(s"Info: ${p.getMessage}")
  }

  val i = 0

//  val src = Source.fromResource("TestClass.java")(Codec.UTF8)
//  val javaAST = ASTUtils.compileJavaString(src)

//  println("=== Java AST ===")
//  println(javaAST)

//  val sourceCode =
//    """
//      |object HelloWorld {
//      |  val i: Int = 10
//      |  val f: Float = 1.5f
//      |  val r = i + f
//      |}
//      |""".stripMargin
//
//
//  val ir = ASTUtils.compileString(sourceCode)
//  println("\n=== Scala -> SJSIR ===")
//  ir.foreach(ASTUtils.printAST)
}
