package org.scalajs.jfe

import java.io.PrintWriter

import org.scalajs.ir.{Printers, Trees}
import org.scalajs.nscplugin.ScalaJSPlugin

import scala.io.{Codec, Source}
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.File
import scala.tools.nsc
import scala.tools.nsc.reporters.ConsoleReporter

object Main extends App {
//  val src = Source.fromResource("TestClass.java")(Codec.UTF8)
//  val javaAST = ASTUtils.compileJavaString(src)

//  println("=== Java AST ===")
//  println(javaAST)

  val sourceCode =
    """
      |package hello
      |import scala.scalajs.js
      |object HelloWorld {
      |  for (var i = 0; i < 5; i++) { println(i) }
      |}
      |""".stripMargin


  val ir = ASTUtils.compileString(sourceCode)
  println("\n=== Scala -> SJSIR ===")
  ir.foreach(ASTUtils.printAST)
}
