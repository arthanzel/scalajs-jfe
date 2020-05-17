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
//  val src = Source.fromResource("TestClass.java")(Codec.UTF8)
//  val javaAST = ASTUtils.compileJavaString(src)

//  println("=== Java AST ===")
//  println(javaAST)

  val sourceCode =
    """
      |object HelloWorld {
      |  val i: Int = 10
      |  val f: Float = 1.5f
      |  val r = i + f
      |  println(-i)
      |}
      |""".stripMargin


  val ir = ASTUtils.compileString(sourceCode)
  println("\n=== Scala -> SJSIR ===")
  ir.foreach(ASTUtils.printAST)
}
