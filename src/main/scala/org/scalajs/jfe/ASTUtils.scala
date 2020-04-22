package org.scalajs.jfe

import java.io.{File, StringWriter}
import java.nio.file.Paths

import org.eclipse.jdt.core.{dom => jdt}
import org.scalajs.ir.Printers.IRTreePrinter
import org.scalajs.ir.Trees
import org.scalajs.jfe.trees.JDTCompiler
import org.scalajs.nscplugin.ScalaJSPlugin

import scala.io.BufferedSource
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.Path
import scala.tools.nsc
import scala.tools.nsc.reporters.ConsoleReporter

object ASTUtils {
  def newCompiler(onGeneratedAST: List[Trees.ClassDef] => Unit): nsc.Global = {
    val settings = newCompilerSettings()
    new nsc.Global(settings, new ConsoleReporter(settings)) {
      override lazy val plugins = List(
        new ScalaJSPlugin(this) {
          override def generatedJSAST(clDefs: List[Trees.ClassDef]): Unit = onGeneratedAST(clDefs)
        }
      )
    }
  }

  def newCompilerSettings(): nsc.Settings = {
    val s = new nsc.Settings()
    s.processArguments(List(
      "-bootclasspath", System.getProperty("jfe.scalalib"),
      "-classpath", Seq(System.getProperty("jfe.sjslib")).mkString(File.pathSeparator)
    ), processAll = true)
    s
  }

  def javaToSJS(javaCode: String): List[Trees.ClassDef] = {
    JDTCompiler.gen(compileJavaString(javaCode))
  }

  def compileJavaString(code: String): jdt.CompilationUnit = {
    val resources = Paths.get(".", "src/test/resources").toAbsolutePath.toString

    val parser = jdt.ASTParser.newParser(jdt.AST.JLS13)
    // TODO: Resolve bindings better
    parser.setResolveBindings(true)
    parser.setEnvironment(
      Array(resources),
      Array(resources),
      null,
      true
    )
    parser.setUnitName("unit")
    parser.setSource(code.toCharArray)
    parser.createAST(null).asInstanceOf[jdt.CompilationUnit]
  }

  def compileJavaString(code: BufferedSource): jdt.CompilationUnit =
    compileJavaString(code.mkString)

  def compileString(code: String): List[Trees.ClassDef] = {
    var ast: List[Trees.ClassDef] = List()
    val global = newCompiler(ast = _)
    val run = new global.Run()
    run.compileSources(List(
      new BatchSourceFile(s"${TextUtils.freshName("source")}.scala", code)
    ))
    ast
  }

  def compileString(code: BufferedSource): List[Trees.ClassDef] =
    compileString(code.mkString)

  def astToString(node: Trees.IRNode): String = {
    val writer = new StringWriter()
    val printer = new IRTreePrinter(writer)
    printer.printAnyNode(node)
    writer.append('\n')
    writer.flush()
    writer.toString
  }

  def printAST(node: Trees.IRNode): Unit = {
    println(astToString(node))
    println(" \n")
  }
}
