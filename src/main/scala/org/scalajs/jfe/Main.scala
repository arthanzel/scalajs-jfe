package org.scalajs.jfe

object Main extends App {
//  val src = Source.fromResource("TestClass.java")(Codec.UTF8)
//  val javaAST = ASTUtils.compileJavaString(src)

//  println("=== Java AST ===")
//  println(javaAST)

  val scala =
    """class Main {
      |    try {
      |      println("foo");
      |    } catch {
      |      case _: IllegalArgumentException => println("nie")
      |      case _ => println("other")
      |    } finally {
      |      println("finally")
      |    }
      |}
      |""".stripMargin
  val scalaIR = ASTUtils.compileString(scala)
  ASTUtils.compileString(scala).foreach(ASTUtils.printAST)

  val sourceCode =
    """
      |class Base {
      |    public void test() {
      |        int i = 0;
      |        int s = i++;
      |    }
      |}
      |""".stripMargin


  val ir = ASTUtils.javaToSJS(sourceCode)
  println("\n=== Java -> SJSIR ===")
  ir.foreach(ASTUtils.printAST)
}
