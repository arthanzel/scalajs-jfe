package org.scalajs.jfe

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
      |  println(+i)
      |}
      |""".stripMargin


  val ir = ASTUtils.compileString(sourceCode)
  println("\n=== Scala -> SJSIR ===")
  ir.foreach(ASTUtils.printAST)
}
