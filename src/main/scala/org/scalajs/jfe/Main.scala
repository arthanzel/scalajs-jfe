package org.scalajs.jfe

object Main extends App {
//  val src = Source.fromResource("TestClass.java")(Codec.UTF8)
//  val javaAST = ASTUtils.compileJavaString(src)

//  println("=== Java AST ===")
//  println(javaAST)

  val sourceCode =
    """
      |class Main {
      |    static void log(String s) {System.out.println(s);}
      |    static void main() {
      |    int i = 4;
      |        switch (i % 2) {
      |  case 0:
      |    log("even");
      |    break;
      |  case 1:
      |    log("odd");
      |  default:
      |    log("fallthrough");
      |}
      |    }
      |}
      |""".stripMargin


  val ir = ASTUtils.javaToSJS(sourceCode)
  println("\n=== Java -> SJSIR ===")
  ir.foreach(ASTUtils.printAST)
}
