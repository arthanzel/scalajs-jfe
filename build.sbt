import java.util.NoSuchElementException

name := "scalajs-jfe"
version := "0.1"
scalaVersion := "2.13.1"

val scalaJSVersion = "1.0.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-encoding", "utf-8")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.eclipse.jdt" % "org.eclipse.jdt.core" % "3.20.0",
  "org.scala-js" % "scalajs-compiler_2.13.0" % scalaJSVersion,
  "org.scala-js" %% "scalajs-library" % scalaJSVersion,
  "org.scala-js" %% "scalajs-linker" % scalaJSVersion,
  "org.scala-js" %% "scalajs-env-nodejs" % scalaJSVersion,
  "org.scala-js" %% "scalajs-js-envs-test-kit" % scalaJSVersion,
  "org.scalatest" % "scalatest_2.13" % "3.1.1" % "test",
)

//fork := true

javaOptions ++= {
  def getDependency(name: String): File = {
    update.value.select(moduleFilter(name = name)).headOption.getOrElse {
      throw new NoSuchElementException(s"Could not find $name in dependencies")
    }
  }

  System.setProperty("jfe.scalalib", getDependency("scala-library*").getAbsolutePath)
  System.setProperty("jfe.sjslib", getDependency("scalajs-library*").getAbsolutePath)
  System.setProperty("jfe.outdir", "compile-out")
  Seq()
}
