package org.scalajs.jfe

import java.nio.file.{Files, Path}

import com.google.common.jimfs.Jimfs
import org.scalajs.ir
import org.scalajs.ir.Trees.ClassDef
import org.scalajs.ir.{Trees => js}
import org.scalajs.jfe.util.TextUtils
import org.scalajs.jsenv.{Input, RunConfig, nodejs}
import org.scalajs.linker.interface.unstable.IRFileImpl
import org.scalajs.linker.interface.{LinkerOutput, ModuleInitializer, StandardConfig}
import org.scalajs.linker.{PathIRContainer, PathOutputFile, StandardImpl}
import org.scalajs.logging.{Logger, NullLogger, ScalaConsoleLogger}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

object Runner {
  import scala.concurrent.ExecutionContext.Implicits.global

  private lazy val libraryIRFiles = {
    // Load the standard library
    val libraryPathStr = System.getProperty("jfe.sjslib")
    val libraryPath = new java.io.File(libraryPathStr).toPath
    val cache = StandardImpl.irFileCache().newCache
    val future = PathIRContainer.fromClasspath(Seq(libraryPath)).flatMap {
      pair => cache.cached(pair._1)
    }
    Await.result(future, Duration.Inf)
  }

  def link(classDef: ClassDef, logger: Logger, mainClass: String): Path =
    link(Seq(classDef), logger, mainClass)

  def link(classDefs: Seq[ClassDef], logger: Logger, mainClass: String): Path = {
    val irFiles = classDefs.map { classDef =>
      new ClassDefIRFileImpl(
        s"${TextUtils.freshName("irfile")}.sjsir",
        None,
        classDef)
    }

    val allIRFiles = libraryIRFiles ++ irFiles
    val config = StandardConfig().withCheckIR(true).withOptimizer(false)
    val linker = StandardImpl.linker(config)

//    val output = Jimfs.newFileSystem().getPath("output.js")
    val output = new java.io.File("output.js").toPath
    val future = linker.link(allIRFiles,
      List(ModuleInitializer.mainMethod(mainClass, "main")),
      LinkerOutput(PathOutputFile(output)), logger)
    Await.result(future, Duration.Inf)
    output
  }

  /** A simple in-memory IR file containing a `ClassDef`. */
  private final class ClassDefIRFileImpl(
                                          path: String,
                                          version: Option[String],
                                          classDef: ClassDef
                                        ) extends IRFileImpl(path, version) {
    def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] =
      Future.successful(ir.EntryPointsInfo.forClassDef(classDef))

    def tree(implicit ec: ExecutionContext): Future[ClassDef] =
      Future.successful(classDef)
  }

  def linkAndRun(classDef: js.ClassDef, mainClass: String = "test.Test"): Unit = {
    val linked = link(classDef, new ScalaConsoleLogger(), mainClass)

    val jsEnv = new nodejs.NodeJSEnv()
    val input = Seq(Input.Script(linked))
    val runConfig = RunConfig().withLogger(new ScalaConsoleLogger())

    Await.result(jsEnv.start(input, runConfig).future, Duration.Inf)
  }
}
