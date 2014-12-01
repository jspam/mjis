import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object MJIS extends Build {
  val shellScript = Seq("#!/usr/bin/env sh", """LD_LIBRARY_PATH=$LD_LIBRARY_PATH:lib/ exec java -jar "$0" "$@" """)

  lazy val root =
    project
      .in(file("."))
      .settings(assemblySettings: _*)
      .settings(
        name := "mjis",
        jarName in assembly := "run.sh",
        mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
            case PathList("META-INF") => MergeStrategy.discard
            case x => old(x)
          }
        },
        mainClass in assembly := Some("mjis.CLIMain"),
        assemblyOption in assembly ~= { _.copy(prependShellScript = Some(shellScript)) },
        scalaVersion := "2.11.4",
        scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint", "-Xfuture"),
        libraryDependencies += "com.github.scopt" % "scopt_2.11" % "3.2.0",
        libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
        parallelExecution in test := false
      )
}
