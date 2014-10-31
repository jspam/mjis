import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object MJIS extends Build {
  lazy val root =
    project
      .in(file("."))
      .settings(assemblySettings: _*)
      .settings(
        name := "mjis",
        jarName in assembly := "mjc.jar",
        scalaVersion := "2.11.4",
        scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint", "-Xfuture"),
        libraryDependencies += "com.github.scopt" % "scopt_2.11" % "3.2.0",
        libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
      )
}
