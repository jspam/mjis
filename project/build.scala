import sbt._
import Keys._

object MJIS extends Build {
  lazy val root = project.in(file(".")).settings(
    name := "mjis",
    scalaVersion := "2.11.2"
  )
}
