// Add SBT commands to generate project definitions for Eclipse and IntelliJ.
// Use: `sbt eclipse` or `sbt gen-idea`

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.5.0")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

// Add SBT command to build a jar file which includes all required dependencies.

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.11.2")
