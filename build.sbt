name := "DataScienceFromScratch"

organization := "org.invisibletech"

scalaVersion := "2.11.7"

version := "0.0.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc()
)

initialCommands := "import org.invisibletech.datasciencefromscratch._"

