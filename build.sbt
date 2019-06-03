name := "15-puzzle"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies +=  "org.scalatest" % "scalatest_2.12" % "3.0.5" % Test

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.0" withSources() withJavadoc()