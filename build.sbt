ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode2023"
  )

libraryDependencies += "org.scalanlp" %% "breeze" % "2.1.0"
