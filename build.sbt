ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.3" cross CrossVersion.full)
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.5.4"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.4" % "test"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "pc-from-scratch"
  )
