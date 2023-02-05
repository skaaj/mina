ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "sekoya"
  )

libraryDependencies += "org.json4s" % "json4s-native_3" % "4.0.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"