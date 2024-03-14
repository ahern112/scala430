ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "agn8",
    // Add libraryDependencies setting to include ScalaTest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test
  )
