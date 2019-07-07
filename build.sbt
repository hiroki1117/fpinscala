import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.13.0",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "FPinScala",
    libraryDependencies += scalaTest % Test
  )
