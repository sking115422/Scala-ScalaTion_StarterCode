val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "hw3_ex14",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
