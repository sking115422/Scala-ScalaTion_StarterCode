
lazy val my_scalation = project.in(file("."))
  .settings(
    scalaVersion  := "3.1.0",
//  scalacOptions += "-deprecation",
//  javacOptions  += "--add-modules jdk.incubator.vector"
  )

fork := true

