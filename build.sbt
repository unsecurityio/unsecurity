organization in ThisBuild := "io.unsecurity"
scalaVersion in ThisBuild := "2.12.8"

lazy val root = (project in file("."))
  .aggregate(core, auth0)

lazy val core = project
lazy val auth0 = project.dependsOn(core)

useGpg := true
