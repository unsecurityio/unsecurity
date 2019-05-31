ThisBuild / organization := "io.unsecurity"
ThisBuild / scalaVersion := "2.12.8"

lazy val root = (project in file(".")).settings(name := "unsecurity")
  .aggregate(core, auth0)

lazy val core = project.configure(Settings.configure)
lazy val auth0 = project.configure(Settings.configure).dependsOn(core)

publishArtifact := false
publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
