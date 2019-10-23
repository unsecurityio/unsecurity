ThisBuild / organization := "io.unsecurity"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / crossScalaVersions := Seq("2.12.8", "2.13.1")

lazy val root = (project in file(".")).settings(name := "unsecurity")
  .aggregate(core, auth0)

lazy val core = (project in file("core"))
lazy val auth0 = (project in file("auth0")).dependsOn(core)
