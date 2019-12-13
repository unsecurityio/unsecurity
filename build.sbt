ThisBuild / organization := "io.unsecurity"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / crossScalaVersions := Seq("2.12.8", "2.13.1")

bloopExportJarClassifiers in Global := Some(Set("sources"))

lazy val root = (project in file("."))
  .settings(name := "unsecurity")
  .configure(PublishSettings(_))
  .aggregate(core, auth0)

lazy val core = project.configure(PublishSettings(_))
lazy val auth0 = project.configure(PublishSettings(_)).dependsOn(core)
