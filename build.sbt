ThisBuild / organization := "io.unsecurity"
ThisBuild / scalaVersion := "3.0.2"

bloopExportJarClassifiers in Global := Some(Set("sources"))

lazy val root = (project in file("."))
  .settings(name := "unsecurity")
  .configure(PublishSettings(_))
  .aggregate(core, auth0)

lazy val core = project.configure(PublishSettings(_))
lazy val auth0 = project.configure(PublishSettings(_)).dependsOn(core)
