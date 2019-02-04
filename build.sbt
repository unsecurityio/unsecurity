lazy val root = (project in file("."))
  .aggregate(core, auth0)

lazy val core = project
lazy val auth0 = project.dependsOn(core)
