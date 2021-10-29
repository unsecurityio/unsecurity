name := "unsecurity-core"

scalacOptions := Seq(
  "-deprecation",
  "-language:higherKinds",
  "-source:3.0-migration",
  "-new-syntax",
  "-rewrite",
  "-explain-types"
)

testFrameworks += new TestFramework("munit.Framework")

val http4sVersion     = "0.23.4"
val directivesVersion = "0.23.1"
val fs2Version        = "3.1.0"
val circeVersion      = "0.14.1"
val scalaTestVersion  = "3.2.9"
val logbackVersion    = "1.2.6"
val munitVersion      = "0.7.29"
val munitCatsVersion  = "1.0.5"
val commonsVersion    = "1.15"

libraryDependencies := Seq(
  "io.circe"           %% "circe-parser"        % circeVersion,
  "org.http4s"         %% "http4s-blaze-client" % http4sVersion,
  "org.http4s"         %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"         %% "http4s-dsl"          % http4sVersion,
  "org.http4s"         %% "http4s-circe"        % http4sVersion,
  "co.fs2"             %% "fs2-core"            % fs2Version,
  "co.fs2"             %% "fs2-io"              % fs2Version,
  "no.scalabin.http4s" %% "http4s-directives"   % directivesVersion,
  "org.scalatest"      %% "scalatest"           % scalaTestVersion % Test,
  "org.scalameta"      %% "munit"               % munitVersion % Test,
  "org.typelevel"      %% "munit-cats-effect-2" % munitCatsVersion % Test,
  "commons-codec"      % "commons-codec"        % commonsVersion,
  "ch.qos.logback"     % "logback-classic"      % logbackVersion % Test
)
