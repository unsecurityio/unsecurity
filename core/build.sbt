name := "unsecurity-core"

scalacOptions := Seq(
  "-deprecation",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

testFrameworks += new TestFramework("munit.Framework")

val http4sVersion     = "0.21.16"
val directivesVersion = "0.21.5"
val fs2Version        = "2.4.4"
val circeVersion      = "0.13.0"
val shapelessVersion  = "2.3.3"
val scalaTestVersion  = "3.2.2"
val logbackVersion    = "1.2.3"
val munitVersion      = "0.7.14"
val munitCatsVersion  = "0.4-759809d"

libraryDependencies := Seq(
  "io.circe"           %% "circe-parser"        % circeVersion,
  "org.http4s"         %% "http4s-blaze-client" % http4sVersion,
  "org.http4s"         %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"         %% "http4s-dsl"          % http4sVersion,
  "org.http4s"         %% "http4s-circe"        % http4sVersion,
  "co.fs2"             %% "fs2-core"            % fs2Version,
  "co.fs2"             %% "fs2-io"              % fs2Version,
  "no.scalabin.http4s" %% "http4s-directives"   % directivesVersion,
  "com.chuusai"        %% "shapeless"           % shapelessVersion,
  "org.scalatest"      %% "scalatest"           % scalaTestVersion % Test,
  "org.scalameta"      %% "munit"               % munitVersion     % Test,
  "org.typelevel"      %% "munit-cats-effect"   % munitCatsVersion % Test,
  "ch.qos.logback"     % "logback-classic"      % logbackVersion   % Test
)
