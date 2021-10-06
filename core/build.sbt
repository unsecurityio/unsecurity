name := "unsecurity-core"

scalacOptions := Seq(
  "-deprecation",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

testFrameworks += new TestFramework("munit.Framework")

val http4sVersion     = "0.21.30"
val directivesVersion = "0.21.24"
val fs2Version        = "2.5.9"
val circeVersion      = "0.14.1"
val shapelessVersion  = "2.3.7"
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
  "com.chuusai"        %% "shapeless"           % shapelessVersion,
  "org.scalatest"      %% "scalatest"           % scalaTestVersion % Test,
  "org.scalameta"      %% "munit"               % munitVersion % Test,
  "org.typelevel"      %% "munit-cats-effect-2" % munitCatsVersion % Test,
  "commons-codec"      % "commons-codec"        % commonsVersion,
  "ch.qos.logback"     % "logback-classic"      % logbackVersion % Test
)
