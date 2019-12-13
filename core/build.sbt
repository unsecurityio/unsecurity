name := "unsecurity-core"

scalacOptions := Seq(
  "-deprecation",
  "-Ypartial-unification",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

val http4sVersion     = "0.20.15"
val circeVersion      = "0.12.2"
val directivesVersion = "0.16.0"
val shapelessVersion  = "2.3.3"
val scalaTestVersion  = "3.1.0"
val logbackVersion    = "1.2.3"

libraryDependencies := Seq(
  "io.circe"           %% "circe-parser"        % circeVersion,
  "org.http4s"         %% "http4s-blaze-client" % http4sVersion,
  "org.http4s"         %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"         %% "http4s-circe"        % http4sVersion,
  "no.scalabin.http4s" %% "http4s-directives"   % directivesVersion,
  "com.chuusai"        %% "shapeless"           % shapelessVersion,
  "org.scalatest"      %% "scalatest"           % scalaTestVersion % Test,
  "ch.qos.logback"     % "logback-classic"      % logbackVersion % Test
)
