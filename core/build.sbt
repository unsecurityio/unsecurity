name := "unsecurity-core"

scalacOptions := Seq("-deprecation", "-Ypartial-unification", "-language:higherKinds", "-Ywarn-value-discard")

val http4sVersion     = "0.20.11"
val circeVersion      = "0.12.2"
val directivesVersion = "0.13.0"

libraryDependencies := Seq(
  "io.circe"           %% "circe-parser"        % circeVersion,
  "org.http4s"         %% "http4s-blaze-client" % http4sVersion,
  "org.http4s"         %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"         %% "http4s-circe"        % http4sVersion,
  "no.scalabin.http4s" %% "http4s-directives"   % directivesVersion,
  "com.chuusai"        %% "shapeless"           % "2.3.3",
  "org.scalatest"      %% "scalatest"           % "3.0.8" % Test,
  "ch.qos.logback"     % "logback-classic"      % "1.2.3" % Test
)
