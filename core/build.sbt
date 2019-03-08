import sbt.Keys.libraryDependencies

name := "unsecurity-core"

scalacOptions := Seq("-deprecation", "-Ypartial-unification", "-language:higherKinds", "-Ywarn-value-discard")

val circeVersion      = "0.11.1"
val http4sVersion     = "0.20.0-M6"
val directivesVersion = "0.20.0-M6-1"

libraryDependencies := Seq(
  "io.circe"           %% "circe-parser"        % circeVersion,
  "org.http4s"         %% "http4s-blaze-client" % http4sVersion,
  "org.http4s"         %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"         %% "http4s-circe"        % http4sVersion,
  "no.scalabin.http4s" %% "http4s-directives"   % directivesVersion,
  "com.chuusai"        %% "shapeless"           % "2.3.3",
  "org.scalatest"      %% "scalatest"           % "3.0.6" % Test,
  "ch.qos.logback"     % "logback-classic"      % "1.2.3" % Test
)
