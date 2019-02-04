import sbt.Keys.libraryDependencies

organization := "io.unsecurity"

name := "unsecurity-core"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.8"

scalacOptions := Seq("-deprecation", "-Ypartial-unification", "-language:higherKinds", "-Ywarn-value-discard")

val circeVersion      = "0.10.1"
val http4sVersion     = "0.20.0-M5"
val directivesVersion = "0.20.0-M5-1"

libraryDependencies := Seq(
  "io.circe"           %% "circe-parser"        % circeVersion,
  "org.http4s"         %% "http4s-blaze-client" % http4sVersion,
  "org.http4s"         %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"         %% "http4s-circe"        % http4sVersion,
  "no.scalabin.http4s" %% "http4s-directives"   % directivesVersion,
  "org.scalatest"      %% "scalatest"           % "3.0.5" % Test,
  "ch.qos.logback"     % "logback-classic"      % "1.2.3" % Test
)
