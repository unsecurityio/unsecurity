import sbt.Keys.libraryDependencies

organization := "io.unsecurity"

name := "unsecurity-auth0"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.8"

scalacOptions := Seq(
  "-deprecation",
  "-Ypartial-unification",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

libraryDependencies := Seq(
//  "io.unsecurity"        %% "unsecurity-core" % "0.1-SNAPSHOT",
  "com.squareup.okhttp3" % "okhttp"           % "3.9.0",
  "com.auth0"            % "auth0"            % "1.5.1",
  "com.auth0"            % "jwks-rsa"         % "0.3.0",
  "com.auth0"            % "java-jwt"         % "3.3.0",
  "org.scalatest"        %% "scalatest"       % "3.0.5" % Test
)
