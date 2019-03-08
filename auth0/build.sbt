import sbt.Keys.libraryDependencies

name := "unsecurity-auth0"

scalacOptions := Seq(
  "-deprecation",
  "-Ypartial-unification",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

libraryDependencies := Seq(
  "com.auth0"            % "auth0"            % "1.10.0",
  "com.auth0"            % "jwks-rsa"         % "0.7.0",
  "com.auth0"            % "java-jwt"         % "3.7.0",
  "org.scalatest"        %% "scalatest"       % "3.0.6" % Test
)
