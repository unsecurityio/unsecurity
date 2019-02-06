import sbt.Keys.libraryDependencies

name := "unsecurity-auth0"

scalacOptions := Seq(
  "-deprecation",
  "-Ypartial-unification",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

libraryDependencies := Seq(
  "com.squareup.okhttp3" % "okhttp"           % "3.9.0",
  "com.auth0"            % "auth0"            % "1.5.1",
  "com.auth0"            % "jwks-rsa"         % "0.3.0",
  "com.auth0"            % "java-jwt"         % "3.3.0",
  "org.scalatest"        %% "scalatest"       % "3.0.5" % Test
)
