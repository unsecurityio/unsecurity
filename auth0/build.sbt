name := "unsecurity-auth0"

scalacOptions := Seq(
  "-deprecation",
  "-Ypartial-unification",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

libraryDependencies := Seq(
  "com.auth0"            % "auth0"            % "1.14.3",
  "com.auth0"            % "jwks-rsa"         % "0.9.0",
  "com.auth0"            % "java-jwt"         % "3.8.3",
  "org.scalatest"        %% "scalatest"       % "3.0.8" % Test
)
