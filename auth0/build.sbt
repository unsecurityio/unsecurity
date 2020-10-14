name := "unsecurity-auth0"

scalacOptions := Seq(
  "-deprecation",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

val auth0Version     = "1.22.1"
val jwksVersion      = "0.14.0"
val jwtVersion       = "3.11.0"
val scalaTestVersion = "3.2.2"

libraryDependencies := Seq(
  "com.auth0"     % "auth0"      % auth0Version,
  "com.auth0"     % "jwks-rsa"   % jwksVersion,
  "com.auth0"     % "java-jwt"   % jwtVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)
