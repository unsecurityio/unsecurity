name := "unsecurity-auth0"

scalacOptions := Seq(
  "-deprecation",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

val auth0Version     = "1.21.0"
val jwksVersion      = "0.12.0"
val jwtVersion       = "3.10.3"
val scalaTestVersion = "3.2.0"

libraryDependencies := Seq(
  "com.auth0"     % "auth0"      % auth0Version,
  "com.auth0"     % "jwks-rsa"   % jwksVersion,
  "com.auth0"     % "java-jwt"   % jwtVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)
