name := "unsecurity-auth0"

scalacOptions := Seq(
  "-deprecation",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, v)) if v <= 12 => Seq("-Ypartial-unification")
  case _                       => Seq.empty
})

val auth0Version     = "1.15.0"
val jwksVersion      = "0.9.0"
val jwtVersion       = "3.8.3"
val scalaTestVersion = "3.0.8"

libraryDependencies := Seq(
  "com.auth0"     % "auth0"      % auth0Version,
  "com.auth0"     % "jwks-rsa"   % jwksVersion,
  "com.auth0"     % "java-jwt"   % jwtVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)
