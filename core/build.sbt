name := "unsecurity-core"

scalacOptions := Seq(
  "-deprecation",
  "-language:higherKinds",
  "-Ywarn-value-discard"
)

scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, v)) if v <= 12 => Seq("-Ypartial-unification")
  case _                       => Seq.empty
})

val http4sVersion     = "0.21.0-RC1"
val fs2Version        = "2.2.1"
val circeVersion      = "0.13.0-RC1"
val directivesVersion = "0.17.0"
val shapelessVersion  = "2.3.3"
val scalaTestVersion  = "3.1.0"
val logbackVersion    = "1.2.3"

libraryDependencies := Seq(
  "io.circe"           %% "circe-parser"        % circeVersion,
  "org.http4s"         %% "http4s-blaze-client" % http4sVersion,
  "org.http4s"         %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"         %% "http4s-circe"        % http4sVersion,
  "co.fs2"             %% "fs2-core"            % fs2Version,
  "co.fs2"             %% "fs2-io"              % fs2Version,
  "no.scalabin.http4s" %% "http4s-directives"   % directivesVersion,
  "com.chuusai"        %% "shapeless"           % shapelessVersion,
  "org.scalatest"      %% "scalatest"           % scalaTestVersion % Test,
  "ch.qos.logback"     % "logback-classic"      % logbackVersion % Test
)
