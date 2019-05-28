disablePlugins(aether.AetherPlugin)
enablePlugins(aether.SignedAetherPlugin)

overridePublishSignedSettings
overridePublishLocalSettings

ThisBuild / scmInfo := Some(
  ScmInfo(
    new URL("https://github.com/unsecurityio/unsecurity"),
    "scm:git:git@github.com:unsecurityio/unsecurity.git",
    Some("scm:git:git@github.com:unsecurityio/unsecurity.git")
  ))

ThisBuild / developers := List(
  Developer(
    id = "eirikm",
    name = "Eirik Meland",
    email = "eirik.meland@gmail.com",
    url = new URL("http://twitter.com/eirikme")
  ),
  Developer(
    id = "kaarenilsen",
    name = "Kaare Nilsen",
    email = "kaare.nilsen@gmail.com",
    url = new URL("http://twitter.com/kaarenilsen")
  ),

)

ThisBuild / licenses := Seq("MIT" -> url("https://raw.githubusercontent.com/unsecurityio/unsecurity/master/LICENSE"))
ThisBuild / homepage := Some(url("https://unsecurity.io"))

ThisBuild / pomIncludeRepository := { x => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

ThisBuild / packageOptions += {
  val title  = name.value
  val ver    = version.value
  val vendor = organization.value

  Package.ManifestAttributes(
    "Created-By"               -> "Scala Build Tool",
    "Built-By"                 -> System.getProperty("user.name"),
    "Build-Jdk"                -> System.getProperty("java.version"),
    "Specification-Title"      -> title,
    "Specification-Version"    -> ver,
    "Specification-Vendor"     -> vendor,
    "Implementation-Title"     -> title,
    "Implementation-Version"   -> ver,
    "Implementation-Vendor-Id" -> vendor,
    "Implementation-Vendor"    -> vendor
  )
}

ThisBuild / credentials ++= Seq(
  Credentials(Path.userHome / ".sbt" / "sonatype_credential")
)

ThisBuild / startYear := Some(2019)

ThisBuild / publishArtifact in Test := false

ThisBuild / releaseCrossBuild := true

ThisBuild / releasePublishArtifactsAction := PgpKeys.publishSigned.value


