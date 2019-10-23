disablePlugins(aether.AetherPlugin)
enablePlugins(aether.SignedAetherPlugin)

overridePublishSignedSettings
overridePublishLocalSettings

publishTo in ThisBuild := {
  if (isSnapshot.value) {
    Some(Opts.resolver.sonatypeSnapshots)
  } else {
    Some(Opts.resolver.sonatypeStaging)
  }
}

pomIncludeRepository in ThisBuild := { x =>
  false
}

packageOptions in ThisBuild += {
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

credentials in ThisBuild ++= Seq(
  Credentials(Path.userHome / ".sbt" / "sonatype_credential"),
)

homepage in ThisBuild := Some(url("https://unsecurity.io"))

startYear in ThisBuild := Some(2019)

licenses in ThisBuild := Seq(
  "MIT" -> url("https://raw.githubusercontent.com/unsecurityio/unsecurity/master/LICENSE")
)

publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

pomIncludeRepository in ThisBuild := { _ =>
  false
}

releaseCrossBuild in ThisBuild := true

releasePublishArtifactsAction in ThisBuild := PgpKeys.publishSigned.value

scmInfo in ThisBuild := Some(
  ScmInfo(
    new URL("https://github.com/unsecurityio/unsecurity"),
    "scm:git:git@github.com:unsecurityio/unsecurity.git",
    Some("scm:git:git@github.com:unsecurityio/unsecurity.git")
  ))

developers in ThisBuild ++= List(
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
