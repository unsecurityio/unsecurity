disablePlugins(aether.AetherPlugin)
enablePlugins(aether.SignedAetherPlugin)

overridePublishSignedSettings
overridePublishLocalSettings

publishTo := {
  if (isSnapshot.value) {
    Some(Opts.resolver.sonatypeSnapshots)
  } else {
    Some(Opts.resolver.sonatypeStaging)
  }
}

pomIncludeRepository := { x =>
  false
}

packageOptions += {
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

credentials ++= Seq(
  Credentials(Path.userHome / ".sbt" / "sonatype_credential"),
)

homepage := Some(url("https://unsecurity.io"))

startYear := Some(2019)

licenses := Seq(
  "MIT" -> url("https://raw.githubusercontent.com/unsecurityio/unsecurity/master/LICENSE")
)

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ =>
  false
}

releaseCrossBuild := true

releasePublishArtifactsAction := PgpKeys.publishSigned.value

scmInfo := Some(
  ScmInfo(
    new URL("https://github.com/unsecurityio/unsecurity"),
    "scm:git:git@github.com:unsecurityio/unsecurity.git",
    Some("scm:git:git@github.com:unsecurityio/unsecurity.git")
  ))

developers ++= List(
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
