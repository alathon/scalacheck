sourceDirectory := file("dummy source directory")

lazy val versionNumber = "1.13.0"

lazy val isRelease = true

lazy val travisCommit = Option(System.getenv().get("TRAVIS_COMMIT"))

lazy val sharedSettings = MimaSettings.settings ++ Seq(

  name := "scalacheck",

  version := {
    val suffix =
      if (isRelease) ""
      else travisCommit.map("-" + _.take(7)).getOrElse("") + "-SNAPSHOT"
    versionNumber + suffix
  },

  isSnapshot := !isRelease,

  organization := "org.scalacheck",

  licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),

  homepage := Some(url("http://www.scalacheck.org")),

  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    username, password
  )).toSeq,

  scalaVersion := "2.11.7",

  crossScalaVersions := Seq("2.10.6", "2.11.7", "2.12.0-M3"),

  unmanagedSourceDirectories in Compile += (baseDirectory in LocalRootProject).value / "src" / "main" / "scala",

  unmanagedSourceDirectories in Test += (baseDirectory in LocalRootProject).value / "src" / "test" / "scala",

  resolvers += "sonatype" at "https://oss.sonatype.org/content/repositories/releases",
  resolvers += "com.todesking" at "http://todesking.github.io/mvn",

  javacOptions += "-Xmx1024M",

  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),

  scalacOptions in (Compile,doc) += "-Xfatal-warnings",

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    val (name, path) = if (isSnapshot.value) ("snapshots", "content/repositories/snapshots")
                       else ("releases", "service/local/staging/deploy/maven2")
    Some(name at nexus + path)
  },

  publishMavenStyle := true,

  // Travis should only publish snapshots
  publishArtifact := !(isRelease && travisCommit.isDefined),

  publishArtifact in Test := false,

  pomIncludeRepository := { _ => false },

  pomExtra := {
    <scm>
      <url>https://github.com/rickynils/scalacheck</url>
      <connection>scm:git:git@github.com:rickynils/scalacheck.git</connection>
    </scm>
    <developers>
      <developer>
        <id>rickynils</id>
        <name>Rickard Nilsson</name>
      </developer>
    </developers>
  }
)

import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact

lazy val js = project.in(file("js"))
  .settings(sharedSettings: _*)
  .settings(
    scalaJSStage in Global := FastOptStage,
    previousArtifact := None,
    libraryDependencies += "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
  )
  .enablePlugins(ScalaJSPlugin)

lazy val jvm = project.in(file("jvm"))
  .settings(sharedSettings: _*)
  .settings(
    previousArtifact := None,
    libraryDependencies += "org.scala-sbt" %  "test-interface" % "1.0",
    libraryDependencies += "com.todesking" %% "scala-pp" % "0.0.4"
  )
