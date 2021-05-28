
lazy val origami = project.in(file("."))
  .enablePlugins(GitBranchPrompt, SitePlugin, GhpagesPlugin)
  .settings(buildSettings)
  .settings(publishSettings)
  .aggregate(core, lib, fs2)

lazy val core = project.in(file("core"))
  .settings(moduleSettings("core"))
  .settings(buildSettings)
  .settings(publishSettings)

lazy val lib = project.in(file("lib"))
  .settings(moduleSettings("lib"))
  .settings(buildSettings)
  .settings(publishSettings)
  .dependsOn(core, core % "test->test")

lazy val fs2 = project.in(file("fs2"))
  .settings(moduleSettings("fs2"))
  .settings(buildSettings)
  .settings(publishSettings)
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.4")
  .settings(libraryDependencies += "co.fs2" %% "fs2-core" % "2.4.4")
  .dependsOn(core, core % "test->test")

def moduleSettings(moduleName: String) = Seq(
  organization := "org.atnos",
  name := "origami-"+moduleName
)

lazy val buildInfoSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "org.atnos.origami"
)

def buildSettings = Seq(
  scalaVersion := "2.13.2",
  crossScalaVersions := Seq("2.12.14", scalaVersion.value),
  scalacOptions ++= commonScalacOptions,
  scalacOptions in (Compile, doc) ++= (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
) ++ warnUnusedImport ++ prompt

lazy val publishSettings =
  Seq(
  publishTo in Global := sonatypePublishToBundle.value,
  sonatypeProfileName := "org.atnos",
  publishMavenStyle := true,
  publishArtifact in Test := false,
  homepage := Some(url("https://github.com/atnos-org/origami")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/atnos-org/origami"), "scm:git:git@github.com:atnos-org/origami.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://atnos.org/origami/api/")),
  pomExtra := (
    <developers>
      <developer>
        <id>etorreborre</id>
        <name>Eric Torreborre</name>
        <url>https://github.com/etorreborre/</url>
      </developer>
    </developers>
    )
) ++ credentialSettings ++ sharedPublishSettings ++ Sonatype.projectSettings

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

lazy val sharedPublishSettings = Seq(
  publishTo := sonatypePublishToBundle.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  sonatypeProfileName := "org.atnos",
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
) ++ Sonatype.projectSettings ++ userGuideSettings

lazy val userGuideSettings =
  Seq(
    ghpagesNoJekyll := false,
    siteSourceDirectory in makeSite := target.value / "specs2-reports" / "site",
    includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js",
    git.remoteRepo := "git@github.com:atnos-org/origami.git"
  )

lazy val warnUnusedImport = Seq(
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)

lazy val prompt = shellPrompt in ThisBuild := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "origami") "" else name) + "> "
}
