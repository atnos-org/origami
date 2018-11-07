enablePlugins(GhpagesPlugin)
enablePlugins(SitePlugin)
enablePlugins(BuildInfoPlugin)

lazy val origami = project.in(file("."))
  .settings(buildSettings)
  .settings(publishSettings)
  .aggregate(core, lib, scalaz, fs2)

lazy val core = project.in(file("core"))
  .settings(moduleSettings("core"))
  .settings(publishSettings)
  .settings(buildSettings)

lazy val lib = project.in(file("lib"))
  .settings(moduleSettings("lib"))
  .settings(buildSettings)
  .settings(publishSettings)
  .dependsOn(core, core % "test->test")

lazy val scalaz = project.in(file("scalaz"))
  .settings(moduleSettings("scalaz"))
  .settings(buildSettings)
  .settings(publishSettings)
  .settings(libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.8.6a")
  .dependsOn(core, core % "test->test")

lazy val fs2 = project.in(file("fs2"))
  .settings(moduleSettings("fs2"))
  .settings(buildSettings)
  .settings(publishSettings)
  .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0")
  .settings(libraryDependencies += "co.fs2" %% "fs2-core" % "1.0.0")
  .dependsOn(core, core % "test->test")

def moduleSettings(moduleName: String) = Seq(
  organization := "org.atnos",
  name := "origami-"+moduleName
)

lazy val notesSettings = Seq(
  ghreleaseRepoOrg := "atnos-org",
  ghreleaseRepoName := "origami",
  ghreleaseTitle := { tagName: TagName => s"origami ${tagName.replace("ORIGAMI-", "")}" },
  ghreleaseIsPrerelease := { tagName: TagName => false },
  ghreleaseNotes := { tagName: TagName =>
    // find the corresponding release notes
    val notesFilePath = s"notes/${tagName.replace("ORIGAMI-", "")}.markdown"
    try scala.io.Source.fromFile(notesFilePath).mkString
    catch { case t: Throwable => throw new Exception(s"$notesFilePath not found") }
  },
  // just upload the notes
  ghreleaseAssets := Seq()
)

lazy val buildInfoSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "org.atnos.origami"
)

def buildSettings = Seq(
  scalaVersion := "2.12.7",
  crossScalaVersions := Seq("2.11.12", "2.12.7"),
  scalacOptions ++= commonScalacOptions,
  scalacOptions in (Compile, doc) ++= (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
) ++ warnUnusedImport ++ prompt

lazy val publishSettings =
  Seq(
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
) ++ credentialSettings ++ sharedPublishSettings

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Ypartial-unification"
)

lazy val sharedPublishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := Option("Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
  sonatypeProfileName := "org.atnos"
) ++ userGuideSettings

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
