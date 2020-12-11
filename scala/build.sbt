// ---------------------------------------------------------------------------
// Commands

addCommandAlias("ci", ";project root ;scalafmtCheckAll ;compile ;test ;package")

// ---------------------------------------------------------------------------
// Dependencies

val MonixVersion = "3.3.0"
val CatsVersion = "2.3.0"
val CatsEffectVersion = "2.3.0"
val SimulacrumVersion = "1.0.1"
val MacroParadiseVersion = "2.1.1"
val ScalaTestVersion = "3.2.3"
val ScalaTestPlusVersion = "3.2.2.0"
val ScalaCheckVersion = "1.15.1"
val KindProjectorVersion = "0.11.2"
val BetterMonadicForVersion = "0.3.1"
val SilencerVersion = "1.7.0"
val KittensVersion = "2.2.1"
val FS2Version = "2.4.6"
val CatsParseVersion = "0.1.0"
val EnumeratumVersion = "1.6.1"

/**
  * Defines common plugins between all projects.
  */
def defaultPlugins: Project => Project =
  pr => {
    pr.enablePlugins(AutomateHeaderPlugin)
      .enablePlugins(GitBranchPrompt)
  }

lazy val sharedSettings = Seq(
  organization := "io.github.jamesfielder",
  scalaVersion := "2.13.4",

  headerLicense := Some(
    HeaderLicense.Custom(
      s"""|Copyright (c) 2020 James Fielder.
          |All rights reserved.
          |""".stripMargin
    )
  ),
  //
  // Turning off fatal warnings for doc generation
  scalacOptions.in(Compile, doc) ~= filterConsoleScalacOptions,
  scalacOptions += "-Ymacro-annotations",

  addCompilerPlugin("org.typelevel" % "kind-projector" % KindProjectorVersion cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % BetterMonadicForVersion),

  // ---------------------------------------------------------------------------
  // Options for testing

  logBuffered in Test := false,
  logBuffered in IntegrationTest := false,
)

/**
  * Shared configuration across all sub-projects.
  */
def defaultProjectConfiguration(pr: Project) = {
  pr.configure(defaultPlugins)
    .settings(sharedSettings)
}

lazy val root = project
  .in(file("."))
  .aggregate(aoc)
  .dependsOn(aoc)
  .configure(defaultProjectConfiguration)
  .settings(
    mainClass in (Compile, run) := Some("advent.of.code.2020.scala.Main")
  )

lazy val aoc = project
  .in(file("aoc"))
  .configure(defaultProjectConfiguration)
  .settings(
    name := "advent-of-code-2020-scala-aoc",
    libraryDependencies ++= Seq(
      "io.monix" %% "monix" % MonixVersion,
      "org.typelevel" %% "simulacrum" % SimulacrumVersion % Provided,
      "org.typelevel" %% "cats-core" % CatsVersion,
      "org.typelevel" %% "cats-effect" % CatsEffectVersion,
      "co.fs2" %% "fs2-core" % FS2Version,
      "co.fs2" %% "fs2-io" % FS2Version,
      "co.fs2" %% "fs2-reactive-streams" % FS2Version,
      "co.fs2" %% "fs2-experimental" % FS2Version,
      "org.typelevel" %% "cats-parse" % CatsParseVersion,
      "com.beachape" %% "enumeratum" % EnumeratumVersion,
      "com.beachape" %% "enumeratum-cats" % EnumeratumVersion,
      "io.scalaland" %% "catnip" % "1.1.1",
      // For testing
      "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
      "org.scalatestplus" %% "scalacheck-1-14" % ScalaTestPlusVersion % Test,
      "org.scalacheck" %% "scalacheck" % ScalaCheckVersion % Test,
      "org.typelevel" %% "cats-laws" % CatsVersion % Test,
      "org.typelevel" %% "cats-effect-laws" % CatsEffectVersion % Test,
      "org.scalameta" %% "munit" % "0.7.19" % Test,
      "org.scalameta" %% "munit-scalacheck" % "0.7.19" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

// Reloads build.sbt changes whenever detected
Global / onChangedBuildSource := ReloadOnSourceChanges
