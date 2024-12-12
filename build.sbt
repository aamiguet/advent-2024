ThisBuild / scalaVersion := "3.6.2"
ThisBuild / scalacOptions ++= Seq(
  "-encoding",
  "utf-8",
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wsafe-init",
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-2024",
    version := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    ),
  )
