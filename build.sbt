ThisBuild / scalaVersion := "3.6.2-RC3"
ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf-8",
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
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  )
