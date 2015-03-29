import Dependencies._

Build.Settings.project

name := "exercise-query"

libraryDependencies ++= Seq(
  // Core Akka
  akka.actor,
  scalaz.core,
  slf4j.slf4j_simple,
  // For improving future based chaining
  async,
  // Testing
  scalatest % "test",
  scalacheck % "test"
)
