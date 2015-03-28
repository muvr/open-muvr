import Dependencies._

Build.Settings.project

name := "exercise-query"

libraryDependencies ++= Seq(
  // Core Akka
  akka.actor,
  akka.cluster,
  akka.contrib,
  akka.persistence,
  akka.streams.core,
  scalaz.core,
  slf4j.slf4j_simple,
  // For improving future based chaining
  async,
  // Testing
  scalatest % "test",
  scalacheck % "test",
  akka.testkit % "test",
  akka.streams.testkit % "test"
)
