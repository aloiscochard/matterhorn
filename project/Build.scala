import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

import pl.project13.scala.sbt.SbtJmh._

object HelloBuild extends Build {
    lazy val root = Project(
      id = "root",
      base = file(".")
    ).aggregate(core, stm, machines, scalaz, bench)

    lazy val core = Project(
      id = "core",
      base = file("core")).settings(
      name := "matterhorn-core",
      libraryDependencies ++= Seq(
        "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
      )
    )

    lazy val stm = Project(
      id = "stm",
      base = file("stm")).settings(
      name := "matterhorn-stm",
      libraryDependencies ++= Seq(
        "org.scala-stm" %% "scala-stm" % "0.7"
      )
    ).dependsOn(core, core % "test->test")

    lazy val machines = Project(
      id = "machines",
      base = file("machines")).settings(
      name := "matterhorn-machines"
    ).dependsOn(core, core % "test->test")

    lazy val scalaz = Project(
      id = "scalaz",
      base = file("scalaz")).settings(
      name := "matterhorn-scalaz",
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.1.0"
      )
    ).dependsOn(core, stm, core % "test->test")

    lazy val bench = Project(
      id = "bench",
      base = file("bench")
    ).dependsOn(core, core % "compile->test")
      .settings(jmhSettings:_*)
      .settings(assemblySettings: _*)
      .settings(
        mainClass in assembly := Some("matterhorn.bench.FilesProfiling")
      )
}
