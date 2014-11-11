version in ThisBuild := "0.1-SNAPSHOT"

organization in ThisBuild := "com.github.aloiscochard"

scalaVersion in ThisBuild := "2.11.4"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-feature")

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")
