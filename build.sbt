import Dependencies._

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / name := "scala-bootcamp-homeworks"
ThisBuild / organization := "ab-fpmi"
ThisBuild / version := "1.0"

lazy val root = (project in file(".")).settings(
  name := "homeworks",
  libraryDependencies ++= Seq(
    cats,
    catsEffect,
    scalajHttp,
    atto,
    slf4j,
    scalaTest % Test,
    scalaCheck % Test,
  ) ++ circe ++ http4s
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

scalacOptions ++= Seq("-Ymacro-annotations")
