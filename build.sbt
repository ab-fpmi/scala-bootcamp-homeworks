import Dependencies._

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / name := "scala-bootcamp-homeworks"
ThisBuild / organization := "ab-fpmi"
ThisBuild / version := "1.0"

lazy val root = (project in file(".")).settings(
  name := "homeworks",
  libraryDependencies ++= Seq(
    cats,
    scalaTest % Test,
    scalaCheck % Test,
    scalaj
  ) ++ circe
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

scalacOptions ++= Seq("-Ymacro-annotations")
