import Dependencies._

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / name := "scala-bootcamp-homeworks"
ThisBuild / organization := "ab-fpmi"
ThisBuild / version := "1.0"

lazy val root = (project in file(".")).settings(
  name := "homeworks",
  libraryDependencies ++= Seq(
    scalaTest % Test,
    cats
  )
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)
