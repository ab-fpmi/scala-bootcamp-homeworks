import Dependencies._

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / name := "scala-bootcamp-homeworks"
ThisBuild / organization := "ab-fpmi"
ThisBuild / version := "1.0"

lazy val root = (project in file(".")).settings(
  name := "homeworks",
  libraryDependencies ++= Seq(
    scalaTest % Test
  )
)
