import sbt._

object Dependencies {
  val scalaTestVersion = "3.2.5"
  val scalatestScalacheckVersion = "3.2.5.0"
  val catsVersion = "2.4.2"
  val circeVersion = "0.13.0"
  val scalajVersion = "2.4.2"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  lazy val scalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % scalatestScalacheckVersion
  lazy val cats = "org.typelevel" %% "cats-core" % catsVersion
  lazy val scalaj = "org.scalaj" %% "scalaj-http" % "2.4.2"

  lazy val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "io.circe" %% "circe-optics" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )
}
