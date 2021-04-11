import sbt._

object Dependencies {
  val scalaTestVersion = "3.2.6"
  val scalatestScalacheckVersion = "3.2.6.0"
  val catsVersion = "2.4.2"
  val catsEffectVersion = "2.4.1"
  val circeVersion = "0.13.0"
  val scalajHttpVersion = "2.4.2"
  val attoVersion = "0.9.3"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  lazy val scalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % scalatestScalacheckVersion
  lazy val cats = "org.typelevel" %% "cats-core" % catsVersion
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
  lazy val scalajHttp = "org.scalaj" %% "scalaj-http" % scalajHttpVersion
  lazy val atto = "org.tpolecat" %% "atto-core" % attoVersion

  lazy val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "io.circe" %% "circe-optics" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )
}
