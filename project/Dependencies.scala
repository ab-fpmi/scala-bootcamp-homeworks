import sbt._

object Dependencies {
  val scalaTestVersion = "3.2.5"
  val scalatestScalacheckVersion = "3.2.5.0"
  val catsVersion = "2.4.2"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  lazy val scalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % scalatestScalacheckVersion
  lazy val cats = "org.typelevel" %% "cats-core" % catsVersion
}
