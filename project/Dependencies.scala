import sbt._

object Dependencies {
  val scalaTestVersion = "3.1.0.0-RC2"
  val catsVersion = "2.1.1"

  lazy val scalaTest = "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion
  lazy val cats = "org.typelevel" %% "cats-core" % catsVersion
}
