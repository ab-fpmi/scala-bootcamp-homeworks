import sbt._

object Dependencies {
  val scalaTestVersion = "3.1.0.0-RC2"

  lazy val scalaTest = "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion
}
