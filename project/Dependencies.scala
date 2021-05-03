import sbt._

object Dependencies {
  val scalaTestVersion = "3.2.8"
  val scalatestScalacheckVersion = "3.2.8.0"
  val catsVersion = "2.6.0"
  val catsEffectVersion = "2.5.0"
  val circeVersion = "0.13.0"
  val scalajHttpVersion = "2.4.2"
  val attoVersion = "0.9.3"
  val http4sVersion = "0.21.22"
  val http4sJdkHttpClientVersion = "0.3.6"
  val slf4jVersion = "1.7.30"
  val akkaVersion = "2.6.14"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  lazy val scalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % scalatestScalacheckVersion
  lazy val cats = "org.typelevel" %% "cats-core" % catsVersion
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
  lazy val scalajHttp = "org.scalaj" %% "scalaj-http" % scalajHttpVersion
  lazy val atto = "org.tpolecat" %% "atto-core" % attoVersion
  lazy val slf4j = "org.slf4j" % "slf4j-simple" % slf4jVersion
  lazy val akka =   "com.typesafe.akka" %% "akka-actor" % akkaVersion

  lazy val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "io.circe" %% "circe-optics" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )

  lazy val http4s = Seq(
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "org.http4s" %% "http4s-jdk-http-client" % http4sJdkHttpClientVersion
  )
}
