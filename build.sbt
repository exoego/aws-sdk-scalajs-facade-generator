lazy val `aws-sdk-scalajs-generator` = (project in file(".")).settings(
  organization := "net.exoego",
  name := "aws-sdk-scalajs-facade-generator",
  version := "0.0.2",
  scalaVersion := "2.13.0"
)

libraryDependencies ++= Seq(
  "org.json4s"    %% "json4s-jackson" % "3.6.6",
  "org.scalatest" %% "scalatest"      % "3.0.8" % "test"
)
