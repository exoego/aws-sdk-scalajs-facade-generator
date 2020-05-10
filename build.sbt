lazy val `aws-sdk-scalajs-generator` = (project in file(".")).settings(
  organization := "net.exoego",
  name := "aws-sdk-scalajs-facade-generator",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:implicitConversions",
    "-Ywarn-unused:_",
    "-Ywarn-value-discard",
    "-Ywarn-numeric-widen",
    "-Ywarn-extra-implicit",
    "-Ywarn-self-implicit",
    "-Xlint:_",
    "-Xfatal-warnings"
  ),
  version := "0.0.2",
  scalaVersion := "2.13.0"
)

libraryDependencies ++= Seq(
  "org.json4s"    %% "json4s-jackson" % "3.6.8",
  "org.scalatest" %% "scalatest"      % "3.1.2" % "test"
)
