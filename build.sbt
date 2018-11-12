lazy val `aws-sdk-scalajs-generator` = (project in file(".")).
  settings(
    organization := "net.exoego",
    name := "aws-sdk-scalajs-generator",
    version := "0.0.1",
    scalaVersion := "2.12.7"
  )
  
libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.6.2",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
