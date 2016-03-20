lazy val root = (project in file(".")).
  settings(
    organization := "com.leeriggins",
    name := "aws-sdk-apis-parser",
    version := "0.0.1",
    scalaVersion := "2.11.8"
  )
  
libraryDependencies ++= Seq(
  "org.webjars.bower" % "aws-sdk-js" % "2.1.23",
  "org.json4s" %% "json4s-jackson" % "3.3.0"
)
