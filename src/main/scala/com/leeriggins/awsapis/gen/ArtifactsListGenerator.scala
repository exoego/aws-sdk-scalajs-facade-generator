package com.leeriggins.awsapis.gen

import com.leeriggins.awsapis.models.Api

import java.io.{FileWriter, PrintWriter}
import java.nio.file.Paths

object ArtifactsListGenerator {
  private val manuallyGeneratedService = Seq(
    "cloudfrontsigner"
  )

  def generate(extractApis: Seq[Api]): Unit = {
    val file   = Paths.get("../aws-sdk-scalajs-facade/ARTIFACTS.md")
    val writer = new PrintWriter(new FileWriter(file.toFile))

    val types = (extractApis.map(_.serviceClassName.toLowerCase) ++ manuallyGeneratedService)
      .map { lowerName =>
        s"""    "net.exoego" %%% "aws-sdk-scalajs-facade-$lowerName" % awsSdkScalajsFacadeVersion,""".stripMargin
      }
      .sorted
      .mkString("\n")

    try {
      writer.append(s"""You can include all-in-one dependency by adding below single dependency.
                       |```scala
                       |libraryDependencies += "net.exoego" %%% "aws-sdk-scalajs-facade" % "VERSION(SEE README.md)"
                       |```
                       |This dependency contains all AWS and, also offers companion object `AWS` as same as `aws-sdk-js`.
                       |However, this artifact is quite huge (100MB+!!).
                       |
                       |If you need only a few AWS (e.g. S3 and DynamoDB), you may consider to depend only minimum dependenies from the below list.
                       |It will shorten download time and linking time (`fullOptJS`/`fastOptJS`).
                       |
                       |```scala
                       |val awsSdkScalajsFacadeVersion = "VERSION(SEE README.md)"
                       |libraryDependencies ++= Seq(
                       |${types}
                       |)
                       |```
                       |""".stripMargin.trim)
      ()
    } finally {
      writer.close()
    }
  }
}
