package com.leeriggins.awsapis.gen

import com.leeriggins.awsapis.models.Api

import java.io.{File, FileWriter, PrintWriter}

object AWSConfigWithServicesDefaultGenerator {
  def generate(extractApis: Seq[Api]): Unit = {
    val projectDir = new File("../aws-sdk-scalajs-facade/aws-sdk-v2/core")
    projectDir.mkdirs()
    val packageRootDir = new File(projectDir, s"src/main/scala/facade/amazonaws")
    packageRootDir.mkdirs()
    val file = new File(packageRootDir, s"AWSConfigWithServicesDefault.scala")
    file.createNewFile()
    val writer = new PrintWriter(new FileWriter(file))

    val types = extractApis
      .map(_.sdkClassName.toLowerCase())
      .sorted
      .map { sdkClassName =>
        val configTypeName = sdkClassName match {
          case "s3" | "s3control" => "S3ParamsWithEndpoint"
          case _                  => "ParamsWithEndpoint"
        }
        s"  var ${sdkClassName}: js.UndefOr[${configTypeName}] = js.undefined"
      }
      .mkString("\n")

    try {
      writer.append(s"""package facade.amazonaws
                       |
                       |import scala.scalajs.js
                       |
                       |class AWSConfigWithServicesDefault extends AWSConfig {
                       |${types}
                       |}
                       |""".stripMargin.trim)
      ()
    } finally {
      writer.close()
    }
  }
}
