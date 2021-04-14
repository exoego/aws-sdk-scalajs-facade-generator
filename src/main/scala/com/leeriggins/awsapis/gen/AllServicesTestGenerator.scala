package com.leeriggins.awsapis.gen

import com.leeriggins.awsapis.models.Api

import java.io.{File, FileWriter, PrintWriter}

object AllServicesTestGenerator {
  def generate(extractApis: Seq[Api]): Unit = {
    val projectDir = new File("../aws-sdk-scalajs-facade/all")
    projectDir.mkdirs()
    val packageRootDir = new File(projectDir, s"src/test/scala/net/exoego")
    packageRootDir.mkdirs()
    val file = new File(packageRootDir, s"AllServicesTest.scala")
    file.createNewFile()
    val writer = new PrintWriter(new FileWriter(file))

    val types = extractApis
      .map(_.serviceClassName)
      .sorted
      .map { sdkClassName =>
        s"""  test("${sdkClassName}") {
           |    val instance = new services.${sdkClassName.toLowerCase}.${sdkClassName}(config)
           |  }
           |""".stripMargin
      }
      .mkString("\n")

    try {
      writer.append(s"""package net.exoego
                       |
                       |import facade.amazonaws.{AWSConfig, services}
                       |import org.scalatest.funsuite.AnyFunSuite
                       |
                       |class AllServicesTest extends AnyFunSuite {
                       |  val config = AWSConfig(
                       |    endpoint = "http://localhost"
                       |  )
                       |
                       |${types}
                       |}
                       |""".stripMargin.trim)
      ()
    } finally {
      writer.close()
    }
  }
}
