package com.leeriggins.awsapis.gen

import com.leeriggins.awsapis.models.Api

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Files
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Using

object ProjectsSbtGenerator {
  def update(extractApis: Seq[Api]): Unit = {
    generateScalaFile(extractApis)
  }

  private val exclude = Set("DynamoDB", "DynamoDBStreams")

  private def generateScalaFile(extractApis: Seq[Api]): Unit = {
    val projectDir = new File("../aws-sdk-scalajs-facade/")
    val file       = new File(projectDir, "build.sbt")
    val base       = Files.readAllLines(file.toPath).asScala.takeWhile(_ != "//AUTO-GENERATED").mkString("\n")

    val names = extractApis.map(_.serviceClassName)
    val projectDefinitions = names
      .filterNot(exclude)
      .sorted
      .map { sdkClassName =>
        s"""lazy val aws$sdkClassName = defineAwsProject("$sdkClassName")"""
      }
      .mkString("\n")
    val projectReferences = names.sorted.map(sdkName => s"aws$sdkName").mkString(",")

    Using(new PrintWriter(new FileWriter(file))) { writer =>
      writer.append(s"""$base
                       |//AUTO-GENERATED
                       |$projectDefinitions
                       |lazy val subProjects: Seq[Project] = Seq(
                       |  core,
                       |  credentials,
                       |  dynamodbShared,
                       |$projectReferences
                       |)
                       |""".stripMargin.trim)
      ()
    }.get
  }
}
