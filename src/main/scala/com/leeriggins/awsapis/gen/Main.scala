package com.leeriggins.awsapis.gen

import com.leeriggins.awsapis.models.Api
import com.leeriggins.awsapis.parser.Apis.{ApiType, json}
import com.leeriggins.awsapis.parser.{AwsApiTypeParser, InputParser, OutputParser}
import org.json4s._
import org.json4s.jackson.JsonMethods._

object Main {
  def main(args: Array[String]): Unit = {
    implicit val formats: Formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format

    val extractApis = com.leeriggins.awsapis.Apis.versions.map { case (name, version) =>
      val text       = json(name, version, ApiType.normal)
      val parsedText = parse(text)
      parsedText.extract[Api]
    }

    ChangeDetector.checkNewService()
    ChangeDetector.checkNonServices()

    ProjectsSbtGenerator.update(extractApis)
    AwsPackageFileGenerator.generate(extractApis)
    AWSConfigWithServicesDefaultGenerator.generate(extractApis)
    AllServicesTestGenerator.generate(extractApis)
    ArtifactsListGenerator.generate(extractApis)
  }
}
