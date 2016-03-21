package com.leeriggins.awsapis.parser

import org.scalatest._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import com.leeriggins.awsapis.Apis.{ versions => apiVersions }
import com.leeriggins.awsapis.models._
import com.leeriggins.awsapis.parser.Apis._


class ApiFormatTest extends WordSpec with Matchers {
  implicit val formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format

  def passTestsForType(tpe: ApiType): Unit = {
    apiVersions.foreach {
      case (apiName, apiVersion) =>
        s"deserialize then serialize ${apiName} version ${apiVersion} (${tpe}) without changes" in {
          val text = Apis.json(apiName, apiVersion, tpe)
          val parsedText = parse(text)

          val api = parsedText.extract[Api]

          val reserialized = parse(write(api))

          val Diff(changed, added, removed) = parsedText diff reserialized
          changed should be(JNothing)
          added should be(JNothing)
          removed should be(JNothing)
        }
    }

  }

  "The Api normal format" should passTestsForType(ApiType.normal)
  "The Api min format" should passTestsForType(ApiType.min)
}
