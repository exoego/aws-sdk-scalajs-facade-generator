package com.leeriggins.awsapis.parser

import org.scalatest.funspec.AnyFunSpec
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import com.leeriggins.awsapis.Apis.{versions => apiVersions}
import com.leeriggins.awsapis.models._
import com.leeriggins.awsapis.parser.Apis._

class ApiFormatTest extends AnyFunSpec {
  implicit val formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format

  def passTestsForType(tpe: ApiType): Unit = {
    apiVersions.foreach { case (apiName, apiVersion) =>
      it(s"deserialize then serialize ${apiName} version ${apiVersion} (${tpe}) without changes") {
        val text       = Apis.json(apiName, apiVersion, tpe)
        val parsedText = parse(text)

        val api = parsedText.extract[Api]

        val reserialized = parse(write(api))

        val Diff(changed, added, removed) = parsedText diff reserialized
        assert(changed == JNothing)
        assert(added == JNothing)
        assert(removed == JNothing)
      }
    }
  }

  describe("The Api normal format") {
    passTestsForType(ApiType.normal)
  }

  describe("The Api min format") {
    passTestsForType(ApiType.min)
  }
}
