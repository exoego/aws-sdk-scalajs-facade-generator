package com.leeriggins.awsapis.parser

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization._
import com.leeriggins.awsapis.models._

object Apis {
  val path = s"aws-sdk-js/apis"

  def filename(service: String, date: String, apiType: ApiType): String = {
    s"${path}/${service}-${date}.${apiType}.json"
  }

  def json(service: String, date: String, apiType: ApiType): String = {
    val source = io.Source.fromFile(filename(service, date, apiType), "UTF-8")
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  sealed abstract trait ApiType
  object ApiType {
    case object min        extends ApiType
    case object normal     extends ApiType
    case object paginators extends ApiType
  }

  def main(args: Array[String]): Unit = {
    implicit val formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format

    val text = json("autoscaling", "2011-01-01", ApiType.normal)
    println(text)
    println()

    val parsedText = parse(text)

    val api = parsedText.extract[Api]

    val reserialized = parse(write(api))

    val Diff(changed, added, removed) = parsedText diff reserialized

    println("Changed:")
    println(pretty(render(changed)))
    println()

    println("Added:")
    println(pretty(render(added)))
    println()

    println("Removed:")
    println(pretty(render(removed)))
    println()

  }

}
