package com.leeriggins.awsapis.parser

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

}
