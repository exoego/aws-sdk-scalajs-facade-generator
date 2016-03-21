package com.leeriggins.awsapis.models

import org.json4s._

case class Api(
  version: Option[String],
  metadata: Metadata,
  operations: Map[String, Operation],
  shapes: Map[String, AwsApiType])

case class Metadata(
  apiVersion: String,
  checksumFormat: Option[String],
  endpointPrefix: String,
  globalEndpoint: Option[String],
  jsonVersion: Option[String],
  serviceAbbreviation: Option[String],
  serviceFullName: String,
  signingName: Option[String],
  signatureVersion: String,
  xmlNamespace: Option[String],
  targetPrefix: Option[String],
  timestampFormat: Option[String],
  protocol: String)

case class Operation(
  http: Option[Http],
  input: Option[Input],
  output: Option[Output],
  deprecated: Option[Boolean],
  alias: Option[String])

case class Http(
  method: Option[String],
  requestUri: Option[String],
  responseCode: Option[Int])

case class Input(
  payload: Option[String],
  `type`: AwsApiType,
  xmlNamespace: Option[XmlNamespace])

case class Output(
  resultWrapper: Option[String],
  payload: Option[String],
  `type`: AwsApiType)

abstract sealed trait AwsApiType {
  val location: Option[String]
  val locationName: Option[String]
  val deprecated: Option[Boolean]
}

case class XmlNamespace(
  uri: Option[String],
  prefix: Option[String])

abstract sealed trait AwsApiPrimitiveType extends AwsApiType
object AwsApiType {
  case class ListType(
    location: Option[String],
    locationName: Option[String],
    member: AwsApiType,
    flattened: Option[Boolean],
    deprecated: Option[Boolean]) extends AwsApiType

  case class MapType(
    location: Option[String],
    locationName: Option[String],
    key: AwsApiType,
    value: AwsApiType,
    flattened: Option[Boolean],
    deprecated: Option[Boolean]) extends AwsApiType

  case class StructureType(
    location: Option[String],
    locationName: Option[String],
    required: List[String] = List(),
    members: Map[String, AwsApiType] = Map(),
    sensitive: Option[Boolean],
    deprecated: Option[Boolean],
    xmlNamespace: Option[XmlNamespace],
    xmlOrder: Option[List[String]],
    wrapper: Option[Boolean]) extends AwsApiType

  case class ShapeType(
    location: Option[String],
    locationName: Option[String],
    shape: String,
    deprecated: Option[Boolean],
    xmlNamespace: Option[XmlNamespace],
    queryName: Option[String]) extends AwsApiType

  case class IntegerType(
    location: Option[String],
    locationName: Option[String],
    deprecated: Option[Boolean]) extends AwsApiPrimitiveType

  case class LongType(
    location: Option[String],
    locationName: Option[String],
    deprecated: Option[Boolean]) extends AwsApiPrimitiveType

  case class DoubleType(
    location: Option[String],
    locationName: Option[String],
    deprecated: Option[Boolean]) extends AwsApiPrimitiveType

  case class FloatType(
    location: Option[String],
    locationName: Option[String],
    deprecated: Option[Boolean]) extends AwsApiPrimitiveType

  case class TimestampType(
    location: Option[String],
    locationName: Option[String],
    timestampFormat: Option[String],
    deprecated: Option[Boolean]) extends AwsApiPrimitiveType

  case class BooleanType(
    location: Option[String],
    locationName: Option[String],
    deprecated: Option[Boolean]) extends AwsApiPrimitiveType

  case class BlobType(
    location: Option[String],
    locationName: Option[String],
    streaming: Option[Boolean],
    sensitive: Option[Boolean],
    deprecated: Option[Boolean]) extends AwsApiPrimitiveType

  trait StringType extends AwsApiPrimitiveType {
    val xmlAttribute: Option[Boolean]
    val streaming: Option[Boolean]
    val sensitive: Option[Boolean]
  }

  case class DefaultStringType(
    location: Option[String],
    xmlAttribute: Option[Boolean],
    locationName: Option[String],
    streaming: Option[Boolean],
    sensitive: Option[Boolean],
    deprecated: Option[Boolean]) extends StringType

  case class ExplicitStringType(
    location: Option[String],
    xmlAttribute: Option[Boolean],
    locationName: Option[String],
    streaming: Option[Boolean],
    sensitive: Option[Boolean],
    deprecated: Option[Boolean]) extends StringType

}
