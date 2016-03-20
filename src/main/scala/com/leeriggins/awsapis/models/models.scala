package com.leeriggins.awsapis.models

import org.json4s._

case class Api(
  metadata: Metadata,
  operations: Map[String, Operation],
  shapes: Map[String, AwsApiType])

case class Metadata(
  apiVersion: String,
  endpointPrefix: String,
  serviceFullName: String,
  signatureVersion: String,
  xmlNamespace: String,
  protocol: String)

case class Operation(
  input: Option[AwsApiType],
  output: Option[Output])

case class Output(
  resultWrapper: String,
  `type`: Option[String],
  shape: Option[String],
  required: Option[List[String]],
  members: Option[Map[String, AwsApiType]])

abstract sealed trait AwsApiType
abstract sealed trait AwsApiPrimitiveType extends AwsApiType
object AwsApiType {
  case class ListType(
    member: AwsApiType) extends AwsApiType

  case class StructureType(
    required: List[String] = List(),
    members: Map[String, AwsApiType] = Map()) extends AwsApiType

  case class ShapeType(
    shape: String) extends AwsApiType

  case object IntegerType extends AwsApiPrimitiveType
  case object TimestampType extends AwsApiPrimitiveType
  case object BooleanType extends AwsApiPrimitiveType
  case object StringType extends AwsApiPrimitiveType
}

