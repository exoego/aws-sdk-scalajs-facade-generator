package com.leeriggins.awsapis.models

/** The API for a single AWS service like AutoScaling or EC2. */
case class Api(version: Option[String],
               metadata: Metadata,
               documentation: Option[String],
               operations: Map[String, Operation],
               shapes: Map[String, AwsApiType],
               examples: Option[Examples],
               authorizers: Option[Map[String, Authorizer]] = None
) {
  // Used as Class name and file name
  val serviceClassName: String = this.metadata.serviceId.replaceAll(" ", "") match {
    // special treatment
    case "amp"                         => "Amp"
    case "ApplicationDiscoveryService" => "ApplicationDiscovery"
    case "Budgets"                     => "BudgetsService"
    case "CostandUsageReportService"   => "CUR"
    case "DatabaseMigrationService"    => "DMS"
    case "ElasticLoadBalancing"        => "ELB"
    case "ElasticLoadBalancingv2"      => "ELBv2"
    case "ElasticsearchService"        => "ES"
    case "fis"                         => "Fis"
    case "IoT"                         => "Iot"
    case "IoTDataPlane"                => "IotData"
    case "LexRuntimeService"           => "LexRuntime"
    case "RDSData"                     => "RDSDataService"
    case "SFN"                         => "StepFunctions"
    case "SageMakerA2IRuntime"         => "AugmentedAIRuntime"
    case "Transcribe"                  => "TranscribeService"
    case "WAFV2"                       => "WAFv2"
    case "codestarnotifications"       => "CodeStarNotifications"
    case "forecast"                    => "Forecast"
    case "forecastquery"               => "ForecastQuery"
    case "identitystore"               => "IdentityStore"
    case "imagebuilder"                => "Imagebuilder"
    case "kendra"                      => "Kendra"
    case "kinesisvideosignaling"       => "KinesisVideoSignalingChannels"
    case "mq"                          => "MQ"
    case "mgn"                         => "Mgn"
    case "savingsplans"                => "SavingsPlans"
    case "schemas"                     => "Schemas"
    case "signer"                      => "Signer"
    case "codeartifact"                => "CodeArtifact"
    case "ivs"                         => "IVS"
    case "synthetics"                  => "Synthetics"
    case otherwise                     => otherwise
  }

  // Used in @JSImport("aws-sdk", "ForecastService")
  val sdkClassName = serviceClassName match {
    case "ApplicationDiscovery"    => "Discovery"
    case "BudgetsService"          => "Budgets"
    case "CognitoIdentityProvider" => "CognitoIdentityServiceProvider"
    case "Forecast"                => "ForecastService"
    case "ForecastQuery"           => "ForecastQueryService"
    case "IoTDataPlane"            => "IotData"
    case "KinesisVideoSignaling"   => "KinesisVideoSignalingChannels"
    case "SESv2"                   => "SESV2"
    case "WAFv2"                   => "WAFV2"
    case otherwise                 => otherwise
  }
}

case class Authorizer(name: String, `type`: String, placement: AuthorizerPlacement)

case class AuthorizerPlacement(location: String, name: String)

case class Examples()

case class Metadata(apiVersion: String,
                    checksumFormat: Option[String],
                    endpointPrefix: String,
                    globalEndpoint: Option[String],
                    jsonVersion: Option[String],
                    serviceAbbreviation: Option[String],
                    serviceFullName: String,
                    serviceId: String,
                    signingName: Option[String],
                    signatureVersion: String,
                    xmlNamespace: Option[String],
                    targetPrefix: Option[String],
                    timestampFormat: Option[String],
                    protocol: String,
                    protocolSettings: Option[ProtocolSettings],
                    uid: Option[String]
)

case class ProtocolSettings(h2: String)

case class Operation(http: Option[Http],
                     input: Option[Input],
                     output: Option[Output],
                     errors: Option[List[Error]],
                     authtype: Option[String],
                     endpointdiscovery: Option[EndpointDiscovery],
                     endpointoperation: Option[Boolean],
                     endpoint: Option[Endpoint],
                     documentation: Option[String],
                     documentationUrl: Option[String],
                     deprecated: Option[Boolean],
                     httpChecksumRequired: Option[Boolean],
                     deprecatedMessage: Option[String],
                     name: Option[String],
                     alias: Option[String],
                     idempotent: Option[Boolean],
                     internal: Option[Boolean]
)

case class EndpointDiscovery(required: Option[Boolean])

case class Endpoint(hostPrefix: Option[String])

case class Error(shape: String,
                 error: Option[ErrorInfo],
                 exception: Option[Boolean],
                 fault: Option[Boolean],
                 documentation: Option[String],
                 xmlOrder: Option[List[String]]
)

case class ErrorInfo(code: Option[String], httpStatusCode: Option[Int], senderFault: Option[Boolean])

case class Http(method: Option[String], requestUri: Option[String], responseCode: Option[Int])

/** Describes the input type of a method. */
case class Input(payload: Option[String], apiType: AwsApiType, xmlNamespace: Option[XmlNamespace])

/** Describes the output type of a method. */
case class Output(resultWrapper: Option[String], payload: Option[String], apiType: AwsApiType)

case class XmlNamespace(uri: Option[String], prefix: Option[String])

/** Parent trait for all types defined in the AWS APIs. */
abstract sealed trait AwsApiType {
  val location: Option[String]
  val locationName: Option[String]
  val description: Option[String]
  val documentation: Option[String]
  val sensitive: Option[Boolean]
  val deprecated: Option[Boolean]
  val deprecatedMessage: Option[String]
}

/** A type that has a size. */
abstract sealed trait Sized {
  val min: Option[BigInt]
  val max: Option[BigInt]
}

/** Represents a (typically primitive) type that can be boxed. */
abstract sealed trait AwsApiBoxedType extends AwsApiType {
  val box: Option[Boolean]
}

/** Represents a String. Can either be explicitly specified as "type": "string" or implicit by omitting any type or shape fields. */
abstract sealed trait StringType extends AwsApiBoxedType {
  val xmlAttribute: Option[Boolean]
  val streaming: Option[Boolean]
  val pattern: Option[String]
  val idempotencyToken: Option[Boolean]
  val jsonvalue: Option[Boolean]

}

object AwsApiType {
  case class ListType(location: Option[String],
                      locationName: Option[String],
                      member: AwsApiType,
                      min: Option[BigInt],
                      max: Option[BigInt],
                      documentation: Option[String],
                      description: Option[String],
                      flattened: Option[Boolean],
                      sensitive: Option[Boolean],
                      deprecated: Option[Boolean],
                      deprecatedMessage: Option[String]
  ) extends AwsApiType
      with Sized

  case class MapType(location: Option[String],
                     locationName: Option[String],
                     key: AwsApiType,
                     value: AwsApiType,
                     min: Option[BigInt],
                     max: Option[BigInt],
                     documentation: Option[String],
                     description: Option[String],
                     flattened: Option[Boolean],
                     sensitive: Option[Boolean],
                     deprecated: Option[Boolean],
                     deprecatedMessage: Option[String]
  ) extends AwsApiType
      with Sized

  case class StructureType(location: Option[String],
                           locationName: Option[String],
                           event: Option[Boolean],
                           eventstream: Option[Boolean],
                           eventpayload: Option[Boolean],
                           required: Option[List[String]] = None,
                           members: Option[Map[String, AwsApiType]],
                           documentation: Option[String],
                           description: Option[String],
                           payload: Option[String],
                           union: Option[Boolean],
                           sensitive: Option[Boolean],
                           deprecated: Option[Boolean],
                           deprecatedMessage: Option[String],
                           xmlNamespace: Option[XmlNamespace],
                           xmlOrder: Option[List[String]],
                           wrapper: Option[Boolean],
                           box: Option[Boolean]
  ) extends AwsApiBoxedType

  /** Describes an error that may be returned. Typically modeled as a structure but represented here as a separate type. */
  case class ErrorType(location: Option[String],
                       locationName: Option[String],
                       required: List[String] = List(),
                       members: Option[Map[String, AwsApiType]],
                       xmlOrder: Option[List[String]],
                       error: Option[ErrorInfo],
                       exception: Boolean,
                       fault: Option[Boolean],
                       documentation: Option[String],
                       description: Option[String],
                       sensitive: Option[Boolean],
                       deprecated: Option[Boolean],
                       deprecatedMessage: Option[String]
  ) extends AwsApiType

  /** Describes a reference to a type defined in the service's shapes. */
  case class ShapeType(location: Option[String],
                       locationName: Option[String],
                       hostLabel: Option[Boolean],
                       shape: String,
                       jsonvalue: Option[Boolean],
                       eventpayload: Option[Boolean],
                       box: Option[Boolean],
                       documentation: Option[String],
                       description: Option[String],
                       flattened: Option[Boolean],
                       deprecated: Option[Boolean],
                       deprecatedMessage: Option[String],
                       xmlNamespace: Option[XmlNamespace],
                       xmlOrder: Option[List[String]],
                       xmlAttribute: Option[Boolean],
                       queryName: Option[String],
                       union: Option[Boolean],
                       streaming: Option[Boolean],
                       enum: Option[List[String]],
                       tags: Option[List[String]],
                       wrapper: Option[Boolean],
                       idempotencyToken: Option[Boolean],
                       sensitive: Option[Boolean]
  ) extends AwsApiType

  case class IntegerType(location: Option[String],
                         locationName: Option[String],
                         min: Option[BigInt],
                         max: Option[BigInt],
                         box: Option[Boolean],
                         documentation: Option[String],
                         description: Option[String],
                         sensitive: Option[Boolean],
                         deprecated: Option[Boolean],
                         deprecatedMessage: Option[String]
  ) extends AwsApiBoxedType
      with Sized

  case class LongType(location: Option[String],
                      locationName: Option[String],
                      min: Option[BigInt],
                      max: Option[BigInt],
                      box: Option[Boolean],
                      documentation: Option[String],
                      description: Option[String],
                      sensitive: Option[Boolean],
                      deprecated: Option[Boolean],
                      deprecatedMessage: Option[String]
  ) extends AwsApiBoxedType
      with Sized

  case class DoubleType(location: Option[String],
                        locationName: Option[String],
                        box: Option[Boolean],
                        documentation: Option[String],
                        description: Option[String],
                        sensitive: Option[Boolean],
                        deprecated: Option[Boolean],
                        deprecatedMessage: Option[String],
                        max: Option[String],
                        min: Option[String]
  ) extends AwsApiBoxedType

  case class FloatType(location: Option[String],
                       locationName: Option[String],
                       box: Option[Boolean],
                       documentation: Option[String],
                       description: Option[String],
                       sensitive: Option[Boolean],
                       deprecated: Option[Boolean],
                       deprecatedMessage: Option[String],
                       max: Option[String],
                       min: Option[String]
  ) extends AwsApiBoxedType

  case class TimestampType(location: Option[String],
                           locationName: Option[String],
                           box: Option[Boolean],
                           documentation: Option[String],
                           description: Option[String],
                           timestampFormat: Option[String],
                           sensitive: Option[Boolean],
                           deprecated: Option[Boolean],
                           deprecatedMessage: Option[String]
  ) extends AwsApiBoxedType

  case class BooleanType(location: Option[String],
                         locationName: Option[String],
                         box: Option[Boolean],
                         documentation: Option[String],
                         description: Option[String],
                         sensitive: Option[Boolean],
                         deprecated: Option[Boolean],
                         deprecatedMessage: Option[String]
  ) extends AwsApiBoxedType

  case class BlobType(location: Option[String],
                      locationName: Option[String],
                      eventpayload: Option[Boolean],
                      box: Option[Boolean],
                      min: Option[BigInt],
                      max: Option[BigInt],
                      documentation: Option[String],
                      description: Option[String],
                      streaming: Option[Boolean],
                      requiresLength: Option[Boolean],
                      sensitive: Option[Boolean],
                      deprecated: Option[Boolean],
                      deprecatedMessage: Option[String]
  ) extends AwsApiBoxedType
      with Sized

  case class EnumType(location: Option[String],
                      locationName: Option[String],
                      box: Option[Boolean],
                      min: Option[BigInt],
                      max: Option[BigInt],
                      pattern: Option[String],
                      documentation: Option[String],
                      description: Option[String],
                      symbols: List[String],
                      sensitive: Option[Boolean],
                      deprecated: Option[Boolean],
                      deprecatedMessage: Option[String]
  ) extends AwsApiBoxedType
      with Sized

  case class DefaultStringType(location: Option[String],
                               xmlAttribute: Option[Boolean],
                               hostLabel: Option[Boolean],
                               locationName: Option[String],
                               min: Option[BigInt],
                               max: Option[BigInt],
                               pattern: Option[String],
                               box: Option[Boolean],
                               documentation: Option[String],
                               description: Option[String],
                               streaming: Option[Boolean],
                               sensitive: Option[Boolean],
                               deprecated: Option[Boolean],
                               deprecatedMessage: Option[String],
                               idempotencyToken: Option[Boolean],
                               jsonvalue: Option[Boolean]
  ) extends StringType
      with Sized

  case class ExplicitStringType(location: Option[String],
                                xmlAttribute: Option[Boolean],
                                locationName: Option[String],
                                min: Option[BigInt],
                                max: Option[BigInt],
                                pattern: Option[String],
                                box: Option[Boolean],
                                documentation: Option[String],
                                description: Option[String],
                                streaming: Option[Boolean],
                                sensitive: Option[Boolean],
                                deprecated: Option[Boolean],
                                deprecatedMessage: Option[String],
                                idempotencyToken: Option[Boolean],
                                jsonvalue: Option[Boolean]
  ) extends StringType
      with Sized

}
