package com.leeriggins.awsapis.parser

import org.json4s._
import com.leeriggins.awsapis.models._
import com.leeriggins.awsapis.models.AwsApiType._

import scala.reflect.ClassTag
import scala.util.Try

object FieldUtils {

  /** Provides convience methods for retriving fields by name from a list of fields. */
  implicit class FieldsImplicits(fields: List[JField]) {

    /** Whether the fieldName/fieldValue exists as a string field in the list. */
    def hasStringValue(fieldName: String, fieldValue: String): Boolean = {
      fields.exists {
        case JField(name, JString(value)) => (name == fieldName && value == fieldValue)
        case _                            => false
      }
    }

    /** Whether the "type" field has the given string value. */
    def hasTypeValue(value: String): Boolean = this.hasStringValue("type", value)

    /** Whether a field with the name "shape" exists. */
    def hasShape(): Boolean = fields.exists(_._1 == "shape")

    /** Retrieve a JSON value by field name. */
    def getFieldValue(fieldName: String): Option[JValue] = {
      fields.find { case JField(name, _) => name == fieldName }.map(_._2)
    }

    /** Retrieve a boolean value by field name. */
    def getBoolean(fieldName: String): Option[Boolean] = {
      getFieldValue(fieldName) match {
        case Some(JBool(value)) => Some(value)
        case _                  => None
      }
    }

    /** Retrieve an int value by field name. */
    def getInt(fieldName: String): Option[BigInt] = {
      getFieldValue(fieldName) match {
        case Some(JInt(value)) => Some(value)
        case _                 => None
      }
    }

    /** Retrieve a double value by field name. */
    def getDouble(fieldName: String): Option[Double] = {
      getFieldValue(fieldName) match {
        case Some(JDouble(value)) => Some(value)
        case _                    => None
      }
    }

    /** Retrieve a string value by field name. */
    def getString(fieldName: String): Option[String] = {
      getFieldValue(fieldName) match {
        case Some(JInt(value))    => Some(value.toString)
        case Some(JDouble(value)) => Some(value.toString)
        case Some(JString(value)) => Some(value)
        case _                    => None
      }
    }

    /** Retrieve the string value of the "location" field. */
    def getLocation(): Option[String] = {
      getFieldValue("location") match {
        case Some(JString(name)) => Some(name)
        case _                   => None
      }
    }

    /** Retrieve the string value of the "locationName" field. */
    def getLocationName(): Option[String] = {
      getFieldValue("locationName") match {
        case Some(JString(name)) => Some(name)
        case _                   => None
      }
    }

    def getDocumentation(): Option[String] = {
      getFieldValue("documentation") match {
        case Some(JString(docs)) => Some(docs)
        case _                   => None
      }
    }
  }

  /** Retrieve a single optional field. */
  def optField(name: String, opt: Option[_])(implicit formats: Formats): Option[JField] = {
    opt.map { value =>
      JField(name, Extraction.decompose(value))
    }
  }
}

object Helper {
  def decompose[T <: JValue](e: Any)(implicit formats: Formats, tag: ClassTag[T]): T = {
    Extraction.decompose(e) match {
      case t: T => t
      case _    => throw new Exception(s"${e} unmatched to ${tag}")
    }
  }
}

object InputParser {
  import FieldUtils._

  object Format
      extends CustomSerializer[Input](
        implicit fmt =>
          ({
            case JObject(fields) => {
              val payload      = fields.getString("payload")
              val apiType      = JObject(fields).extract[AwsApiType]
              val xmlNamespace = fields.getFieldValue("xmlNamespace").map(_.extract[XmlNamespace])
              Input(payload, apiType, xmlNamespace)
            }
          }, {
            case input: Input => {
              val JObject(typeFields) = input.apiType match {
                case structure: StructureType if (structure.members.isEmpty) => {
                  val JObject(baseFields) = Helper.decompose[JObject](structure)
                  JObject(baseFields :+ JField("members", JObject(Nil)))
                }
                case _ => Helper.decompose[JObject](input.apiType)
              }

              val payloadField      = optField("payload", input.payload)
              val xmlNamespaceField = optField("xmlNamespace", input.xmlNamespace)

              JObject(List(payloadField, xmlNamespaceField).flatten ++ typeFields)
            }
          })
      )
}

object OutputParser {
  import FieldUtils._

  object Format
      extends CustomSerializer[Output](
        implicit format =>
          ({
            case JObject(fields) => {
              val resultWrapper = fields.getString("resultWrapper")
              val payload       = fields.getString("payload")
              val apiType       = JObject(fields).extract[AwsApiType]
              Output(resultWrapper, payload, apiType)
            }
          }, {
            case output: Output => {
              val JObject(typeFields) = output.apiType match {
                case structure: StructureType if (structure.members.isEmpty) => {
                  val JObject(baseFields) = Helper.decompose[JObject](structure)
                  JObject(baseFields :+ JField("members", JObject(Nil)))
                }
                case other => Helper.decompose[JObject](other)
              }

              val resultWrapperField = optField("resultWrapper", output.resultWrapper)
              val payloadField       = optField("payload", output.payload)

              JObject(List(resultWrapperField, payloadField).flatten ++ typeFields)
            }
          })
      )
}

object AwsApiTypeParser {
  class Deserializer(implicit formats: Formats) extends PartialFunction[JValue, AwsApiType] {
    import FieldUtils._

    /** Attempt to deserialize the given JSON into an AwsApiType. */
    override def apply(value: JValue): AwsApiType = {
      val deprecated = value match {
        case JObject(fields) => {
          fields.getBoolean("deprecated")
        }
        case _ => None
      }

      val sensitive = value match {
        case JObject(fields) => {
          fields.getBoolean("sensitive")
        }
        case _ => None
      }

      value match {
        case JObject(fields) if (fields.hasShape) => {
          val xmlNamespace = fields.getFieldValue("xmlNamespace").map(_.extract[XmlNamespace])
          val xmlOrder     = fields.getFieldValue("xmlOrder").map(_.extract[List[String]])
          ShapeType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            shape = fields.getString("shape").get,
            eventpayload = fields.getBoolean("eventpayload"),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            flattened = fields.getBoolean("flattened"),
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage"),
            xmlNamespace = xmlNamespace,
            xmlOrder = xmlOrder,
            xmlAttribute = fields.getBoolean("xmlAttribute"),
            queryName = fields.getString("queryName"),
            streaming = fields.getBoolean("streaming"),
            wrapper = fields.getBoolean("wrapper"),
            jsonvalue = fields.getBoolean("jsonvalue"),
            sensitive = sensitive,
            idempotencyToken = fields.getBoolean("idempotencyToken")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("map")) => {
          val keyType   = fields.getFieldValue("key").get.extract[AwsApiType]
          val valueType = fields.getFieldValue("value").get.extract[AwsApiType]
          val flattened = fields.getBoolean("flattened")

          MapType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            key = keyType,
            value = valueType,
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            documentation = fields.getDocumentation(),
            flattened = flattened,
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("structure") && fields.exists(_._1 == "exception")) => {
          val required  = fields.getFieldValue("required").map(_.extract[List[String]]).getOrElse(List())
          val members   = fields.getFieldValue("members").map(_.extract[Map[String, AwsApiType]])
          val xmlOrder  = fields.getFieldValue("xmlOrder").map(_.extract[List[String]])
          val error     = fields.getFieldValue("error").map(_.extract[ErrorInfo])
          val exception = fields.getBoolean("exception").get

          ErrorType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            error = error,
            exception = exception,
            fault = fields.getBoolean("fault"),
            required = required,
            members = members,
            xmlOrder = xmlOrder,
            documentation = fields.getDocumentation(),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("structure") && !fields.exists(_._1 == "error")) => {
          val required     = fields.getFieldValue("required").map(_.extract[List[String]])
          val members      = fields.getFieldValue("members").map(_.extract[Map[String, AwsApiType]])
          val xmlNamespace = fields.getFieldValue("xmlNamespace").map(_.extract[XmlNamespace])
          val xmlOrder     = fields.getFieldValue("xmlOrder").map(_.extract[List[String]])

          StructureType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            event = fields.getBoolean("event"),
            eventstream = fields.getBoolean("eventstream"),
            eventpayload = fields.getBoolean("eventpayload"),
            required = required,
            members = members,
            documentation = fields.getDocumentation(),
            payload = fields.getString("payload"),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage"),
            xmlNamespace = xmlNamespace,
            xmlOrder = xmlOrder,
            wrapper = fields.getBoolean("wrapper")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("list")) => {
          val flattened = fields.getBoolean("flattened")

          ListType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            member = fields.getFieldValue("member").map(_.extract[AwsApiType]).get,
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            documentation = fields.getDocumentation(),
            flattened = flattened,
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("integer")) => {
          IntegerType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("long")) => {
          LongType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("float")) => {
          FloatType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage"),
            max = fields.getString("max"),
            min = fields.getString("min")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("double")) => {
          DoubleType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage"),
            max = fields.getString("max"),
            min = fields.getString("min")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("timestamp")) => {
          val timestampFormat = fields.getString("timestampFormat")
          TimestampType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            timestampFormat = timestampFormat,
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("boolean")) => {
          BooleanType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("blob")) => {
          val streaming = fields.getBoolean("streaming")

          BlobType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            eventpayload = fields.getBoolean("eventpayload"),
            box = fields.getBoolean("box"),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            documentation = fields.getDocumentation(),
            streaming = streaming,
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("string") && fields.exists(_._1 == "enum")) => {
          val enumSymbols = fields.getFieldValue("enum").map(_.extract[List[String]]).get
          EnumType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            pattern = fields.getString("pattern"),
            symbols = enumSymbols,
            documentation = fields.getDocumentation(),
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage")
          )
        }

        case JObject(fields) if (fields.hasTypeValue("string")) => {
          val streaming    = fields.getBoolean("streaming")
          val xmlAttribute = fields.getBoolean("xmlAttribute")
          ExplicitStringType(
            location = fields.getLocation(),
            xmlAttribute = xmlAttribute,
            locationName = fields.getLocationName(),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            pattern = fields.getString("pattern"),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            streaming = streaming,
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage"),
            jsonvalue = fields.getBoolean("jsonvalue"),
            idempotencyToken = fields.getBoolean("idempotencyToken")
          )
        }

        case JObject(fields) if (!fields.exists(_._1 == "type")) => {
          val streaming    = fields.getBoolean("streaming")
          val xmlAttribute = fields.getBoolean("xmlAttribute")
          DefaultStringType(
            location = fields.getLocation(),
            xmlAttribute = xmlAttribute,
            locationName = fields.getLocationName(),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            pattern = fields.getString("pattern"),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            streaming = streaming,
            sensitive = sensitive,
            deprecated = deprecated,
            deprecatedMessage = fields.getString("deprecatedMessage"),
            jsonvalue = fields.getBoolean("jsonvalue"),
            idempotencyToken = fields.getBoolean("idempotencyToken")
          )
        }

        case _ => {
          throw new AssertionError("Unreachable: value = " + value)
        }
      }
    }

    // just check for an object
    override def isDefinedAt(value: JValue): Boolean = {
      value.isInstanceOf[JObject]
    }
  }

  class Serializer(implicit formats: Formats) extends PartialFunction[Any, JObject] {
    import FieldUtils._

    private def toNumber(d: String): AnyVal = {
      if (d == "0.0") {
        0.0
      } else {
        Try(d.toLong) match {
          case scala.util.Success(value) => value
          case _                         => d.toDouble
        }
      }
    }

    private implicit class AwsApiTypeImplicits(awsApiType: AwsApiType) {
      def defaultFields()(implicit formats: Formats): List[JField] = {
        val baseFields = optField("location", awsApiType.location) ::
          optField("locationName", awsApiType.locationName) ::
          optField("documentation", awsApiType.documentation) ::
          optField("sensitive", awsApiType.sensitive) ::
          optField("deprecated", awsApiType.deprecated) ::
          optField("deprecatedMessage", awsApiType.deprecatedMessage) ::
          Nil

        val sizeFields = awsApiType match {
          case sized: Sized => {
            optField("min", sized.min) :: optField("max", sized.max) :: Nil
          }
          case _ => Nil
        }

        val primitiveFields = awsApiType match {
          case primitive: AwsApiBoxedType => {
            optField("box", primitive.box) ::
              Nil
          }
          case _ => Nil
        }

        val stringFields = awsApiType match {
          case string: StringType => {
            optField("xmlAttribute", string.xmlAttribute) ::
              optField("streaming", string.streaming) ::
              optField("pattern", string.pattern) ::
              optField("idempotencyToken", string.idempotencyToken) ::
              optField("jsonvalue", string.jsonvalue) ::
              Nil
          }
          case enum: EnumType => {
            optField("pattern", enum.pattern) ::
              Nil
          }
          case _ => Nil
        }

        (baseFields ++ sizeFields ++ primitiveFields ++ stringFields).flatten
      }
    }

    override def apply(value: Any): JObject = value match {
      case list: ListType => {
        val flattenedField = optField("flattened", list.flattened)
        val fields         = List(JField("type", JString("list")), JField("member", Extraction.decompose(list.member))) ++ flattenedField

        JObject(list.defaultFields() ++ fields)
      }
      case map: MapType => {
        val flattenedField = optField("flattened", map.flattened)
        val fields = List(
          JField("type", JString("map")),
          JField("key", Extraction.decompose(map.key)),
          JField("value", Extraction.decompose(map.value))
        ) ++ flattenedField
        JObject(map.defaultFields() ++ fields)
      }
      case error: ErrorType => {
        val requiredField = if (error.required.isEmpty) {
          List()
        } else {
          List(JField("required", JArray(error.required.map { fieldName =>
            JString(fieldName)
          })))
        }
        val membersField = Some(JField("members", Extraction.decompose(error.members)))
        val xmlOrderField = error.xmlOrder.map { xmlOrder =>
          JField("xmlOrder", Extraction.decompose(xmlOrder))
        }
        val errorField = error.error.map { error =>
          JField("error", Extraction.decompose(error))
        }
        val exceptionField = Some(JField("exception", Extraction.decompose(error.exception)))
        val typeField      = Some(JField("type", JString("structure")))
        val faultField     = optField("fault", error.fault)

        val fields =
          (errorField :: exceptionField :: faultField :: typeField :: membersField :: xmlOrderField :: Nil).flatten

        JObject(error.defaultFields() ++ fields ++ requiredField)
      }
      case structure: StructureType => {
        val requiredField = structure.required match {
          case None => List()
          case Some(list) =>
            List(JField("required", JArray(list.map { fieldName =>
              JString(fieldName)
            })))
        }

        val membersField      = optField("members", structure.members)
        val payloadField      = optField("payload", structure.payload)
        val sensitiveField    = optField("sensitive", structure.sensitive)
        val xmlNamespaceField = optField("xmlNamespace", structure.xmlNamespace)
        val xmlOrderField     = optField("xmlOrder", structure.xmlOrder)
        val wrapperField      = optField("wrapper", structure.wrapper)
        val eventField        = optField("event", structure.event)
        val eventstreamField  = optField("eventstream", structure.eventstream)
        val eventpayloadField = optField("eventpayload", structure.eventpayload)

        val fields = JField("type", JString("structure")) +: (requiredField ++ membersField ++ payloadField ++ sensitiveField ++ xmlNamespaceField ++ xmlOrderField ++ wrapperField ++ eventField ++ eventstreamField ++ eventpayloadField)
        JObject(structure.defaultFields() ++ fields)
      }
      case shape: ShapeType => {
        val optFields = List(
          optField("box", shape.box),
          optField("eventpayload", shape.eventpayload),
          optField("xmlNamespace", shape.xmlNamespace),
          optField("xmlOrder", shape.xmlOrder),
          optField("xmlAttribute", shape.xmlAttribute),
          optField("queryName", shape.queryName),
          optField("flattened", shape.flattened),
          optField("wrapper", shape.wrapper),
          optField("streaming", shape.streaming),
          optField("jsonvalue", shape.jsonvalue),
          optField("idempotencyToken", shape.idempotencyToken)
        ).flatten

        val fields = JField("shape", JString(shape.shape)) +: optFields

        JObject(shape.defaultFields() ++ fields)
      }
      case boolean: BooleanType => {
        JObject(boolean.defaultFields() ++ List(JField("type", JString("boolean"))))
      }
      case integer: IntegerType => {
        JObject(integer.defaultFields() ++ List(JField("type", JString("integer"))))
      }
      case long: LongType => {
        JObject(long.defaultFields() ++ List(JField("type", JString("long"))))
      }
      case float: FloatType => {
        val optFields = List(optField("max", float.max.map(toNumber)), optField("min", float.min.map(toNumber))).flatten
        val fields    = JField("type", JString("float")) +: optFields
        JObject(float.defaultFields() ++ fields)
      }
      case double: DoubleType => {
        val optFields =
          List(optField("max", double.max.map(toNumber)), optField("min", double.min.map(toNumber))).flatten
        val fields = JField("type", JString("double")) +: optFields
        JObject(double.defaultFields() ++ fields)
      }
      case timestamp: TimestampType => {
        val timestampFormatField = optField("timestampFormat", timestamp.timestampFormat)
        val fields               = List(JField("type", JString("timestamp"))) ++ timestampFormatField
        JObject(timestamp.defaultFields() ++ fields)
      }
      case blob: BlobType => {
        val streamingField    = optField("streaming", blob.streaming)
        val sensitiveField    = optField("sensitive", blob.sensitive)
        val eventpayloadField = optField("eventpayload", blob.eventpayload)
        JObject(
          blob.defaultFields() ++ List(JField("type", JString("blob"))) ++ streamingField ++ sensitiveField ++ eventpayloadField
        )
      }
      case enum: EnumType => {
        val enumSymbols = enum.symbols.map { symbol =>
          JString(symbol)
        }
        val enumField = JField("enum", JArray(enumSymbols))
        val typeField = JField("type", JString("string"))

        JObject(typeField :: enumField :: enum.defaultFields())
      }
      case string: ExplicitStringType => {
        val typeField = JField("type", JString("string"))
        JObject(typeField +: string.defaultFields())
      }
      case string: DefaultStringType => {
        JObject(string.defaultFields())
      }
    }

    override def isDefinedAt(value: Any): Boolean = {
      value.isInstanceOf[AwsApiType]
    }
  }

  object Format extends CustomSerializer[AwsApiType](implicit format => (new Deserializer(), new Serializer()))
}
