package com.leeriggins.awsapis.parser

import org.json4s._
import com.leeriggins.awsapis.models._
import com.leeriggins.awsapis.models.AwsApiType._

object FieldUtils {
  /** Provides convience methods for retriving fields by name from a list of fields. */
  implicit class FieldsImplicits(fields: List[JField]) {
    /** Whether the fieldName/fieldValue exists as a string field in the list. */
    def hasStringValue(fieldName: String, fieldValue: String): Boolean = {
      fields.exists {
        case JField(name, JString(value)) => (name == fieldName && value == fieldValue)
        case _ => false
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
        case _ => None
      }
    }

    /** Retrieve an int value by field name. */
    def getInt(fieldName: String): Option[BigInt] = {
      getFieldValue(fieldName) match {
        case Some(JInt(value)) => Some(value)
        case _ => None
      }
    }

    /** Retrieve a string value by field name. */
    def getString(fieldName: String): Option[String] = {
      getFieldValue(fieldName) match {
        case Some(JString(value)) => Some(value)
        case _ => None
      }
    }

    /** Retrieve the string value of the "location" field. */
    def getLocation(): Option[String] = {
      getFieldValue("location") match {
        case Some(JString(name)) => Some(name)
        case _ => None
      }
    }

    /** Retrieve the string value of the "locationName" field. */
    def getLocationName(): Option[String] = {
      getFieldValue("locationName") match {
        case Some(JString(name)) => Some(name)
        case _ => None
      }
    }

    def getDocumentation(): Option[String] = {
      getFieldValue("documentation") match {
        case Some(JString(docs)) => Some(docs)
        case _ => None
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

object InputParser {
  import FieldUtils._

  object Format extends CustomSerializer[Input](implicit fmt => ({
    case JObject(fields) => {
      val payload = fields.getString("payload")
      val apiType = JObject(fields).extract[AwsApiType]
      val xmlNamespace = fields.getFieldValue("xmlNamespace").map(_.extract[XmlNamespace])
      Input(payload, apiType, xmlNamespace)
    }
  }, {
    case input: Input => {
      val JObject(typeFields) = input.`type` match {
        case structure: StructureType if (structure.members.isEmpty) => {
          val JObject(baseFields) = Extraction.decompose(structure)
          JObject(baseFields :+ JField("members", JObject(Nil)))
        }
        case other => Extraction.decompose(input.`type`)
      }

      val payloadField = optField("payload", input.payload)
      val xmlNamespaceField = optField("xmlNamespace", input.xmlNamespace)

      JObject(List(payloadField, xmlNamespaceField).flatten ++ typeFields)
    }
  }))
}

object OutputParser {
  import FieldUtils._

  object Format extends CustomSerializer[Output](implicit format => ({
    case JObject(fields) => {
      val resultWrapper = fields.getString("resultWrapper")
      val payload = fields.getString("payload")
      val apiType = JObject(fields).extract[AwsApiType]
      Output(resultWrapper, payload, apiType)
    }
  }, {
    case output: Output => {
      val JObject(typeFields) = output.`type` match {
        case structure: StructureType if (structure.members.isEmpty) => {
          val JObject(baseFields) = Extraction.decompose(structure)
          JObject(baseFields :+ JField("members", JObject(Nil)))
        }
        case other => Extraction.decompose(other)
      }

      val resultWrapperField = optField("resultWrapper", output.resultWrapper)
      val payloadField = optField("payload", output.payload)

      JObject(List(resultWrapperField, payloadField).flatten ++ typeFields)
    }
  }))
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

      value match {
        case JObject(fields) if (fields.hasShape) => {
          val xmlNamespace = fields.getFieldValue("xmlNamespace").map(_.extract[XmlNamespace])
          val xmlOrder = fields.getFieldValue("xmlOrder").map(_.extract[List[String]])
          ShapeType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            shape = fields.getString("shape").get,
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            flattened = fields.getBoolean("flattened"),
            deprecated = deprecated,
            xmlNamespace = xmlNamespace,
            xmlOrder = xmlOrder,
            xmlAttribute = fields.getBoolean("xmlAttribute"),
            queryName = fields.getString("queryName"),
            streaming = fields.getBoolean("streaming"),
            wrapper = fields.getBoolean("wrapper"))
        }

        case JObject(fields) if (fields.hasTypeValue("map")) => {
          val keyType = fields.getFieldValue("key").get.extract[AwsApiType]
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
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("structure") && fields.exists(_._1 == "exception")) => {
          val required = fields.getFieldValue("required").map(_.extract[List[String]]).getOrElse(List())
          val members = fields.getFieldValue("members").map(_.extract[Map[String, AwsApiType]])
          val xmlOrder = fields.getFieldValue("xmlOrder").map(_.extract[List[String]])
          val error = fields.getFieldValue("error").map(_.extract[ErrorInfo])
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
            deprecated = deprecated)

        }

        case JObject(fields) if (fields.hasTypeValue("structure") && !fields.exists(_._1 == "error")) => {
          val required = fields.getFieldValue("required").map(_.extract[List[String]]).getOrElse(List())
          val members = fields.getFieldValue("members").map(_.extract[Map[String, AwsApiType]])
          val xmlNamespace = fields.getFieldValue("xmlNamespace").map(_.extract[XmlNamespace])
          val xmlOrder = fields.getFieldValue("xmlOrder").map(_.extract[List[String]])

          StructureType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            required = required,
            members = members,
            documentation = fields.getDocumentation(),
            payload = fields.getString("payload"),
            sensitive = fields.getBoolean("sensitive"),
            deprecated = deprecated,
            xmlNamespace = xmlNamespace,
            xmlOrder = xmlOrder,
            wrapper = fields.getBoolean("wrapper"))
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
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("integer")) => {
          IntegerType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("long")) => {
          LongType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("float")) => {
          FloatType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("double")) => {
          DoubleType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("timestamp")) => {
          val timestampFormat = fields.getString("timestampFormat")
          TimestampType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            timestampFormat = timestampFormat,
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("boolean")) => {
          BooleanType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            documentation = fields.getDocumentation(),
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("blob")) => {
          val streaming = fields.getBoolean("streaming")
          val sensitive = fields.getBoolean("sensitive")

          BlobType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            documentation = fields.getDocumentation(),
            streaming = streaming,
            sensitive = sensitive,
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("string") && fields.exists(_._1 == "enum")) => {
          val enumSymbols = fields.getFieldValue("enum").map(_.extract[List[String]]).get
          EnumType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            box = fields.getBoolean("box"),
            min = fields.getInt("min"),
            max = fields.getInt("max"),
            symbols = enumSymbols,
            documentation = fields.getDocumentation(),
            deprecated = deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("string")) => {
          val sensitive = fields.getBoolean("sensitive")
          val streaming = fields.getBoolean("streaming")
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
            deprecated = deprecated)
        }

        case JObject(fields) if (!fields.exists(_._1 == "type")) => {
          val sensitive = fields.getBoolean("sensitive")
          val streaming = fields.getBoolean("streaming")
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
            deprecated = deprecated)
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

    private implicit class AwsApiTypeImplicits(awsApiType: AwsApiType) {
      def defaultFields()(implicit formats: Formats): List[JField] = {
        val baseFields = optField("location", awsApiType.location) ::
          optField("locationName", awsApiType.locationName) ::
          optField("documentation", awsApiType.documentation) ::
          optField("deprecated", awsApiType.deprecated) ::
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
              optField("sensitive", string.sensitive) ::
              optField("streaming", string.streaming) ::
              optField("pattern", string.pattern) ::
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
        val fields = List(JField("type", JString("list")), JField("member", Extraction.decompose(list.member))) ++ flattenedField

        JObject(list.defaultFields() ++ fields)
      }
      case map: MapType => {
        val flattenedField = optField("flattened", map.flattened)
        val fields = List(JField("type", JString("map")), JField("key", Extraction.decompose(map.key)), JField("value", Extraction.decompose(map.value))) ++ flattenedField
        JObject(map.defaultFields() ++ fields)
      }
      case error: ErrorType => {
        val requiredField = if (error.required.isEmpty) {
          List()
        } else {
          List(JField("required", JArray(error.required.map { fieldName => JString(fieldName) })))
        }
        val membersField = Some(JField("members", Extraction.decompose(error.members)))
        val xmlOrderField = error.xmlOrder.map { xmlOrder => JField("xmlOrder", Extraction.decompose(xmlOrder)) }
        val errorField = error.error.map { error =>
          JField("error", Extraction.decompose(error))
        }
        val exceptionField = Some(JField("exception", Extraction.decompose(error.exception)))
        val typeField = Some(JField("type", JString("structure")))
        val faultField = optField("fault", error.fault)

        val fields = (errorField :: exceptionField :: faultField :: typeField :: membersField :: xmlOrderField :: Nil).flatten

        JObject(error.defaultFields() ++ fields ++ requiredField)
      }
      case structure: StructureType => {
        val requiredField = if (structure.required.isEmpty) {
          List()
        } else {
          List(JField("required", JArray(structure.required.map { fieldName => JString(fieldName) })))
        }

        val membersField = optField("members", structure.members)
        val payloadField = optField("payload", structure.payload)
        val sensitiveField = optField("sensitive", structure.sensitive)
        val xmlNamespaceField = optField("xmlNamespace", structure.xmlNamespace)
        val xmlOrderField = optField("xmlOrder", structure.xmlOrder)
        val wrapperField = optField("wrapper", structure.wrapper)

        val fields = JField("type", JString("structure")) +: (requiredField ++ membersField ++ payloadField ++ sensitiveField ++ xmlNamespaceField ++ xmlOrderField ++ wrapperField)
        JObject(structure.defaultFields() ++ fields)
      }
      case shape: ShapeType => {
        val optFields = List(
          optField("box", shape.box),
          optField("xmlNamespace", shape.xmlNamespace),
          optField("xmlOrder", shape.xmlOrder),
          optField("xmlAttribute", shape.xmlAttribute),
          optField("queryName", shape.queryName),
          optField("flattened", shape.flattened),
          optField("wrapper", shape.wrapper),
          optField("streaming", shape.streaming)).flatten

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
        JObject(float.defaultFields() ++ List(JField("type", JString("float"))))
      }
      case double: DoubleType => {
        JObject(double.defaultFields() ++ List(JField("type", JString("double"))))
      }
      case timestamp: TimestampType => {
        val timestampFormatField = optField("timestampFormat", timestamp.timestampFormat)
        val fields = List(JField("type", JString("timestamp"))) ++ timestampFormatField
        JObject(timestamp.defaultFields() ++ fields)
      }
      case blob: BlobType => {
        val streamingField = optField("streaming", blob.streaming)
        val sensitiveField = optField("sensitive", blob.sensitive)
        JObject(blob.defaultFields() ++ List(JField("type", JString("blob"))) ++ streamingField ++ sensitiveField)
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