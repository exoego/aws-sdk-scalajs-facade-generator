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
  }

  def optField(name: String, opt: Option[_])(implicit formats: Formats): Option[JField] = {
    opt.map { value =>
      JField(name, Extraction.decompose(value))
    }
  }
}

object InputParser {
  import FieldUtils._

  object Format extends CustomSerializer[Input](implicit format => ({
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
        case other => Extraction.decompose(output.`type`)
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
          val shape = fields.getString("shape").get
          val xmlNamespace = fields.getFieldValue("xmlNamespace").map(_.extract[XmlNamespace])
          val queryName = fields.getString("queryName")
          ShapeType(
            fields.getLocation(),
            fields.getLocationName(),
            shape,
            deprecated,
            xmlNamespace,
            queryName)
        }

        case JObject(fields) if (fields.hasTypeValue("map")) => {
          val keyType = apply(fields.getFieldValue("key").get)
          val valueType = apply(fields.getFieldValue("value").get)
          val flattened = fields.getBoolean("flattened")

          MapType(
            fields.getLocation(),
            fields.getLocationName(),
            keyType,
            valueType,
            flattened,
            deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("structure")) => {
          val required = fields.getFieldValue("required").map(_.extract[List[String]]).getOrElse(List())

          val members = fields.getFieldValue("members").map {
            _ match {
              case JObject(memberFields) => {
                memberFields.map {
                  case (fieldName, fieldValue) => fieldName -> apply(fieldValue)
                }
              }
              case _ => throw new AssertionError("Unreachable if input is well-formed.")
            }
          }.getOrElse(List())

          val sensitive = fields.getBoolean("sensitive")
          val xmlNamespace = fields.getFieldValue("xmlNamespace").map(_.extract[XmlNamespace])
          val xmlOrder = fields.getFieldValue("xmlOrder").map(_.extract[List[String]])
          
          val wrapper = fields.getBoolean("wrapper")

          StructureType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            required = required,
            members = Map(members: _*),
            sensitive = sensitive,
            deprecated = deprecated,
            xmlNamespace = xmlNamespace,
            xmlOrder = xmlOrder,
            wrapper = wrapper)
        }

        case JObject(fields) if (fields.hasTypeValue("list")) => {
          val flattened = fields.getBoolean("flattened")

          ListType(
            fields.getLocation(),
            fields.getLocationName(),
            apply(fields.getFieldValue("member").get),
            flattened,
            deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("integer")) => {
          IntegerType(
            fields.getLocation(),
            fields.getLocationName(),
            deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("long")) => {
          LongType(
            fields.getLocation(),
            fields.getLocationName(),
            deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("float")) => {
          FloatType(
            fields.getLocation(),
            fields.getLocationName(),
            deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("double")) => {
          DoubleType(
            fields.getLocation(),
            fields.getLocationName(),
            deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("timestamp")) => {
          val timestampFormat = fields.getString("timestampFormat")
          TimestampType(
            fields.getLocation(),
            fields.getLocationName(),
            timestampFormat,
            deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("boolean")) => {
          BooleanType(
            fields.getLocation(),
            fields.getLocationName(),
            deprecated)
        }

        case JObject(fields) if (fields.hasTypeValue("blob")) => {
          val streaming = fields.getBoolean("streaming")
          val sensitive = fields.getBoolean("sensitive")

          BlobType(
            location = fields.getLocation(),
            locationName = fields.getLocationName(),
            streaming = streaming,
            sensitive = sensitive,
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
      def locationFields: List[JField] = {
        List(optField("location", awsApiType.location), optField("locationName", awsApiType.locationName)).flatten
      }

      def deprecatedField: Option[JField] = optField("deprecated", awsApiType.deprecated)
    }

    override def apply(value: Any): JObject = value match {
      case list: ListType => {
        val flattenedField = optField("flattened", list.flattened)
        val fields = List(JField("type", JString("list")), JField("member", apply(list.member))) ++ flattenedField

        JObject(list.locationFields ++ fields ++ list.deprecatedField)
      }
      case map: MapType => {
        val flattenedField = optField("flattened", map.flattened)
        val fields = List(JField("type", JString("map")), JField("key", apply(map.key)), JField("value", apply(map.value))) ++ flattenedField
        JObject(map.locationFields ++ fields ++ map.deprecatedField)
      }
      case structure: StructureType => {
        val requiredField = if (structure.required.isEmpty) {
          List()
        } else {
          List(JField("required", JArray(structure.required.map { fieldName => JString(fieldName) })))
        }

        val membersField = if (structure.members.isEmpty) {
          List()
        } else {
          val memberFields = structure.members.toList.map {
            case (memberName, member) =>
              JField(memberName, apply(member))
          }

          List(JField("members", JObject(memberFields)))
        }

        val sensitiveField = optField("sensitive", structure.sensitive)
        val xmlNamespaceField = optField("xmlNamespace", structure.xmlNamespace)
        val xmlOrderField = optField("xmlOrder", structure.xmlOrder)
        val wrapperField = optField("wrapper", structure.wrapper)

        val fields = JField("type", JString("structure")) +: (requiredField ++ membersField ++ xmlNamespaceField ++ xmlOrderField ++ wrapperField)
        JObject(structure.locationFields ++ fields ++ structure.deprecatedField ++ sensitiveField)
      }
      case shape: ShapeType => {
        val xmlNamespaceField = optField("xmlNamespace", shape.xmlNamespace)
        val queryNameField = optField("queryName", shape.queryName)

        JObject(shape.locationFields ++ List(JField("shape", JString(shape.shape))) ++ xmlNamespaceField ++ queryNameField ++ shape.deprecatedField)
      }
      case boolean: BooleanType => {
        JObject(boolean.locationFields ++ List(JField("type", JString("boolean"))) ++ boolean.deprecatedField)
      }
      case integer: IntegerType => {
        JObject(integer.locationFields ++ List(JField("type", JString("integer"))) ++ integer.deprecatedField)
      }
      case long: LongType => {
        JObject(long.locationFields ++ List(JField("type", JString("long"))) ++ long.deprecatedField)
      }
      case float: FloatType => {
        JObject(float.locationFields ++ List(JField("type", JString("float"))) ++ float.deprecatedField)
      }
      case double: DoubleType => {
        JObject(double.locationFields ++ List(JField("type", JString("double"))) ++ double.deprecatedField)
      }
      case timestamp: TimestampType => {
        val timestampFormatField = optField("timestampFormat", timestamp.timestampFormat)
        val fields = List(JField("type", JString("timestamp"))) ++ timestampFormatField
        JObject(timestamp.locationFields ++ fields ++ timestamp.deprecatedField)
      }
      case blob: BlobType => {
        val streamingField = optField("streaming", blob.streaming)
        val sensitiveField = optField("sensitive", blob.sensitive)
        JObject(blob.locationFields ++ List(JField("type", JString("blob"))) ++ streamingField ++ sensitiveField ++ blob.deprecatedField)
      }
      case string: ExplicitStringType => {
        val typeField = Some(JField("type", JString("string")))
        val xmlAttributeField = optField("xmlAttribute", string.xmlAttribute)
        val sensitiveField = optField("sensitive", string.sensitive)
        val streamingField = optField("streaming", string.streaming)

        JObject(string.locationFields ++ List(typeField, xmlAttributeField, streamingField, sensitiveField).flatten ++ string.deprecatedField)
      }
      case string: DefaultStringType => {
        val xmlAttributeField = string.xmlAttribute.map { xmlAttribute =>
          JField("xmlAttribute", JBool(xmlAttribute))
        }

        val sensitiveField = string.sensitive.map { sensitive =>
          JField("sensitive", JBool(sensitive))
        }

        val streamingField = string.streaming.map { streaming =>
          JField("streaming", JBool(streaming))
        }

        JObject(string.locationFields ++ List(xmlAttributeField, streamingField, sensitiveField).flatten ++ string.deprecatedField)
      }
    }

    override def isDefinedAt(value: Any): Boolean = {
      value.isInstanceOf[AwsApiType]
    }
  }

  object Format extends CustomSerializer[AwsApiType](implicit format => (new Deserializer(), new Serializer()))
}