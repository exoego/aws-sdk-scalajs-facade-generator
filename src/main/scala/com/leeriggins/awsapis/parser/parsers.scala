package com.leeriggins.awsapis.parser

import org.json4s._
import com.leeriggins.awsapis.models._
import com.leeriggins.awsapis.models.AwsApiType._

object AwsApiTypeDeserializer extends PartialFunction[JValue, AwsApiType] {
  private implicit class FieldsImplicits(fields: List[JField]) {
    def hasFieldValue(fieldName: String, fieldValue: String): Boolean = {
      fields.exists {
        case JField(name, JString(value)) => (name == fieldName && value == fieldValue)
        case _ => false
      }
    }

    def hasTypeValue(value: String): Boolean = this.hasFieldValue("type", value)

    def hasShape(): Boolean = fields.exists(_._1 == "shape")

    def getFieldValue(fieldName: String): Option[JValue] = {
      fields.find { case JField(name, _) => name == fieldName }.map(_._2)
    }
  }

  override def apply(value: JValue): AwsApiType = {
    value match {
      case JObject(fields) if (fields.hasShape) => {
        val shape = fields.getFieldValue("shape") match {
          case Some(JString(shapeValue)) => shapeValue
          case _ => ???
        }
        ShapeType(shape)
      }

      case JObject(fields) if (fields.hasTypeValue("structure")) => {
        val required = fields.getFieldValue("required").map {
          _ match {
            case JArray(values) => {
              values.map {
                _ match {
                  case JString(value) => value
                  case _ => ???
                }
              }
            }
            case _ => ???
          }
        }.getOrElse(List())

        val members = fields.getFieldValue("members").map {
          _ match {
            case JObject(memberFields) => {
              memberFields.map {
                case (fieldName, fieldValue) => fieldName -> apply(fieldValue)
              }
            }
            case _ => ???
          }
        }.getOrElse(List())

        StructureType(required, Map(members: _*))
      }

      case JObject(fields) if (fields.hasTypeValue("list")) => {
        ListType(apply(fields.getFieldValue("member").get))
      }

      case JObject(fields) if (fields.hasTypeValue("integer")) => {
        IntegerType
      }

      case JObject(fields) if (fields.hasTypeValue("timestamp")) => {
        TimestampType
      }

      case JObject(fields) if (fields.hasTypeValue("boolean")) => {
        BooleanType
      }

      case JObject(Nil) => {
        StringType
      }

      case _ => {
        ???
      }
    }
  }

  // just check for "shape" or "type" field
  override def isDefinedAt(value: JValue): Boolean = {
    value match {
      case JObject(fields) if (fields.isEmpty || fields.exists { field => field._1 == "shape" || field._1 == "type" }) => true
      case _ => false
    }
  }
}

object AwsApiTypeSerializer extends PartialFunction[Any, JObject] {
  override def apply(value: Any): JObject = value match {
    case list: ListType => {
      JObject(List(JField("type", JString("list")), JField("member", apply(list.member))))
    }
    case structure: StructureType => {
      val requiredField = if (structure.required.isEmpty) {
        None
      } else {
        Some(JField("required", JArray(structure.required.map { fieldName => JString(fieldName) })))
      }

      val membersField = if (structure.members.isEmpty) {
        None
      } else {
        val memberFields = structure.members.toList.map {
          case (memberName, member) =>
            JField(memberName, apply(member))
        }

        Some(JField("members", JObject(memberFields)))
      }

      JObject(JField("type", JString("structure")) :: List(requiredField, membersField).flatten)
    }
    case shape: ShapeType => JObject(JField("shape", JString(shape.shape)))
    case BooleanType => JObject(JField("type", JString("boolean")))
    case IntegerType => JObject(JField("type", JString("integer")))
    case TimestampType => JObject(JField("type", JString("timestamp")))
    case StringType => JObject(Nil)
  }

  override def isDefinedAt(value: Any): Boolean = {
    value.isInstanceOf[AwsApiType]
  }
}

object AwsApiTypeSerDe extends CustomSerializer[AwsApiType](format => (AwsApiTypeDeserializer, AwsApiTypeSerializer))
