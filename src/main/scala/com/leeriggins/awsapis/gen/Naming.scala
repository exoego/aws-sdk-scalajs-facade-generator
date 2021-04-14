package com.leeriggins.awsapis.gen

import com.leeriggins.awsapis.models.AwsApiType._
import com.leeriggins.awsapis.models._

object Naming {
  private val scalaKeywords: Set[String] = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "print",
    "printf",
    "println",
    "private",
    "protected",
    "public",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "true",
    "try",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  private val primitive2Scala: Map[String, String] = Map(
    "bool"    -> "Boolean",
    "Bool"    -> "Boolean",
    "boolean" -> "Boolean",
    "float"   -> "Float",
    "int"     -> "Int",
    "integer" -> "Int",
    "Integer" -> "Int",
    "long"    -> "Double",
    "Long"    -> "Double",
    "string"  -> "String"
  )

  def cleanName(name: String): String = {
    if (
      name.exists { char =>
        !char.isLetterOrDigit && char != '_'
      } || !name.head.isLetter
      || Naming.scalaKeywords.contains(name)
    ) {
      "`" + name + "`"
    } else {
      name
    }
  }

  def className(awsApiType: AwsApiType): Option[String] = {
    awsApiType match {
      case _: StringType    => Some("String")
      case _: LongType      => Some("Double")
      case _: IntegerType   => Some("Int")
      case _: FloatType     => Some("Float")
      case _: DoubleType    => Some("Double")
      case _: BooleanType   => Some("Boolean")
      case _: TimestampType => Some("js.Date")
      case _: BlobType      => Some("js.typedarray.TypedArray[_, _] | js.Array[Byte] | String")
      case _: EnumType      => None
      case shape: ShapeType => primitive2Scala.get(shape.shape).orElse(Some(shape.shape))
      case map: MapType     => Some(s"js.Dictionary[${className(map.value).get}]")
      case list: ListType   => Some(s"js.Array[${className(list.member).get}]")
      case _                => None
    }
  }

  def isAwsPrimitiveShape(shapeName: String): Boolean = primitive2Scala.contains(shapeName)
}
