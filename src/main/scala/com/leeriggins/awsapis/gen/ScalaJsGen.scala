package com.leeriggins.awsapis.gen

import java.io.File
import com.leeriggins.awsapis.models._
import com.leeriggins.awsapis.models.AwsApiType._
import com.leeriggins.awsapis.parser._
import org.json4s.DefaultFormats

class ScalaJsGen(
    projectDir: File,
    api: Api) {

  private def kebab2camel(name : String): String = {
    def loop(x : List[Char]): List[Char] = (x: @unchecked) match {
      case '-' :: '-' :: rest => loop('-' :: rest)
      case '-' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
      case '-' :: Nil => Nil
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }
    if (name == null)
      ""
    else
      loop('-' :: name.toList).mkString
  }

  val rawServiceName: String = api.metadata.endpointPrefix.toLowerCase
  val scalaServiceName: String = cleanName(rawServiceName.replaceAll("-","_"))
  val serviceName = cleanName(kebab2camel(rawServiceName))
  val serviceAbbreviation = api.metadata.serviceAbbreviation.orElse(Some(api.metadata.serviceFullName)).map(_.replaceAll("Amazon ", "").replaceAll("AWS ", "").replaceAll(" ", ""))

  val sourceDir = new File(projectDir, "src/main/scala")
  val packageDir = new File(sourceDir, s"facade/amazonaws/services/${scalaServiceName}")

  val packageName = s"facade.amazonaws.services.${scalaServiceName}"
  val header =
    s"""package ${rawServiceName}
       |
       |import scalajs._
       |""".stripMargin

  private val useCompanionObjectExtensions = Set("AttributeValue")

  private def lower(str: String): String = {
    str.head.toLower +: str.tail
  }

  private def upper(str: String): String = {
    str.head.toUpper +: str.tail
  }

  def mkdirs(): Unit = {
    packageDir.mkdirs()
  }

  def gen(): Unit = {
    import java.io._

    mkdirs()
    val f = new File(packageDir, serviceName + ".scala")
    f.createNewFile()

    val writer = new PrintWriter(new FileWriter(f))
    try {
      val contents = genContents()
      writer.println(contents)
    } finally {
      writer.close()
    }
  }

  private def genContents(): String = {
    val shapeTypeRefs = api.shapes.flatMap {
      case (shapeName, shapeType) =>
        genShapeTypeRef(shapeName, shapeType).map { typeRef =>
          shapeName -> typeRef
        }
    }

    val allTypes = api.shapes.toIndexedSeq.sortBy(_._1).foldLeft(Map[String, String]()) {
      case (resolvedTypes, (shapeName, shapeType)) => {
        genTypesRecursive(shapeName, shapeType, resolvedTypes)
      }
    }

    s"""package facade.amazonaws.services
       |
       |import scalajs._
       |import scalajs.js.annotation.JSImport
       |import facade.amazonaws._
       |
       |package object ${scalaServiceName} {
       |${shapeTypeRefs.toIndexedSeq.sorted.map("  " + _._2).mkString("\n")}
       |}
       |
       |package ${scalaServiceName} {
       |${serviceDefinition()}
       |
       |${allTypes.toIndexedSeq.sorted.map(_._2).mkString("\n\n").split('\n').map { line => if (line.length > 0) "  " + line else line }.mkString("\n")}
       |}""".stripMargin
  }

  private def serviceDefinition(): String = {
    val footer = "}"

    val operations = api.operations.map {
      case (opName, operation) =>
        val outputType = operation.output.flatMap { output =>
          className(output.`type`)
        }.getOrElse("js.Object")

        val parameters = operation.input.map { input =>
          val inputName = className(input.`type`).get
          s"params: ${inputName}"
        }

        val withCallback = IndexedSeq(
          parameters,
          Some(s"callback: Callback[${outputType}]")).flatten.mkString(", ")
        s"""    def ${lower(opName)}(${parameters.getOrElse("")}): Request[${outputType}] = js.native"""

//        s"""    def ${lower(opName)}(${withCallback}): Unit = js.native
//           |    def ${lower(opName)}(${parameters.getOrElse("")}): Request[${outputType}] = js.native"""
    }

    val (serviceAbbreviation2, className2) = serviceAbbreviation match {
      case Some(x) => (x, cleanName(x))
      case None => ("<service class name unknown...>", upper(serviceName))
    }

    s"""  @js.native
       |  @JSImport("aws-sdk", "${serviceAbbreviation2}")
       |  class ${className2}(config: facade.amazonaws.AWSConfig) extends js.Object {
       |${operations.toIndexedSeq.sorted.mkString("\n")}
       |  }""".stripMargin
  }

  private def docsAndAnnotation(awsApiType: AwsApiType, isJsNative: Boolean = true): String = {
    val doc = awsApiType.documentation.map { documentation =>
      s"""/**
         | * ${documentation}
         | */""".stripMargin
    }

    val deprecation = awsApiType.deprecated.flatMap { dep =>
      if (dep) Some("@deprecated") else None
    }

    val jsNative = if (isJsNative) Some("@js.native") else None

    IndexedSeq(doc, deprecation, jsNative).flatten.mkString("\n")
  }

  private def className(awsApiType: AwsApiType): Option[String] = {
    awsApiType match {
      case _: StringType => Some("String")
      case _: LongType => Some("Double")
      case _: IntegerType => Some("Int")
      case _: FloatType => Some("Float")
      case _: DoubleType => Some("Double")
      case _: BooleanType => Some("Boolean")
      case _: TimestampType => Some("js.Date")
      case _: BlobType => Some("js.Array[Byte]")
      case _: EnumType => Some("String")
      case shape: ShapeType =>
        // FIXME
        shape.shape match {
          case "Long" => Some("Double")
          case _ => Some(shape.shape)
        }
      case map: MapType => Some(s"js.Dictionary[${className(map.value).get}]")
      case list: ListType => Some(s"js.Array[${className(list.member).get}]")
      case _ => None
    }
  }

  private def cleanName(name: String): String = {
    if (name.exists { char =>
      !char.isLetterOrDigit && char != '_'
    } || !name.head.isLetter
      || ScalaJsGen.scalaKeywords.contains(name)) {
      "`" + name + "`"
    } else {
      name
    }
  }

  private def genShapeTypeRef(shapeName: String, shapeType: AwsApiType): Option[String] = {
    className(shapeType).flatMap { className =>
      if (shapeName != className) {
        Some(s"""type ${shapeName} = ${className}""")
      } else {
        None
      }
    }
  }

  /** Adds the new type recursively to the previously resolved types. */
  private def genTypesRecursive(name: String, definition: AwsApiType, resolvedTypes: Map[String, String] = Map()): Map[String, String] = {
    if (resolvedTypes.contains(name)) {
      return resolvedTypes
    }

    val typeName = className(definition).getOrElse(name)

    definition match {
      case _: StringType |
        _: LongType |
        _: IntegerType |
        _: FloatType |
        _: DoubleType |
        _: BooleanType |
        _: TimestampType |
        _: BlobType => {
        // true primitive types require no recursive generation

        resolvedTypes
      }
      case enum: EnumType => {
        // enums are just strings in the AWS API

        val symbolMap = enum.symbols.map { symbol =>
          cleanName(symbol) -> symbol
        }
        val symbolDefinitions = symbolMap.map {
          case (symbolName, symbol) =>
            s"""  val ${symbolName} = "${symbol}""""
        }

        val valuesList = s"""  val values = IndexedSeq(${symbolMap.map(_._1).mkString(", ")})"""

        val enumDefinition =
          s"""${docsAndAnnotation(enum, isJsNative = false)}
             |object ${name}Enum {
             |${symbolDefinitions.mkString("\n")}
             |
             |${valuesList}
             |}""".stripMargin

        resolvedTypes + (name -> (enumDefinition))
      }
      case error: ErrorType => {
        val withMemberTypes = error.members.map(_.foldLeft(resolvedTypes) {
          case (types, (memberName, memberType)) =>
            genTypesRecursive(memberName, memberType, types)
        }).getOrElse(resolvedTypes)

        val memberFields = error.members.map { members =>
          members.map {
            case (memberName, memberType) =>
              s"""  val ${cleanName(memberName)}: ${className(memberType).getOrElse(memberName)}"""
          }.mkString("\n")
        }.getOrElse("")

        val errorDefinition =
          s"""${docsAndAnnotation(error)}
             |trait ${typeName}Exception extends js.Object {
             |${memberFields}
             |}""".stripMargin.trim

        withMemberTypes + (typeName -> errorDefinition)
      }
      case list: ListType => {
        genTypesRecursive(name + "Item", list.member, resolvedTypes)
      }
      case map: MapType => {
        val withKey = genTypesRecursive(name + "Key", map.key, resolvedTypes)
        val withValue = genTypesRecursive(name + "Value", map.value, withKey)

        withValue
      }
      case structure: StructureType => {
        val withMemberTypes = structure.members.map(_.foldLeft(resolvedTypes) {
          case (types, (memberName, memberType)) =>
            genTypesRecursive(memberName, memberType, types)
        }).getOrElse(resolvedTypes)

        val memberFields = structure.members.map { members =>
          members.map {
            case (memberName, memberType) =>
              s"""  var ${cleanName(memberName)}: js.UndefOr[${className(memberType).getOrElse(memberName)}]"""
          }.mkString("\n")
        }.getOrElse("")

        val constructorArgs = structure.members.map { members =>
          members.map {
            case (memberName, memberType) =>
              s"""    ${cleanName(memberName)}: js.UndefOr[${className(memberType).getOrElse(memberName)}] = js.undefined"""
          }.mkString(",\n")
        }.getOrElse("")
        
        val fieldMapping = structure.members.map { members =>
          members.map {
            case (memberName, memberType) =>
              s"""      "${cleanName(memberName)}" -> ${cleanName(memberName)}.map { x => x: js.Any }"""
          }.mkString(",\n")
        }.getOrElse("")

        val applyDeprecated = if (useCompanionObjectExtensions.contains(typeName)) {
            s"""@deprecated("Use the extension methods by importing ${typeName}Extensions._ instead.")"""
        } else {
            ""
        }
        val structureDefinition =
          s"""${docsAndAnnotation(structure)}
             |trait ${typeName} extends js.Object {
             |${memberFields}
             |}
             |
             |object ${typeName} {
             |  ${applyDeprecated} def apply(
             |${constructorArgs}
             |  ): ${typeName} = {
             |    val _fields = IndexedSeq[(String, js.Any)](
             |${fieldMapping}
             |    ).filter(_._2 != (js.undefined : js.Any))
             |
             |    js.Dynamic.literal.applyDynamicNamed("apply")(_fields: _*).asInstanceOf[${typeName}]
             |  }
             |}""".stripMargin.trim


        withMemberTypes + (typeName -> structureDefinition)
      }
      case shape: ShapeType => {
        resolvedTypes
      }
    }
  }

}

object ScalaJsGen {
  val scalaKeywords = Set(
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "for",
    "if",
    "implicit",
    "match",
    "new",
    "null",
    "print",
    "printf",
    "println",
    "private",
    "protected",
    "public",
    "return",
    "throw",
    "trait",
    "true",
    "type",
    "try",
    "val",
    "var",
    "while",
    "with")

  def main(args: Array[String]): Unit = {
    import Apis._
    import org.json4s._
    import org.json4s.Extraction._
    import org.json4s.jackson.JsonMethods._
    import java.io._

    implicit val formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format

    val projectDir = new File("../aws-sdk-scalajs-facade/")
    if (!projectDir.exists()) {
      projectDir.mkdirs()
    }

    val apiVersions = com.leeriggins.awsapis.Apis.versions

    apiVersions.foreach {
      case (name, version) =>
        val text = json(name, version, ApiType.normal)
        val parsedText = parse(text)

        val api = parsedText.extract[Api]

        val gen = new ScalaJsGen(projectDir, api)
        gen.gen()
    }
  }
}
