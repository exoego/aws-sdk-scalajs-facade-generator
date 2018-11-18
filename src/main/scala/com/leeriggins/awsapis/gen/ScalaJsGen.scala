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

  private val serviceClassName = api.metadata.serviceId.replaceAll(" ", "") match {
    // special treatment
    case "SFN" => "StepFunctions"
    case "ElasticLoadBalancing" => "ELB"
    case "Budgets" => "BudgetsService"
    case otherwise => otherwise
  }
  private val sdkClassName = serviceClassName match {
    case "BudgetsService" => "Budgets"
    case "CognitoIdentityProvider" => "CognitoIdentityServiceProvider"
    case otherwise => otherwise
  }
  private val scalaServiceName: String = serviceClassName.toLowerCase

  val sourceDir = new File(projectDir, "src/main/scala")
  val packageDir = new File(sourceDir, s"facade/amazonaws/services")

  private def lowerFirst(str: String): String = {
    str.head.toLower +: str.tail
  }

  private def upperFirst(str: String): String = {
    str.head.toUpper +: str.tail
  }

  def mkdirs(): Unit = {
    packageDir.mkdirs()
  }

  def gen(): Unit = {
    import java.io._

    mkdirs()
    val f = new File(packageDir, serviceClassName + ".scala")
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
       |import scala.scalajs.js.|
       |import io.scalajs.nodejs
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
    val operations = api.operations.map {
      case (opName, operation) =>
        val outputType = operation.output.flatMap { output =>
          className(output.`type`)
        }.getOrElse("js.Object")

        val parameters = operation.input.fold("") { input =>
          val inputName = className(input.`type`).get
          s"params: ${inputName}"
        }

        s"""    def ${lowerFirst(opName)}(${parameters}): Request[${outputType}] = js.native"""
    }

    s"""  @js.native
       |  @JSImport("aws-sdk", "${sdkClassName}")
       |  class ${serviceClassName}(config: facade.amazonaws.AWSConfig) extends js.Object {
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
      case _: BlobType => Some("nodejs.buffer.Buffer | nodejs.stream.Readable | js.typedarray.TypedArray[_, _] | js.Array[Byte] | String")
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
        val withMemberTypes = error.members.fold(resolvedTypes)(_.foldLeft(resolvedTypes) {
          case (types, (memberName, memberType)) =>
            genTypesRecursive(memberName, memberType, types)
        })

        val memberFields = error.members.fold("") { members =>
          members.map {
            case (memberName, memberType) =>
              s"""  val ${cleanName(memberName)}: ${className(memberType).getOrElse(memberName)}"""
          }.mkString("\n")
        }

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
        val withMemberTypes = structure.members.fold(resolvedTypes)(_.foldLeft(resolvedTypes) {
          case (types, (memberName, memberType)) =>
            genTypesRecursive(memberName, memberType, types)
        })

        val memberFields = structure.members.fold("") { members =>
          members.map {
            case (memberName, memberType) =>
              s"""  var ${cleanName(memberName)}: js.UndefOr[${className(memberType).getOrElse(memberName)}]"""
          }.mkString("\n")
        }

        val constructorArgs = structure.members.fold("") { members =>
          members.map {
            case (memberName, memberType) =>
              s"""    ${cleanName(memberName)}: js.UndefOr[${className(memberType).getOrElse(memberName)}] = js.undefined"""
          }.mkString(",\n")
        }
        
        val fieldMapping = structure.members.fold("") { members =>
          members.map {
            case (memberName, memberType) =>
              s"""      "${cleanName(memberName)}" -> ${cleanName(memberName)}.map { x => x.asInstanceOf[js.Any] }"""
          }.mkString(",\n")
        }

        val traitDefinition =
          s"""${docsAndAnnotation(structure)}
             |trait ${typeName} extends js.Object {
             |${memberFields}
             |}""".stripMargin

        val insertFile = new File(s"src/main/resources/${serviceClassName}", s"${typeName}.scala")
        val insertContent = if (insertFile.exists()) {
          val source = io.Source.fromFile(insertFile, "UTF-8")
          try {
            source.mkString
          } finally {
            source.close()
          }
        } else ""

        val structureDefinition =
          s"""${traitDefinition}
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
             |${insertContent}}""".stripMargin

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

    val packageRootDir = new File(projectDir, s"src/main/scala/facade/amazonaws")
    packageRootDir.mkdirs()
    val awsFile = new File(packageRootDir, s"AWS.scala")
    awsFile.createNewFile()
    val awsWriter = new PrintWriter(new FileWriter(awsFile))

    try {
      awsWriter.append(
        s"""package facade.amazonaws
           |
           |object AWS {
         """.stripMargin.trim)
      awsWriter.println()

      apiVersions.foreach {
        case (name, version) =>
          val text = json(name, version, ApiType.normal)
          val parsedText = parse(text)

          val api = parsedText.extract[Api]

          val gen = new ScalaJsGen(projectDir, api)
          gen.gen()

          val qualifiedName = s"services.${gen.scalaServiceName}.${gen.serviceClassName}"
          awsWriter.println(s"  def ${gen.serviceClassName}(options: AWSConfig = AWSConfig()): ${qualifiedName} = new ${qualifiedName}(options)")
      }
      awsWriter.println("}")
    } finally {
      awsWriter.close()
    }
  }
}
