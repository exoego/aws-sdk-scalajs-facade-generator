package com.leeriggins.awsapis.gen

import java.io.File

import com.leeriggins.awsapis.models._
import com.leeriggins.awsapis.models.AwsApiType._
import com.leeriggins.awsapis.parser._

class ScalaJsGen(projectDir: File, api: Api) {
  private val scalaServiceName: String = api.serviceClassName.toLowerCase

  val sourceDir  = new File(projectDir, "src/main/scala")
  val packageDir = new File(sourceDir, s"facade/amazonaws/services")

  private def lowerFirst(str: String): String = {
    str.head.toLower +: str.tail
  }

  def mkdirs(): Unit = {
    packageDir.mkdirs()
    ()
  }

  def gen(): Unit = {
    import java.io._

    mkdirs()
    val f = new File(packageDir, api.serviceClassName + ".scala")
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
    val shapeTypeRefs = api.shapes.toSeq.flatMap {
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
       |import scala.concurrent.Future
       |import io.scalajs.nodejs
       |import facade.amazonaws._
       |
       |package object ${scalaServiceName} {
       |${shapeTypeRefs.toIndexedSeq.sorted.map("  " + _._2).mkString("\n")}
       |
       |${futureExtensionMethodDefinition()}
       |}
       |
       |package ${scalaServiceName} {
       |${serviceDefinition()}
       |
       |${allTypes.toIndexedSeq.sorted
         .map { case (_, resolvedType) => resolvedType }
         .mkString("\n\n")
         .split('\n')
         .map { line =>
           if (line.length > 0) "  " + line else line
         }
         .mkString("\n")}
       |}""".stripMargin
  }

  private def futureExtensionMethodDefinition(): String = {
    val operations = api.operations.toSeq.map {
      case (opName, operation) =>
        val outputType = operation.output
          .flatMap { output =>
            className(output.apiType)
          }
          .getOrElse("js.Object")

        val parameters = operation.input.fold("") { input =>
          val inputName = className(input.apiType).get
          s"params: ${inputName}"
        }

        val methodName = lowerFirst(opName)
        val arg        = operation.input.map(_ => "params").getOrElse("")
        // Don't generate extension method for deprecated API
        operation.deprecated match {
          case Some(true) => ""
          case _ =>
            s"  def ${methodName}Future(${parameters}): Future[${outputType}] = service.${methodName}(${arg}).promise.toFuture"
        }
    }

    s"""  implicit final class ${api.serviceClassName}Ops(val service: ${api.serviceClassName}) extends AnyVal {
       |
       |${operations.toIndexedSeq.sorted.mkString("\n")}
       |  }""".stripMargin
  }

  private def serviceDefinition(): String = {
    val operations = api.operations.toSeq.map {
      case (opName, operation) =>
        val outputType = operation.output
          .flatMap { output =>
            className(output.apiType)
          }
          .getOrElse("js.Object")

        val parameters = operation.input.fold("") { input =>
          val inputName = className(input.apiType).get
          s"params: ${inputName}"
        }
        val deprecated = operation.deprecated.fold("") { dep =>
          if (dep) s"""@deprecated("${operation.deprecatedMessage.getOrElse("Deprecated in AWS SDK")}", "forever")"""
          else ""
        }

        s"""  ${deprecated} def ${lowerFirst(opName)}(${parameters}): Request[${outputType}] = js.native"""
    }

    s"""  @js.native
       |  @JSImport("aws-sdk", "${api.sdkClassName}")
       |  class ${api.serviceClassName}() extends js.Object {
       |    def this(config: AWSConfig) = this()
       |
       |${operations.toIndexedSeq.sorted.mkString("\n")}
       |  }""".stripMargin
  }

  private final val seeAlso               = """<div class="seeAlso">\s*(.+)\s*</div>""".r
  private final val fieldReference        = "<a>(\\w+)\\$(\\w+)</a>".r
  private final val boldText              = "<b>([^<]+)</b>".r
  private final val headings              = "<h4(?:[^>]*)>([^<]+)</h4>".r
  private final val subheadings           = "<h5(?:[^>]*)>([^<]+)</h5>".r
  private final val externalLinkReference = """<a href="([^"]+)">([^<]+)</a>""".r
  private final val codeBlock             = """<pre><code>(.+?)</code></pre>""".r
  private final val listItemPattern       = "<li>\\s*(.*?)\\s*</li>".r
  private final val paragraphPattern      = "<p>\\s*(.*?)\\s*</p>".r
  private final val notePattern           = "<note>\\s*".r
  private final val removeTagPattern      = "\\s*</?(ul|note)>\\s*".r

  private def docsAndAnnotation(awsApiType: AwsApiType, typeName: String, isJsNative: Boolean = true): String = {
    val doc = awsApiType.documentation.filter(_ != s"<p>${typeName}</p>").map { documentation =>
      val reps: Seq[String => String] = Seq(
        doc =>
          fieldReference.replaceAllIn(doc, matched => {
            matched.group(1) match {
              case `typeName` => s"${matched.group(2)}"
              case _          => s"[[${matched.group(1)}.${matched.group(2)}]]"
            }
          }),
        doc => doc.replaceAllLiterally("$", ""),
        doc => notePattern.replaceAllIn(doc, "\n'''Note:'''"),
        doc => boldText.replaceAllIn(doc, matched => s"```${matched.group(1)}```"),
        doc => headings.replaceAllIn(doc, matched => s"\n=${matched.group(1)}=\n"),
        doc => subheadings.replaceAllIn(doc, matched => s"\n==${matched.group(1)}==\n"),
        doc => codeBlock.replaceAllIn(doc, matched => s"{{{${matched.group(1)}}}}"),
        doc => externalLinkReference.replaceAllIn(doc, matched => s"[[${matched.group(1)}|${matched.group(2)}]]"),
        doc => seeAlso.replaceAllIn(doc, matched => s"\n@see ${matched.group(1)}"),
        doc => removeTagPattern.replaceAllIn(doc, _ => s""),
        doc => listItemPattern.replaceAllIn(doc, matched => s"* ${matched.group(1)}\n"),
        doc => paragraphPattern.replaceAllIn(doc, matched => s"${matched.group(1)}\n")
      )
      val formattedDoc = reps.foldLeft(documentation) { case (d, pair) => pair(d) }.split("\n").filter(_.nonEmpty)
      formattedDoc.mkString(sep = "\n * ", start = "/**\n * ", end = "\n */")
    }

    val deprecation = awsApiType.deprecated.flatMap { dep =>
      if (dep) Some(s"""@deprecated("${awsApiType.deprecatedMessage.getOrElse("Deprecated in AWS SDK")}", "forever")""")
      else None
    }

    val jsNative = if (isJsNative) Some("@js.native") else None

    IndexedSeq(doc, deprecation, jsNative).flatten.mkString("\n")
  }

  private def className(awsApiType: AwsApiType): Option[String] = {
    awsApiType match {
      case _: StringType    => Some("String")
      case _: LongType      => Some("Double")
      case _: IntegerType   => Some("Int")
      case _: FloatType     => Some("Float")
      case _: DoubleType    => Some("Double")
      case _: BooleanType   => Some("Boolean")
      case _: TimestampType => Some("js.Date")
      case _: BlobType =>
        Some("nodejs.buffer.Buffer | nodejs.stream.Readable | js.typedarray.TypedArray[_, _] | js.Array[Byte] | String")
      case _: EnumType      => Some("String")
      case shape: ShapeType => primitive2Scala.get(shape.shape).orElse(Some(shape.shape))
      case map: MapType     => Some(s"js.Dictionary[${className(map.value).get}]")
      case list: ListType   => Some(s"js.Array[${className(list.member).get}]")
      case _                => None
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

  private def genShapeTypeRef(shapeName: String, shapeType: AwsApiType): Option[String] = {
    className(shapeType).flatMap { className =>
      if (shapeName != className && !primitive2Scala.contains(shapeName)) {
        Some(s"""type ${shapeName} = ${className}""")
      } else {
        None
      }
    }
  }

  /** Adds the new type recursively to the previously resolved types. */
  private def genTypesRecursive(name: String,
                                definition: AwsApiType,
                                resolvedTypes: Map[String, String]): Map[String, String] = {
    if (resolvedTypes.contains(name)) {
      return resolvedTypes
    }

    val typeName = className(definition).getOrElse(name)

    definition match {
      case _: StringType | _: LongType | _: IntegerType | _: FloatType | _: DoubleType | _: BooleanType |
          _: TimestampType | _: BlobType => {
        // true primitive types require no recursive generation

        resolvedTypes
      }
      case enum: EnumType   => genEnumType(enum, name, typeName, resolvedTypes)
      case error: ErrorType => genErrorType(error, typeName, resolvedTypes)
      case list: ListType   => genTypesRecursive(name + "Item", list.member, resolvedTypes)
      case map: MapType => {
        val withKey   = genTypesRecursive(name + "Key", map.key, resolvedTypes)
        val withValue = genTypesRecursive(name + "Value", map.value, withKey)
        withValue
      }
      case structure: StructureType => genStructureType(structure, typeName, resolvedTypes)
      case _: ShapeType             => resolvedTypes
    }
  }

  private def genEnumType(enum: EnumType, name: String, typeName: String, resolvedTypes: Map[String, String]) = {
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
      s"""${docsAndAnnotation(enum, typeName, isJsNative = false)}
         |object ${name}Enum {
         |${symbolDefinitions.mkString("\n")}
         |
             |${valuesList}
         |}""".stripMargin

    resolvedTypes + (name -> (enumDefinition))
  }

  private def genErrorType(error: ErrorType, typeName: String, resolvedTypes: Map[String, String]) = {
    val withMemberTypes = error.members.fold(resolvedTypes)(_.foldLeft(resolvedTypes) {
      case (types, (memberName, memberType)) =>
        genTypesRecursive(memberName, memberType, types)
    })

    val memberFields = error.members.fold("")(_.map {
      case (memberName, memberType) =>
        s"""  val ${cleanName(memberName)}: ${className(memberType).getOrElse(memberName)}"""
    }.mkString("\n"))

    val errorDefinition =
      s"""${docsAndAnnotation(error, typeName)}
         |trait ${typeName}Exception extends js.Object {
         |${memberFields}
         |}""".stripMargin.trim

    withMemberTypes + (typeName -> errorDefinition)
  }

  private def genStructureMemberFields(sortedMembers: Option[Seq[(String, AwsApiType)]],
                                       requiredFields: Set[String]) = {
    sortedMembers.fold("")(_.map {
      case (memberName, memberType) =>
        val memberType_ = if (requiredFields(memberName)) {
          s"${className(memberType).getOrElse(memberName)}"
        } else {
          s"js.UndefOr[${className(memberType).getOrElse(memberName)}]"
        }
        s"""  var ${cleanName(memberName)}: ${memberType_}"""
    }.mkString("\n"))
  }

  private def genStructureConstructorArgs(sortedMembers: Option[Seq[(String, AwsApiType)]],
                                          requiredFields: Set[String]) = {
    sortedMembers.fold("")(_.map {
      case (memberName, memberType) =>
        val memberType_ = if (requiredFields(memberName)) {
          s"${className(memberType).getOrElse(memberName)}"
        } else {
          s"js.UndefOr[${className(memberType).getOrElse(memberName)}] = js.undefined"
        }
        s"""    ${cleanName(memberName)}: ${memberType_}"""
    }.mkString(",\n"))
  }

  private def genStructureFieldMapping(sortedMembers: Option[Seq[(String, AwsApiType)]],
                                       requiredFields: Set[String]) = {
    sortedMembers.fold("")(_.map {
      case (memberName, _) =>
        val memberType_ = if (requiredFields(memberName)) {
          s"${cleanName(memberName)}.asInstanceOf[js.Any]"
        } else {
          s"${cleanName(memberName)}.map { x => x.asInstanceOf[js.Any] }"
        }
        s"""      "${cleanName(memberName)}" -> ${memberType_}"""
    }.mkString(",\n"))
  }

  private def requiredAndAlphabetical(requiredFields: Set[String]): ((String, AwsApiType)) => (Boolean, String) = {
    case (memberName, _) =>
      (
        !requiredFields(memberName), // required member first, optional second
        memberName                   // alphabetical
      )
  }

  private def genStructureType(structure: StructureType, typeName: String, resolvedTypes: Map[String, String]) = {
    val withMemberTypes = structure.members.fold(resolvedTypes)(_.foldLeft(resolvedTypes) {
      case (types, (memberName, memberType)) =>
        genTypesRecursive(memberName, memberType, types)
    })
    val requiredFields  = structure.required.map(_.toSet).getOrElse(Set.empty[String])
    val sortedMembers   = structure.members.map(_.toSeq.sortBy(requiredAndAlphabetical(requiredFields)))
    val memberFields    = genStructureMemberFields(sortedMembers, requiredFields)
    val constructorArgs = genStructureConstructorArgs(sortedMembers, requiredFields)
    val fieldMapping    = genStructureFieldMapping(sortedMembers, requiredFields)
    val traitDefinition =
      s"""${docsAndAnnotation(structure, typeName)}
         |trait ${typeName} extends js.Object {
         |${memberFields}
         |}""".stripMargin
    val insertFile = new File(s"src/main/resources/${api.serviceClassName}", s"${typeName}.scala")
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
         |  def apply(
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
    "with"
  )

  def generatePackageFile(): Unit = {
    import Apis._
    import org.json4s._
    import org.json4s.jackson.JsonMethods._
    import java.io._

    implicit val formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format

    val projectDir = new File("../aws-sdk-scalajs-facade")
    val allDir     = new File(projectDir, "all")
    if (!allDir.exists()) {
      projectDir.mkdirs()
    }

    val apiVersions    = com.leeriggins.awsapis.Apis.versions
    val allPackageRoot = new File(allDir, s"src/main/scala/facade/amazonaws")
    allPackageRoot.mkdirs()
    val awsFile = new File(allPackageRoot, s"package.scala")
    awsFile.createNewFile()
    val awsWriter = new PrintWriter(new FileWriter(awsFile))

    val types = apiVersions
      .map {
        case (name, version) =>
          val text       = json(name, version, ApiType.normal)
          val parsedText = parse(text)
          val api        = parsedText.extract[Api]
          val subDir     = new File(projectDir, s"services/${api.serviceClassName.toLowerCase()}")
          val gen        = new ScalaJsGen(subDir, api)
          gen.gen()

          val qualifiedName = s"services.${gen.scalaServiceName}.${api.serviceClassName}"
          s"""    type ${api.serviceClassName} = ${qualifiedName}
             |    def ${api.serviceClassName}(): ${qualifiedName} = new ${qualifiedName}()
             |    def ${api.serviceClassName}(config: AWSConfig): ${qualifiedName} = new ${qualifiedName}(config)
             |""".stripMargin
      }
      .mkString("\n")

    try {
      awsWriter.append(s"""package facade
                          |
                          |package object amazonaws {
                          |  implicit final class AWSExtensionMethods(val aws: AWS.type) extends AnyVal {
                          |    def config_=(config: AWSConfig): Unit = {
                          |      aws.config = config match {
                          |        case global: AWSConfigWithServicesDefault => global
                          |        case _                                    => config.asInstanceOf[AWSConfigWithServicesDefault]
                          |      }
                          |    }
                          |
                          |${types}
                          |  }
                          |}
         """.stripMargin.trim)
      ()
    } finally {
      awsWriter.close()
    }
  }

  def generateAWSConfigWithServicesDefault(): Unit = {
    import Apis._
    import org.json4s._
    import org.json4s.jackson.JsonMethods._
    import java.io._

    implicit val formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format

    val projectDir = new File("../aws-sdk-scalajs-facade/core")
    if (!projectDir.exists()) {
      projectDir.mkdirs()
    }

    val apiVersions    = com.leeriggins.awsapis.Apis.versions
    val packageRootDir = new File(projectDir, s"src/main/scala/facade/amazonaws")
    packageRootDir.mkdirs()
    val awsFile = new File(packageRootDir, s"AWSConfigWithServicesDefault.scala")
    awsFile.createNewFile()
    val awsWriter = new PrintWriter(new FileWriter(awsFile))

    val types = apiVersions
      .map {
        case (name, version) =>
          val text       = json(name, version, ApiType.normal)
          val parsedText = parse(text)
          val api        = parsedText.extract[Api]
          api.sdkClassName.toLowerCase
      }
      .sorted
      .map { sdkClassName =>
        s"  var ${sdkClassName}: js.UndefOr[ParamsWithEndpoint] = js.undefined"
      }
      .mkString("\n")

    try {
      awsWriter.append(s"""package facade.amazonaws
                          |
                          |import scala.scalajs.js
                          |
                          |class AWSConfigWithServicesDefault extends AWSConfig {
                          |${types}
                          |}
                          |""".stripMargin.trim)
      ()
    } finally {
      awsWriter.close()
    }
  }

  def main(args: Array[String]): Unit = {
    generatePackageFile()
    generateAWSConfigWithServicesDefault()
  }
}
