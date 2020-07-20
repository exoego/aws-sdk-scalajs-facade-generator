package com.leeriggins.awsapis.gen

import java.io._
import java.nio.file.{Files, Path, Paths}
import java.util.stream.Collectors

import scala.jdk.CollectionConverters._

import com.leeriggins.awsapis.models._
import com.leeriggins.awsapis.models.AwsApiType._
import com.leeriggins.awsapis.parser._

import Apis._
import org.json4s._
import org.json4s.jackson.JsonMethods._

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

    val insertFile = new File(s"src/main/resources/${api.serviceClassName}", s"package_extension.scala")
    val topLevelInsertion = if (insertFile.exists()) {
      val source = io.Source.fromFile(insertFile, "UTF-8")
      try {
        source.mkString
      } finally {
        source.close()
      }
    } else ""

    s"""package facade.amazonaws.services
       |
       |import scalajs._
       |import scalajs.js.annotation.JSImport
       |import scala.scalajs.js.|
       |import scala.concurrent.Future
       |import facade.amazonaws._
       |
       |package object ${scalaServiceName} {
       |${shapeTypeRefs.toIndexedSeq.sorted.map("  " + _._2).mkString("\n")}
       |
       |${futureExtensionMethodDefinition()}
       |$topLevelInsertion}
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
            s"  @inline def ${methodName}Future(${parameters}): Future[${outputType}] = service.${methodName}(${arg}).promise().toFuture"
        }
    }

    val insertFile = new File(s"src/main/resources/${api.serviceClassName}", s"ops_extension.scala")
    val opsExtension = if (insertFile.exists()) {
      val source = io.Source.fromFile(insertFile, "UTF-8")
      try {
        source.mkString
      } finally {
        source.close()
      }
    } else ""

    s"""  implicit final class ${api.serviceClassName}Ops(private val service: ${api.serviceClassName}) extends AnyVal {
       |
       |${operations.toIndexedSeq.sorted.mkString("\n")}
       |${opsExtension}}""".stripMargin
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
       |  @JSImport("aws-sdk", "${api.sdkClassName}", "AWS.${api.sdkClassName}")
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
          fieldReference.replaceAllIn(
            doc,
            matched => {
              matched.group(1) match {
                case `typeName` => s"${matched.group(2)}"
                case _          => s"[[${matched.group(1)}.${matched.group(2)}]]"
              }
            }
          ),
        doc => doc.replace("$", ""),
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
      case _: BlobType      => Some("js.typedarray.TypedArray[_, _] | js.Array[Byte] | String")
      case _: EnumType      => None
      case shape: ShapeType => primitive2Scala.get(shape.shape).orElse(Some(shape.shape))
      case map: MapType     => Some(s"js.Dictionary[${className(map.value).get}]")
      case list: ListType   => Some(s"js.Array[${className(list.member).get}]")
      case _                => None
    }
  }

  private def cleanName(name: String): String = {
    if (
      name.exists { char =>
        !char.isLetterOrDigit && char != '_'
      } || !name.head.isLetter
      || ScalaJsGen.scalaKeywords.contains(name)
    ) {
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
        val deprecated = shapeType.deprecated.contains(true) match {
          case true =>
            val message = shapeType.deprecatedMessage.getOrElse("Deprecated in AWS SDK")
            s"""@deprecated("${message}", "forever")
               |""".stripMargin
          case false => ""
        }
        Some(s"""${deprecated} type ${shapeName} = ${className}""")
      } else {
        None
      }
    }
  }

  /** Adds the new type recursively to the previously resolved types. */
  private def genTypesRecursive(name: String,
                                definition: AwsApiType,
                                resolvedTypes: Map[String, String]
  ): Map[String, String] = {
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
        s"""  @inline val ${symbolName} = "${symbol}".asInstanceOf[${name}]"""
    }

    val valuesList = s"""  @inline val values = js.Object.freeze(js.Array(${symbolMap.map(_._1).mkString(", ")}))"""

    val enumDefinition =
      s"""${docsAndAnnotation(enum, typeName, isJsNative = false)}
         |@js.native
         |sealed trait ${name} extends js.Any
         |object ${name} extends js.Object {
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
                                       requiredFields: Set[String]
  ) = {
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
                                          requiredFields: Set[String]
  ) = {
    sortedMembers.fold("")(_.map {
      case (memberName, memberType) =>
        val memberTypeStr = if (requiredFields(memberName)) {
          s"${className(memberType).getOrElse(memberName)}"
        } else {
          s"js.UndefOr[${className(memberType).getOrElse(memberName)}] = js.undefined"
        }
        s"""    ${cleanName(memberName)}: ${memberTypeStr}"""
    }.mkString(",\n"))
  }

  private def genStructureObjectConstruction(sortedMembers: Option[Seq[(String, AwsApiType)]],
                                             requiredFields: Set[String]
  ) = {
    val instanceWithRequiredFields = if (requiredFields.isEmpty) {
      "val __obj = js.Dynamic.literal()"
    } else {
      val requiredFieldsStr = sortedMembers.fold("")(_.filter {
        case (memberName, _) => requiredFields(memberName)
      }.map {
        case (memberName, _) =>
          val memberType = s"${cleanName(memberName)}.asInstanceOf[js.Any]"
          s"""      "${memberName}" -> ${memberType}"""
      }.mkString(",\n"))
      s"""val __obj = js.Dynamic.literal(
        |  ${requiredFieldsStr}
        |)
        |""".stripMargin
    }
    val optionalFields = sortedMembers.fold("")(
      _.filterNot { case (memberName, _) => requiredFields(memberName) }
        .map {
          case (memberName, _) =>
            val clean = cleanName(memberName)
            s"""  ${clean}.foreach(__v => __obj.updateDynamic("${memberName}")(__v.asInstanceOf[js.Any]))"""
        }
        .mkString("\n")
    )

    s"""${instanceWithRequiredFields}
      |${optionalFields}""".stripMargin
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
    val objConstruction = genStructureObjectConstruction(sortedMembers, requiredFields)
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
         |  @inline
         |  def apply(
         |${constructorArgs}
         |  ): ${typeName} = {
         |${objConstruction}
         |    __obj.asInstanceOf[${typeName}]
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
    "package",
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

  implicit val formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format

  def generatePackageFile(): Unit = {
    val projectDir = new File("../aws-sdk-scalajs-facade")
    val allDir     = new File(projectDir, "all")
    projectDir.mkdirs()
    val allPackageRoot = new File(allDir, s"src/main/scala/facade/amazonaws")
    allPackageRoot.mkdirs()
    val awsFile = new File(allPackageRoot, s"package.scala")
    awsFile.createNewFile()
    val awsWriter = new PrintWriter(new FileWriter(awsFile))

    val types = com.leeriggins.awsapis.Apis.versions
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
           |  implicit final class AWSExtensionMethods(private val aws: AWS.type) extends AnyVal {
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
    val projectDir = new File("../aws-sdk-scalajs-facade/core")
    projectDir.mkdirs()
    val packageRootDir = new File(projectDir, s"src/main/scala/facade/amazonaws")
    packageRootDir.mkdirs()
    val awsFile = new File(packageRootDir, s"AWSConfigWithServicesDefault.scala")
    awsFile.createNewFile()
    val awsWriter = new PrintWriter(new FileWriter(awsFile))

    val types = com.leeriggins.awsapis.Apis.versions
      .map {
        case (name, version) =>
          val text       = json(name, version, ApiType.normal)
          val parsedText = parse(text)
          val api        = parsedText.extract[Api]
          api.sdkClassName.toLowerCase
      }
      .sorted
      .map { sdkClassName =>
        val configTypeName = sdkClassName match {
          case "s3" | "s3control" => "S3ParamsWithEndpoint"
          case _                  => "ParamsWithEndpoint"
        }
        s"  var ${sdkClassName}: js.UndefOr[${configTypeName}] = js.undefined"
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

  def generateAllServicesTest(): Unit = {
    val projectDir = new File("../aws-sdk-scalajs-facade/all")
    projectDir.mkdirs()
    val packageRootDir = new File(projectDir, s"src/test/scala/net/exoego")
    packageRootDir.mkdirs()
    val awsFile = new File(packageRootDir, s"AllServicesTest.scala")
    awsFile.createNewFile()
    val awsWriter = new PrintWriter(new FileWriter(awsFile))

    val types = com.leeriggins.awsapis.Apis.versions
      .map {
        case (name, version) =>
          val text       = json(name, version, ApiType.normal)
          val parsedText = parse(text)
          val api        = parsedText.extract[Api]
          api.serviceClassName
      }
      .sorted
      .map { sdkClassName =>
        s"""  test("${sdkClassName}") {
           |    val instance = new services.${sdkClassName.toLowerCase}.${sdkClassName}(config)
           |  }
           |""".stripMargin
      }
      .mkString("\n")

    try {
      awsWriter.append(s"""package net.exoego
                          |
                          |import facade.amazonaws.{AWSConfig, services}
                          |import org.scalatest.funsuite.AnyFunSuite
                          |
                          |class AllServicesTest extends AnyFunSuite {
                          |  val config = AWSConfig(
                          |    endpoint = "http://localhost"
                          |  )
                          |
                          |${types}
                          |}
                          |""".stripMargin.trim)
      ()
    } finally {
      awsWriter.close()
    }
  }

  def checkNewService(): Unit = {
    val apiVersionsMap = com.leeriggins.awsapis.Apis.versions.toMap
    val versionPattern = "^(.+)-(\\d{4}-\\d{2}-\\d{2})".r

    case class NameAndVersion(name: String, version: String)

    val jsonStream = Files.list(Paths.get(s"aws-sdk-js/apis")).collect(Collectors.toList[Path])
    val servicesJsons = jsonStream.asScala
      .flatMap { path =>
        versionPattern.findFirstMatchIn(path.getFileName.toString).map { m =>
          NameAndVersion(name = m.group(1), version = m.group(2))
        }
      }
      .toSet
      .groupMap[String, String](_.name)(_.version)

    val newServiceKeys = (servicesJsons.keySet -- apiVersionsMap.keySet)
    if (newServiceKeys.nonEmpty) {
      newServiceKeys.foreach { key =>
        val latestVersion = servicesJsons(key).max
        println(s""" "${key}" -> "${latestVersion}",  """)
      }
      throw new Exception("Newly-added services found !!")
    }

    val oldVersions = servicesJsons
      .filter {
        case (_, group) =>
          group.sizeIs >= 2
      }
      .flatMap {
        case (key, versions) =>
          val latestVersion = versions.max
          if (latestVersion > apiVersionsMap(key)) {
            Some(key -> latestVersion)
          } else {
            None
          }
      }
    if (oldVersions.nonEmpty) {
      oldVersions.foreach {
        case (key, latestVersion) =>
          println(s""" "${key}" -> "${latestVersion}",  """)
      }
      throw new Exception("Newer version found !!")
    }
  }

  def main(args: Array[String]): Unit = {
    checkNewService()
    generatePackageFile()
    generateAWSConfigWithServicesDefault()
    generateAllServicesTest()
  }
}
