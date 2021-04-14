package com.leeriggins.awsapis.gen

import com.leeriggins.awsapis.NonServiceClasses

import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

object ChangeDetector {
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
      .filter { case (_, group) =>
        group.sizeIs >= 2
      }
      .flatMap { case (key, versions) =>
        val latestVersion = versions.max
        if (latestVersion > apiVersionsMap(key)) {
          Some(key -> latestVersion)
        } else {
          None
        }
      }
    if (oldVersions.nonEmpty) {
      oldVersions.foreach { case (key, latestVersion) =>
        println(s""" "${key}" -> "${latestVersion}",  """)
      }
      throw new Exception("Newer version found in services !!")
    }
  }

  def checkNonServices(): Unit = {
    val digester = MessageDigest.getInstance("MD5")
    val anyUpdate = NonServiceClasses.digests
      .map { case (pathStr, lastDigest) =>
        val path = Paths.get(s"aws-sdk-js/lib", pathStr)
        if (Files.isDirectory(path)) {
          Files.newDirectoryStream(path, "*.d.ts").forEach { file =>
            digester.update(Files.readAllBytes(file))
          }
        } else {
          digester.update(Files.readAllBytes(path))
        }
        val digest = digester.digest().map(s => "%02x".format(s)).mkString
        digester.reset()
        if (digest != lastDigest) {
          println(s"$path: ${digest} ${lastDigest}")
          true
        } else {
          false
        }
      }
      .exists(_ == true)

    if (anyUpdate) {
      throw new Exception("Newer version found in non-services !!")
    }
  }
}
