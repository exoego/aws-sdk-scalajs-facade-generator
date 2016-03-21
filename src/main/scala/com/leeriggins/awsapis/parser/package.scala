package com.leeriggins.awsapis

import org.json4s.DefaultFormats

package object parser {
  val formats = DefaultFormats + AwsApiTypeParser.Format + InputParser.Format + OutputParser.Format
}
