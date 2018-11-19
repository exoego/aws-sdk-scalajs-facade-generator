# aws-sdk-scalajs-facade-generator

This is a fork of awesome [balshor/aws-sdk-apis-parser](https://github.com/balshor/aws-sdk-apis-parser).
Intention of fork is to change facade generator logic drastically.

A generated type facade can be found https://github.com/exoego/aws-sdk-scalajs-facade


## Notes

  * Currently no support for paginator json files.
  * Should split the schema into min and normal versions instead of having them mixed into a single hierarchy.
  * Should reconfigure this as a multi-project build so I don't need a parallel project to test the Scala.js output.
  
