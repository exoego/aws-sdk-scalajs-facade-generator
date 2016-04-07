# aws-sdk-apis-parser

Parse the json files in the aws-sdk-js project into Scala.

Why? I was curious to see if I could auto-gen some Scala.js wrappers.

## Notes

  * Currently no support for paginator json files.
  * Should split the schema into min and normal versions instead of having them mixed into a single hierarchy.
  * Should reconfigure this as a multi-project build so I don't need a parallel project to test the Scala.js output.
  