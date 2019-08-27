package com.leeriggins.awsapis

object Apis {
  val versions = IndexedSeq(
    "acm"                          -> "2015-12-08",
    "acm-pca"                      -> "2017-08-22",
    "alexaforbusiness"             -> "2017-11-09",
    "amplify"                      -> "2017-07-25",
    "apigateway"                   -> "2015-07-09",
    "apigatewaymanagementapi"      -> "2018-11-29",
    "apigatewayv2"                 -> "2018-11-29",
    "application-autoscaling"      -> "2016-02-06",
    "appmesh"                      -> "2019-01-25",
    "appstream"                    -> "2016-12-01",
    "appsync"                      -> "2017-07-25",
    "athena"                       -> "2017-05-18",
    "autoscaling"                  -> "2011-01-01",
    "autoscaling-plans"            -> "2018-01-06",
    "AWSMigrationHub"              -> "2017-05-31",
    "backup"                       -> "2018-11-15",
    "batch"                        -> "2016-08-10",
    "budgets"                      -> "2016-10-20",
    "ce"                           -> "2017-10-25",
    "chime"                        -> "2018-05-01",
    "cloud9"                       -> "2017-09-23",
    "clouddirectory"               -> "2017-01-11",
    "cloudformation"               -> "2010-05-15",
    "cloudfront"                   -> "2019-03-26",
    "cloudhsm"                     -> "2014-05-30",
    "cloudhsmv2"                   -> "2017-04-28",
    "cloudsearch"                  -> "2013-01-01",
    "cloudsearchdomain"            -> "2013-01-01",
    "cloudtrail"                   -> "2013-11-01",
    "codebuild"                    -> "2016-10-06",
    "codecommit"                   -> "2015-04-13",
    "codedeploy"                   -> "2014-10-06",
    "codepipeline"                 -> "2015-07-09",
    "codestar"                     -> "2017-04-19",
    "cognito-identity"             -> "2014-06-30",
    "cognito-idp"                  -> "2016-04-18",
    "cognito-sync"                 -> "2014-06-30",
    "comprehend"                   -> "2017-11-27",
    "comprehendmedical"            -> "2018-10-30",
    "config"                       -> "2014-11-12",
    "connect"                      -> "2017-08-08",
    "cur"                          -> "2017-01-06",
    "datapipeline"                 -> "2012-10-29",
    "datasync"                     -> "2018-11-09",
    "dax"                          -> "2017-04-19",
    "devicefarm"                   -> "2015-06-23",
    "directconnect"                -> "2012-10-25",
    "discovery"                    -> "2015-11-01",
    "dlm"                          -> "2018-01-12",
    "dms"                          -> "2016-01-01",
    "docdb"                        -> "2014-10-31",
    "ds"                           -> "2015-04-16",
    "dynamodb"                     -> "2012-08-10",
    "ec2"                          -> "2016-11-15",
    "ec2-instance-connect"         -> "2018-04-02",
    "ecr"                          -> "2015-09-21",
    "ecs"                          -> "2014-11-13",
    "eks"                          -> "2017-11-01",
    "elasticache"                  -> "2015-02-02",
    "elasticbeanstalk"             -> "2010-12-01",
    "elasticfilesystem"            -> "2015-02-01",
    "elasticloadbalancing"         -> "2012-06-01",
    "elasticloadbalancingv2"       -> "2015-12-01",
    "elasticmapreduce"             -> "2009-03-31",
    "elastictranscoder"            -> "2012-09-25",
    "email"                        -> "2010-12-01",
    "entitlement.marketplace"      -> "2017-01-11",
    "es"                           -> "2015-01-01",
    "eventbridge"                  -> "2015-10-07",
    "events"                       -> "2015-10-07",
    "firehose"                     -> "2015-08-04",
    "fms"                          -> "2018-01-01",
    "forecast"                     -> "2018-06-26",
    "forecastquery"                -> "2018-06-26",
    "fsx"                          -> "2018-03-01",
    "gamelift"                     -> "2015-10-01",
    "glacier"                      -> "2012-06-01",
    "globalaccelerator"            -> "2018-08-08",
    "glue"                         -> "2017-03-31",
    "greengrass"                   -> "2017-06-07",
    "groundstation"                -> "2019-05-23",
    "guardduty"                    -> "2017-11-28",
    "health"                       -> "2016-08-04",
    "iam"                          -> "2010-05-08",
    "importexport"                 -> "2010-06-01",
    "inspector"                    -> "2016-02-16",
    "iot"                          -> "2015-05-28",
    "iot-data"                     -> "2015-05-28",
    "iot-jobs-data"                -> "2017-09-29",
    "iot1click-devices"            -> "2018-05-14",
    "iot1click-projects"           -> "2018-05-14",
    "iotanalytics"                 -> "2017-11-27",
    "iotevents"                    -> "2018-07-27",
    "iotthingsgraph"               -> "2018-09-06",
    "kafka"                        -> "2018-11-14",
    "kinesis"                      -> "2013-12-02",
    "kinesis-video-archived-media" -> "2017-09-30",
    "kinesis-video-media"          -> "2017-09-30",
    "kinesisanalytics"             -> "2015-08-14",
    "kinesisanalyticsv2"           -> "2018-05-23",
    "kinesisvideo"                 -> "2017-09-30",
    "kms"                          -> "2014-11-01",
    "lakeformation"                -> "2017-03-31",
    "lambda"                       -> "2015-03-31",
    "lex-models"                   -> "2017-04-19",
    "license-manager"              -> "2018-08-01",
    "lightsail"                    -> "2016-11-28",
    "logs"                         -> "2014-03-28",
    "machinelearning"              -> "2014-12-12",
    "macie"                        -> "2017-12-19",
    "managedblockchain"            -> "2018-09-24",
    "marketplacecommerceanalytics" -> "2015-07-01",
    "mediaconnect"                 -> "2018-11-14",
    "mediaconvert"                 -> "2017-08-29",
    "medialive"                    -> "2017-10-14",
    "mediapackage"                 -> "2017-10-12",
    "mediapackage-vod"             -> "2018-11-07",
    "mediastore"                   -> "2017-09-01",
    "mediastore-data"              -> "2017-09-01",
    "mediatailor"                  -> "2018-04-23",
    "meteringmarketplace"          -> "2016-01-14",
    "mobile"                       -> "2017-07-01",
    "mobileanalytics"              -> "2014-06-05",
    "monitoring"                   -> "2010-08-01",
    "mq"                           -> "2017-11-27",
    "mturk-requester"              -> "2017-01-17",
    "neptune"                      -> "2014-10-31",
    "opsworks"                     -> "2013-02-18",
    "opsworkscm"                   -> "2016-11-01",
    "organizations"                -> "2016-11-28",
    "personalize"                  -> "2018-05-22",
    "personalize-events"           -> "2018-03-22",
    "personalize-runtime"          -> "2018-05-22",
    "pi"                           -> "2018-02-27",
    "pinpoint"                     -> "2016-12-01",
    "pinpoint-email"               -> "2018-07-26",
    "polly"                        -> "2016-06-10",
    "pricing"                      -> "2017-10-15",
    "quicksight"                   -> "2018-04-01",
    "ram"                          -> "2018-01-04",
    "rds"                          -> "2014-10-31",
    "rds-data"                     -> "2018-08-01",
    "redshift"                     -> "2012-12-01",
    "rekognition"                  -> "2016-06-27",
    "resource-groups"              -> "2017-11-27",
    "resourcegroupstaggingapi"     -> "2017-01-26",
    "robomaker"                    -> "2018-06-29",
    "route53"                      -> "2013-04-01",
    "route53domains"               -> "2014-05-15",
    "route53resolver"              -> "2018-04-01",
    "runtime.lex"                  -> "2016-11-28",
    "runtime.sagemaker"            -> "2017-05-13",
    "s3"                           -> "2006-03-01",
    "s3control"                    -> "2018-08-20",
    "sagemaker"                    -> "2017-07-24",
    "sdb"                          -> "2009-04-15",
    "secretsmanager"               -> "2017-10-17",
    "securityhub"                  -> "2018-10-26",
    "serverlessrepo"               -> "2017-09-08",
    "service-quotas"               -> "2019-06-24",
    "servicecatalog"               -> "2015-12-10",
    "servicediscovery"             -> "2017-03-14",
    "shield"                       -> "2016-06-02",
    "signer"                       -> "2017-08-25",
    "sms"                          -> "2016-10-24",
    "sms-voice"                    -> "2018-09-05",
    "snowball"                     -> "2016-06-30",
    "sns"                          -> "2010-03-31",
    "sqs"                          -> "2012-11-05",
    "ssm"                          -> "2014-11-06",
    "states"                       -> "2016-11-23",
    "storagegateway"               -> "2013-06-30",
    "streams.dynamodb"             -> "2012-08-10",
    "sts"                          -> "2011-06-15",
    "support"                      -> "2013-04-15",
    "swf"                          -> "2012-01-25",
    "textract"                     -> "2018-06-27",
    "transcribe"                   -> "2017-10-26",
    "transfer"                     -> "2018-11-05",
    "translate"                    -> "2017-07-01",
    "waf"                          -> "2015-08-24",
    "waf-regional"                 -> "2016-11-28",
    "workdocs"                     -> "2016-05-01",
    "worklink"                     -> "2018-09-25",
    "workmail"                     -> "2017-10-01",
    "workspaces"                   -> "2015-04-08",
    "xray"                         -> "2016-04-12"
  )
}
