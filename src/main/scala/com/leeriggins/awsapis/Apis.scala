package com.leeriggins.awsapis

object Apis {
  val versions = IndexedSeq(
    "accessanalyzer"                  -> "2019-11-01",
    "account"                         -> "2021-02-01",
    "acm"                             -> "2015-12-08",
    "acm-pca"                         -> "2017-08-22",
    "alexaforbusiness"                -> "2017-11-09",
    "amp"                             -> "2020-08-01",
    "amplify"                         -> "2017-07-25",
    "amplifybackend"                  -> "2020-08-11",
    "amplifyuibuilder"                -> "2021-08-11",
    "apigateway"                      -> "2015-07-09",
    "apigatewaymanagementapi"         -> "2018-11-29",
    "apigatewayv2"                    -> "2018-11-29",
    "appconfig"                       -> "2019-10-09",
    "appconfigdata"                   -> "2021-11-11",
    "appflow"                         -> "2020-08-23",
    "appintegrations"                 -> "2020-07-29",
    "application-autoscaling"         -> "2016-02-06",
    "application-insights"            -> "2018-11-25",
    "applicationcostprofiler"         -> "2020-09-10",
    "appmesh"                         -> "2019-01-25",
    "apprunner"                       -> "2020-05-15",
    "appstream"                       -> "2016-12-01",
    "appsync"                         -> "2017-07-25",
    "athena"                          -> "2017-05-18",
    "auditmanager"                    -> "2017-07-25",
    "autoscaling"                     -> "2011-01-01",
    "autoscaling-plans"               -> "2018-01-06",
    "AWSMigrationHub"                 -> "2017-05-31",
    "backup"                          -> "2018-11-15",
    "backup-gateway"                  -> "2021-01-01",
    "batch"                           -> "2016-08-10",
    "braket"                          -> "2019-09-01",
    "budgets"                         -> "2016-10-20",
    "ce"                              -> "2017-10-25",
    "chime"                           -> "2018-05-01",
    "chime-sdk-identity"              -> "2021-04-20",
    "chime-sdk-meetings"              -> "2021-07-15",
    "chime-sdk-messaging"             -> "2021-05-15",
    "cloud9"                          -> "2017-09-23",
    "cloudcontrol"                    -> "2021-09-30",
    "clouddirectory"                  -> "2017-01-11",
    "cloudformation"                  -> "2010-05-15",
    "cloudfront"                      -> "2020-05-31",
    "cloudhsm"                        -> "2014-05-30",
    "cloudhsmv2"                      -> "2017-04-28",
    "cloudsearch"                     -> "2013-01-01",
    "cloudsearchdomain"               -> "2013-01-01",
    "cloudtrail"                      -> "2013-11-01",
    "codeartifact"                    -> "2018-09-22",
    "codebuild"                       -> "2016-10-06",
    "codecommit"                      -> "2015-04-13",
    "codedeploy"                      -> "2014-10-06",
    "codeguru-reviewer"               -> "2019-09-19",
    "codeguruprofiler"                -> "2019-07-18",
    "codepipeline"                    -> "2015-07-09",
    "codestar"                        -> "2017-04-19",
    "codestar-connections"            -> "2019-12-01",
    "codestar-notifications"          -> "2019-10-15",
    "cognito-identity"                -> "2014-06-30",
    "cognito-idp"                     -> "2016-04-18",
    "cognito-sync"                    -> "2014-06-30",
    "comprehend"                      -> "2017-11-27",
    "comprehendmedical"               -> "2018-10-30",
    "compute-optimizer"               -> "2019-11-01",
    "config"                          -> "2014-11-12",
    "connect"                         -> "2017-08-08",
    "connect-contact-lens"            -> "2020-08-21",
    "connectparticipant"              -> "2018-09-07",
    "cur"                             -> "2017-01-06",
    "customer-profiles"               -> "2020-08-15",
    "databrew"                        -> "2017-07-25",
    "dataexchange"                    -> "2017-07-25",
    "datapipeline"                    -> "2012-10-29",
    "datasync"                        -> "2018-11-09",
    "dax"                             -> "2017-04-19",
    "detective"                       -> "2018-10-26",
    "devicefarm"                      -> "2015-06-23",
    "devops-guru"                     -> "2020-12-01",
    "directconnect"                   -> "2012-10-25",
    "discovery"                       -> "2015-11-01",
    "dlm"                             -> "2018-01-12",
    "dms"                             -> "2016-01-01",
    "docdb"                           -> "2014-10-31",
    "drs"                             -> "2020-02-26",
    "ds"                              -> "2015-04-16",
    "dynamodb"                        -> "2012-08-10",
    "ebs"                             -> "2019-11-02",
    "ec2"                             -> "2016-11-15",
    "ec2-instance-connect"            -> "2018-04-02",
    "ecr"                             -> "2015-09-21",
    "ecr-public"                      -> "2020-10-30",
    "ecs"                             -> "2014-11-13",
    "eks"                             -> "2017-11-01",
    "elastic-inference"               -> "2017-07-25",
    "elasticache"                     -> "2015-02-02",
    "elasticbeanstalk"                -> "2010-12-01",
    "elasticfilesystem"               -> "2015-02-01",
    "elasticloadbalancing"            -> "2012-06-01",
    "elasticloadbalancingv2"          -> "2015-12-01",
    "elasticmapreduce"                -> "2009-03-31",
    "elastictranscoder"               -> "2012-09-25",
    "email"                           -> "2010-12-01",
    "emr-containers"                  -> "2020-10-01",
    "entitlement.marketplace"         -> "2017-01-11",
    "es"                              -> "2015-01-01",
    "eventbridge"                     -> "2015-10-07",
    "events"                          -> "2015-10-07",
    "evidently"                       -> "2021-02-01",
    "finspace"                        -> "2021-03-12",
    "finspace-data"                   -> "2020-07-13",
    "firehose"                        -> "2015-08-04",
    "fis"                             -> "2020-12-01",
    "fms"                             -> "2018-01-01",
    "forecast"                        -> "2018-06-26",
    "forecastquery"                   -> "2018-06-26",
    "frauddetector"                   -> "2019-11-15",
    "fsx"                             -> "2018-03-01",
    "gamelift"                        -> "2015-10-01",
    "glacier"                         -> "2012-06-01",
    "globalaccelerator"               -> "2018-08-08",
    "glue"                            -> "2017-03-31",
    "grafana"                         -> "2020-08-18",
    "greengrass"                      -> "2017-06-07",
    "greengrassv2"                    -> "2020-11-30",
    "groundstation"                   -> "2019-05-23",
    "guardduty"                       -> "2017-11-28",
    "health"                          -> "2016-08-04",
    "healthlake"                      -> "2017-07-01",
    "honeycode"                       -> "2020-03-01",
    "iam"                             -> "2010-05-08",
    "identitystore"                   -> "2020-06-15",
    "imagebuilder"                    -> "2019-12-02",
    "importexport"                    -> "2010-06-01",
    "inspector"                       -> "2016-02-16",
    "inspector2"                      -> "2020-06-08",
    "iot"                             -> "2015-05-28",
    "iot-data"                        -> "2015-05-28",
    "iot-jobs-data"                   -> "2017-09-29",
    "iot1click-devices"               -> "2018-05-14",
    "iot1click-projects"              -> "2018-05-14",
    "iotanalytics"                    -> "2017-11-27",
    "iotdeviceadvisor"                -> "2020-09-18",
    "iotevents"                       -> "2018-07-27",
    "iotevents-data"                  -> "2018-10-23",
    "iotfleethub"                     -> "2020-11-03",
    "iotsecuretunneling"              -> "2018-10-05",
    "iotsitewise"                     -> "2019-12-02",
    "iotthingsgraph"                  -> "2018-09-06",
    "iottwinmaker"                    -> "2021-11-29",
    "iotwireless"                     -> "2020-11-22",
    "ivs"                             -> "2020-07-14",
    "kafka"                           -> "2018-11-14",
    "kafkaconnect"                    -> "2021-09-14",
    "kendra"                          -> "2019-02-03",
    "kinesis"                         -> "2013-12-02",
    "kinesis-video-archived-media"    -> "2017-09-30",
    "kinesis-video-media"             -> "2017-09-30",
    "kinesis-video-signaling"         -> "2019-12-04",
    "kinesisanalytics"                -> "2015-08-14",
    "kinesisanalyticsv2"              -> "2018-05-23",
    "kinesisvideo"                    -> "2017-09-30",
    "kms"                             -> "2014-11-01",
    "lakeformation"                   -> "2017-03-31",
    "lambda"                          -> "2015-03-31",
    "lex-models"                      -> "2017-04-19",
    "license-manager"                 -> "2018-08-01",
    "lightsail"                       -> "2016-11-28",
    "location"                        -> "2020-11-19",
    "logs"                            -> "2014-03-28",
    "lookoutequipment"                -> "2020-12-15",
    "lookoutmetrics"                  -> "2017-07-25",
    "lookoutvision"                   -> "2020-11-20",
    "machinelearning"                 -> "2014-12-12",
    "macie"                           -> "2017-12-19",
    "macie2"                          -> "2020-01-01",
    "managedblockchain"               -> "2018-09-24",
    "marketplace-catalog"             -> "2018-09-17",
    "marketplacecommerceanalytics"    -> "2015-07-01",
    "mediaconnect"                    -> "2018-11-14",
    "mediaconvert"                    -> "2017-08-29",
    "medialive"                       -> "2017-10-14",
    "mediapackage"                    -> "2017-10-12",
    "mediapackage-vod"                -> "2018-11-07",
    "mediastore"                      -> "2017-09-01",
    "mediastore-data"                 -> "2017-09-01",
    "mediatailor"                     -> "2018-04-23",
    "memorydb"                        -> "2021-01-01",
    "meteringmarketplace"             -> "2016-01-14",
    "mgn"                             -> "2020-02-26",
    "migration-hub-refactor-spaces"   -> "2021-10-26",
    "migrationhub-config"             -> "2019-06-30",
    "migrationhubstrategy"            -> "2020-02-19",
    "mobile"                          -> "2017-07-01",
    "mobileanalytics"                 -> "2014-06-05",
    "models.lex.v2"                   -> "2020-08-07",
    "monitoring"                      -> "2010-08-01",
    "mq"                              -> "2017-11-27",
    "mturk-requester"                 -> "2017-01-17",
    "mwaa"                            -> "2020-07-01",
    "neptune"                         -> "2014-10-31",
    "network-firewall"                -> "2020-11-12",
    "networkmanager"                  -> "2019-07-05",
    "nimble"                          -> "2020-08-01",
    "opensearch"                      -> "2021-01-01",
    "opsworks"                        -> "2013-02-18",
    "opsworkscm"                      -> "2016-11-01",
    "organizations"                   -> "2016-11-28",
    "outposts"                        -> "2019-12-03",
    "panorama"                        -> "2019-07-24",
    "personalize"                     -> "2018-05-22",
    "personalize-events"              -> "2018-03-22",
    "personalize-runtime"             -> "2018-05-22",
    "pi"                              -> "2018-02-27",
    "pinpoint"                        -> "2016-12-01",
    "pinpoint-email"                  -> "2018-07-26",
    "polly"                           -> "2016-06-10",
    "pricing"                         -> "2017-10-15",
    "proton"                          -> "2020-07-20",
    "qldb"                            -> "2019-01-02",
    "qldb-session"                    -> "2019-07-11",
    "quicksight"                      -> "2018-04-01",
    "ram"                             -> "2018-01-04",
    "rbin"                            -> "2021-06-15",
    "rds"                             -> "2014-10-31",
    "rds-data"                        -> "2018-08-01",
    "redshift"                        -> "2012-12-01",
    "redshift-data"                   -> "2019-12-20",
    "rekognition"                     -> "2016-06-27",
    "resiliencehub"                   -> "2020-04-30",
    "resource-groups"                 -> "2017-11-27",
    "resourcegroupstaggingapi"        -> "2017-01-26",
    "robomaker"                       -> "2018-06-29",
    "route53"                         -> "2013-04-01",
    "route53-recovery-cluster"        -> "2019-12-02",
    "route53-recovery-control-config" -> "2020-11-02",
    "route53-recovery-readiness"      -> "2019-12-02",
    "route53domains"                  -> "2014-05-15",
    "route53resolver"                 -> "2018-04-01",
    "rum"                             -> "2018-05-10",
    "runtime.lex"                     -> "2016-11-28",
    "runtime.lex.v2"                  -> "2020-08-07",
    "runtime.sagemaker"               -> "2017-05-13",
    "s3"                              -> "2006-03-01",
    "s3control"                       -> "2018-08-20",
    "s3outposts"                      -> "2017-07-25",
    "sagemaker"                       -> "2017-07-24",
    "sagemaker-a2i-runtime"           -> "2019-11-07",
    "sagemaker-edge"                  -> "2020-09-23",
    "sagemaker-featurestore-runtime"  -> "2020-07-01",
    "savingsplans"                    -> "2019-06-28",
    "schemas"                         -> "2019-12-02",
    "sdb"                             -> "2009-04-15",
    "secretsmanager"                  -> "2017-10-17",
    "securityhub"                     -> "2018-10-26",
    "serverlessrepo"                  -> "2017-09-08",
    "service-quotas"                  -> "2019-06-24",
    "servicecatalog"                  -> "2015-12-10",
    "servicecatalog-appregistry"      -> "2020-06-24",
    "servicediscovery"                -> "2017-03-14",
    "sesv2"                           -> "2019-09-27",
    "shield"                          -> "2016-06-02",
    "signer"                          -> "2017-08-25",
    "sms"                             -> "2016-10-24",
    "sms-voice"                       -> "2018-09-05",
    "snow-device-management"          -> "2021-08-04",
    "snowball"                        -> "2016-06-30",
    "sns"                             -> "2010-03-31",
    "sqs"                             -> "2012-11-05",
    "ssm"                             -> "2014-11-06",
    "ssm-contacts"                    -> "2021-05-03",
    "ssm-incidents"                   -> "2018-05-10",
    "sso"                             -> "2019-06-10",
    "sso-admin"                       -> "2020-07-20",
    "sso-oidc"                        -> "2019-06-10",
    "states"                          -> "2016-11-23",
    "storagegateway"                  -> "2013-06-30",
    "streams.dynamodb"                -> "2012-08-10",
    "sts"                             -> "2011-06-15",
    "support"                         -> "2013-04-15",
    "swf"                             -> "2012-01-25",
    "synthetics"                      -> "2017-10-11",
    "textract"                        -> "2018-06-27",
    "timestream-query"                -> "2018-11-01",
    "timestream-write"                -> "2018-11-01",
    "transcribe"                      -> "2017-10-26",
    "transfer"                        -> "2018-11-05",
    "translate"                       -> "2017-07-01",
    "voice-id"                        -> "2021-09-27",
    "waf"                             -> "2015-08-24",
    "waf-regional"                    -> "2016-11-28",
    "wafv2"                           -> "2019-07-29",
    "wellarchitected"                 -> "2020-03-31",
    "wisdom"                          -> "2020-10-19",
    "workdocs"                        -> "2016-05-01",
    "worklink"                        -> "2018-09-25",
    "workmail"                        -> "2017-10-01",
    "workmailmessageflow"             -> "2019-05-01",
    "workspaces-web"                  -> "2020-07-08",
    "workspaces"                      -> "2015-04-08",
    "xray"                            -> "2016-04-12"
  )
}
