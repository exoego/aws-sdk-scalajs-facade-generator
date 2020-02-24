@js.native
sealed trait Operation extends js.Any
object Operation extends js.Object {
  val abortMultipartUpload = "abortMultipartUpload".asInstanceOf[Operation]
  val completeMultipartUpload = "completeMultipartUpload".asInstanceOf[Operation]
  val copyObject = "copyObject".asInstanceOf[Operation]
  val createBucket = "createBucket".asInstanceOf[Operation]
  val createMultipartUpload = "createMultipartUpload".asInstanceOf[Operation]
  val deleteBucketAnalyticsConfiguration = "deleteBucketAnalyticsConfiguration".asInstanceOf[Operation]
  val deleteBucketCors = "deleteBucketCors".asInstanceOf[Operation]
  val deleteBucketEncryption = "deleteBucketEncryption".asInstanceOf[Operation]
  val deleteBucket = "deleteBucket".asInstanceOf[Operation]
  val deleteBucketInventoryConfiguration = "deleteBucketInventoryConfiguration".asInstanceOf[Operation]
  val deleteBucketLifecycle = "deleteBucketLifecycle".asInstanceOf[Operation]
  val deleteBucketMetricsConfiguration = "deleteBucketMetricsConfiguration".asInstanceOf[Operation]
  val deleteBucketPolicy = "deleteBucketPolicy".asInstanceOf[Operation]
  val deleteBucketReplication = "deleteBucketReplication".asInstanceOf[Operation]
  val deleteBucketTagging = "deleteBucketTagging".asInstanceOf[Operation]
  val deleteBucketWebsite = "deleteBucketWebsite".asInstanceOf[Operation]
  val deleteObject = "deleteObject".asInstanceOf[Operation]
  val deleteObjectTagging = "deleteObjectTagging".asInstanceOf[Operation]
  val deleteObjects = "deleteObjects".asInstanceOf[Operation]
  val deletePublicAccessBlock = "deletePublicAccessBlock".asInstanceOf[Operation]
  val getBucketAccelerateConfiguration = "getBucketAccelerateConfiguration".asInstanceOf[Operation]
  val getBucketAcl = "getBucketAcl".asInstanceOf[Operation]
  val getBucketAnalyticsConfiguration = "getBucketAnalyticsConfiguration".asInstanceOf[Operation]
  val getBucketCors = "getBucketCors".asInstanceOf[Operation]
  val getBucketEncryption = "getBucketEncryption".asInstanceOf[Operation]
  val getBucketInventoryConfiguration = "getBucketInventoryConfiguration".asInstanceOf[Operation]
  val getBucketLifecycleConfiguration = "getBucketLifecycleConfiguration".asInstanceOf[Operation]
  val getBucketLocation = "getBucketLocation".asInstanceOf[Operation]
  val getBucketLogging = "getBucketLogging".asInstanceOf[Operation]
  val getBucketMetricsConfiguration = "getBucketMetricsConfiguration".asInstanceOf[Operation]
  val getBucketNotificationConfiguration = "getBucketNotificationConfiguration".asInstanceOf[Operation]
  val getBucketPolicy = "getBucketPolicy".asInstanceOf[Operation]
  val getBucketPolicyStatus = "getBucketPolicyStatus".asInstanceOf[Operation]
  val getBucketReplication = "getBucketReplication".asInstanceOf[Operation]
  val getBucketRequestPayment = "getBucketRequestPayment".asInstanceOf[Operation]
  val getBucketTagging = "getBucketTagging".asInstanceOf[Operation]
  val getBucketVersioning = "getBucketVersioning".asInstanceOf[Operation]
  val getBucketWebsite = "getBucketWebsite".asInstanceOf[Operation]
  val getObjectAcl = "getObjectAcl".asInstanceOf[Operation]
  val getObject = "getObject".asInstanceOf[Operation]
  val getObjectLegalHold = "getObjectLegalHold".asInstanceOf[Operation]
  val getObjectLockConfiguration = "getObjectLockConfiguration".asInstanceOf[Operation]
  val getObjectRetention = "getObjectRetention".asInstanceOf[Operation]
  val getObjectTagging = "getObjectTagging".asInstanceOf[Operation]
  val getObjectTorrent = "getObjectTorrent".asInstanceOf[Operation]
  val getPublicAccessBlock = "getPublicAccessBlock".asInstanceOf[Operation]
  val headBucket = "headBucket".asInstanceOf[Operation]
  val headObject = "headObject".asInstanceOf[Operation]
  val listBucketAnalyticsConfigurations = "listBucketAnalyticsConfigurations".asInstanceOf[Operation]
  val listBucketInventoryConfigurations = "listBucketInventoryConfigurations".asInstanceOf[Operation]
  val listBucketMetricsConfigurations = "listBucketMetricsConfigurations".asInstanceOf[Operation]
  val listBuckets = "listBuckets".asInstanceOf[Operation]
  val listMultipartUploads = "listMultipartUploads".asInstanceOf[Operation]
  val listObjectVersions = "listObjectVersions".asInstanceOf[Operation]
  val listObjects = "listObjects".asInstanceOf[Operation]
  val listObjectsV2 = "listObjectsV2".asInstanceOf[Operation]
  val listParts = "listParts".asInstanceOf[Operation]
  val putBucketAccelerateConfiguration = "putBucketAccelerateConfiguration".asInstanceOf[Operation]
  val putBucketAcl = "putBucketAcl".asInstanceOf[Operation]
  val putBucketAnalyticsConfiguration = "putBucketAnalyticsConfiguration".asInstanceOf[Operation]
  val putBucketCors = "putBucketCors".asInstanceOf[Operation]
  val putBucketEncryption = "putBucketEncryption".asInstanceOf[Operation]
  val putBucketInventoryConfiguration = "putBucketInventoryConfiguration".asInstanceOf[Operation]
  val putBucketLifecycleConfiguration = "putBucketLifecycleConfiguration".asInstanceOf[Operation]
  val putBucketLogging = "putBucketLogging".asInstanceOf[Operation]
  val putBucketMetricsConfiguration = "putBucketMetricsConfiguration".asInstanceOf[Operation]
  val putBucketNotificationConfiguration = "putBucketNotificationConfiguration".asInstanceOf[Operation]
  val putBucketPolicy = "putBucketPolicy".asInstanceOf[Operation]
  val putBucketReplication = "putBucketReplication".asInstanceOf[Operation]
  val putBucketRequestPayment = "putBucketRequestPayment".asInstanceOf[Operation]
  val putBucketTagging = "putBucketTagging".asInstanceOf[Operation]
  val putBucketVersioning = "putBucketVersioning".asInstanceOf[Operation]
  val putBucketWebsite = "putBucketWebsite".asInstanceOf[Operation]
  val putObjectAcl = "putObjectAcl".asInstanceOf[Operation]
  val putObject = "putObject".asInstanceOf[Operation]
  val putObjectLegalHold = "putObjectLegalHold".asInstanceOf[Operation]
  val putObjectLockConfiguration = "putObjectLockConfiguration".asInstanceOf[Operation]
  val putObjectRetention = "putObjectRetention".asInstanceOf[Operation]
  val putObjectTagging = "putObjectTagging".asInstanceOf[Operation]
  val putPublicAccessBlock = "putPublicAccessBlock".asInstanceOf[Operation]
  val restoreObject = "restoreObject".asInstanceOf[Operation]
  val selectObjectContent = "selectObjectContent".asInstanceOf[Operation]
  val uploadPartCopy = "uploadPartCopy".asInstanceOf[Operation]
  val uploadPart = "uploadPart".asInstanceOf[Operation]
}

implicit final class S3OpsExtension(private val service: S3) extends AnyVal {
  /**
   * Get a pre-signed URL for a given operation name.
   *
   * @see http://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#getSignedUrl-property
   * @param operation the name of the operation to call. E.g. `getObject`
   * @param params    Parameters to pass to the operation. See the given operation for the expected operation parameters. In addition, you can also pass the "Expires" parameter to inform S3 how long the URL should work for.
   * @param expires   The number of seconds to expire the pre-signed URL operation in. Defaults to 900 seconds (15 minutes).
   * @return Future of the signed URL
   */
  def getSignedUrlFuture(operation: Operation, params: js.Object, expires: Int = 900): Future[String] = {
    val paramsWithExpires = if (params.hasOwnProperty("Expires") || expires == 900) {
      params
    } else {
      val deepCloned = js.JSON.parse(js.JSON.stringify(params))
      deepCloned.Expires = expires
      deepCloned.asInstanceOf[js.Object]
    }
    service.asInstanceOf[js.Dynamic]
      .getSignedUrlPromise(operation, paramsWithExpires)
      .asInstanceOf[js.Promise[String]]
      .toFuture
  }
}
