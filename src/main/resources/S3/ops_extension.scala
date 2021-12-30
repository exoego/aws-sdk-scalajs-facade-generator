/**
* Get a pre-signed URL for a given operation name.
*
* @see https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#getSignedUrl-property
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

/**
 * Delete objects whose key name start with the given prefix.
 *
 * @see https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#listObjectsV2-property
 * @see https://docs.aws.amazon.com/AWSJavaScriptSDK/latest/AWS/S3.html#deleteObjects-property
 * @param bucket The name of the bucket
 * @param prefix An object key name prefix. To include all objects in bucket, specify an empty string.
 * @return Future of unit (no value)
 */
def deleteObjectsByPrefixFuture(bucket: String, prefix: String)(implicit executor: scala.concurrent.ExecutionContext): Future[Unit] = {
  def deleteObjects(objects: ObjectIdentifierList): Future[Unit] = {
    if (objects.nonEmpty) {
      service.deleteObjectsFuture(
        DeleteObjectsRequest(
          Bucket = bucket,
          Delete = Delete(Objects = objects)
        )
      ).map(_ => ())
    } else {
      Future.unit
    }
  }
  def loop(): Future[Unit] = {
    for {
      listObjectsResult <- service.listObjectsV2Future(
        ListObjectsV2Request(
          Bucket = bucket,
          Prefix = prefix,
          MaxKeys = 1000 // 1000 is upper-limit of listObjectsV2 and deleteObjects
        )
      )
      objectsToDelete = listObjectsResult.Contents.get.map(x => ObjectIdentifier(Key = x.Key.get))
      _ <- deleteObjects(objectsToDelete)
      _ <- listObjectsResult.NextContinuationToken.fold(Future.unit) { _ =>
        // The value of NextContinuationToken is ignored here, since it was revoked by object deletion
        loop()
      }
    } yield {}
  }
  loop()
}

/**
 * Uploads an arbitrarily sized buffer, blob, or stream, using intelligent concurrent handling of parts if the payload is large enough.
 * Note that this is the only operation for which the SDK can retry requests with stream bodies.
 * @return The managed upload object that can call send() or track progress.
 */
def upload(params: PutObjectRequest): managedupload.ManagedUpload = service.asInstanceOf[js.Dynamic].upload(params).asInstanceOf[managedupload.ManagedUpload]

/**
 * Uploads an arbitrarily sized buffer, blob, or stream, using intelligent concurrent handling of parts if the payload is large enough.
 * You can configure the concurrent queue size by setting options.
 * Note that this is the only operation for which the SDK can retry requests with stream bodies.
 * @return The managed upload object that can call send() or track progress.
 */
def upload(params: PutObjectRequest, options: managedupload.ManagedUploadOptions): managedupload.ManagedUpload = service.asInstanceOf[js.Dynamic].upload(params, options).asInstanceOf[managedupload.ManagedUpload]

/**
 * Uploads an arbitrarily sized buffer, blob, or stream, using intelligent concurrent handling of parts if the payload is large enough.
 * Note that this is the only operation for which the SDK can retry requests with stream bodies.
 * @return The response data from the successful upload
 */
def uploadFuture(params: PutObjectRequest): Future[managedupload.SendData] = {
  service.upload(params).sendFuture()
}

/**
 * Uploads an arbitrarily sized buffer, blob, or stream, using intelligent concurrent handling of parts if the payload is large enough.
 * You can configure the concurrent queue size by setting options.
 * Note that this is the only operation for which the SDK can retry requests with stream bodies.
 * @return The response data from the successful upload
 */
def uploadFuture(params: PutObjectRequest, options: managedupload.ManagedUploadOptions): Future[managedupload.SendData] = {
  service.upload(params, options).sendFuture()
}
