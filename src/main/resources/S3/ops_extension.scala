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
