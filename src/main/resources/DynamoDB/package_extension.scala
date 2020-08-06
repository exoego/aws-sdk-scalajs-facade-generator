
@js.native
@JSImport("aws-sdk", "DynamoDB.Converter", "AWS.DynamoDB.Converter")
object DynamoDBConverter extends js.Object {
  def input(data: js.Any, options: DynamoDBConverterOptions = js.native): AttributeValue = js.native
  def output(data: AttributeValue, options: DynamoDBConverterOptions = js.native): js.Any = js.native

  def marshall(data: js.Object, options: DynamoDBConverterOptions = js.native): AttributeMap = js.native
  def unmarshall(data: AttributeMap, options: DynamoDBConverterOptions = js.native): js.Object = js.native
}

trait DynamoDBConverterOptions extends js.Object {
  var convertEmptyValues: js.UndefOr[Boolean] = js.undefined
  var wrapNumbers: js.UndefOr[Boolean] = js.undefined
}

object DynamoDBConverterOptions {
  @inline def apply(
    convertEmptyValues: js.UndefOr[Boolean] = js.undefined,
    wrapNumbers: js.UndefOr[Boolean] = js.undefined
  ): DynamoDBConverterOptions = {
    val __obj = js.Dynamic.literal()
    convertEmptyValues.foreach(__v => __obj.updateDynamic("convertEmptyValues")(__v.asInstanceOf[js.Any]))
    wrapNumbers.foreach(__v => __obj.updateDynamic("wrapNumbers")(__v.asInstanceOf[js.Any]))
    __obj.asInstanceOf[DynamoDBConverterOptions]
  }
}
