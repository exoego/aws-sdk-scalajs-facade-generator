
@js.native
@JSImport("aws-sdk/clients/dynamodb", "Converter", "AWS.DynamoDB.Converter")
object DynamoDBConverter extends js.Object {
  def input(data: js.Any, options: DynamoDBConverterOptions = js.native): AttributeValue = js.native
  def output(data: AttributeValue, options: DynamoDBConverterOptions = js.native): js.Any = js.native

  def marshall(data: js.Object, options: DynamoDBConverterOptions = js.native): AttributeMap = js.native
  def unmarshall(data: AttributeMap, options: DynamoDBConverterOptions = js.native): js.Object = js.native
}

trait DynamoDBSetWrapper[V] extends js.Object {
  def `type`: String
  def wrapperName: String
  def values: js.Array[V]
}

trait DynamoDBNumberSet extends DynamoDBSetWrapper[Double]
trait DynamoDBStringSet extends DynamoDBSetWrapper[String]
trait DynamoDBBinarySet extends DynamoDBSetWrapper[js.Any]

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
