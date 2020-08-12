  def S(value: StringAttributeValue): AttributeValue = js.Dynamic.literal("S" -> value).asInstanceOf[AttributeValue]
  def NFromInt(value: Int): AttributeValue = js.Dynamic.literal("N" -> value.toString).asInstanceOf[AttributeValue]
  def NFromLong(value: Long): AttributeValue = js.Dynamic.literal("N" -> value.toString).asInstanceOf[AttributeValue]
  def NFromDouble(value: Double): AttributeValue = js.Dynamic.literal("N" -> value.toString).asInstanceOf[AttributeValue]
  def NFromBigInt(value: BigInt): AttributeValue = js.Dynamic.literal("N" -> value.toString).asInstanceOf[AttributeValue]
  def NFromBigDecimal(value: BigDecimal): AttributeValue = js.Dynamic.literal("N" -> value.toString).asInstanceOf[AttributeValue]
  def BOOL(value: BooleanAttributeValue): AttributeValue = js.Dynamic.literal("BOOL" -> value).asInstanceOf[AttributeValue]
  def M(value: js.UndefOr[MapAttributeValue]): AttributeValue = if (value.isEmpty) AttributeValue.NULL(true) else js.Dynamic.literal("M" -> value).asInstanceOf[AttributeValue]
  def M(value: Option[MapAttributeValue]): AttributeValue = if (value.isEmpty) AttributeValue.NULL(true) else js.Dynamic.literal("M" -> value.get).asInstanceOf[AttributeValue]
  def MFromMap[T](value: Map[String, T])(implicit valueType: AttributeValueMapper[T]): AttributeValue = AttributeValueMapper.AttributeValueTypeMapT[T].apply(value)
  def L(value: ListAttributeValue): AttributeValue = js.Dynamic.literal("L" -> value).asInstanceOf[AttributeValue]
  def LFromSeq[T](value: Seq[T])(implicit valueType: AttributeValueMapper[T]): AttributeValue = AttributeValueMapper.AttributeValueTypeSeqT[T].apply(value)
  def NULL(value: NullAttributeValue): AttributeValue = js.Dynamic.literal("NULL" -> value).asInstanceOf[AttributeValue]
  def B(value: BinaryAttributeValue): AttributeValue = js.Dynamic.literal("B" -> value.asInstanceOf[js.Any]).asInstanceOf[AttributeValue]
  def NS(value: NumberSetAttributeValue): AttributeValue = js.Dynamic.literal("NS" -> value).asInstanceOf[AttributeValue]
  def BS(value: BinarySetAttributeValue): AttributeValue = js.Dynamic.literal("BS" -> value).asInstanceOf[AttributeValue]
  def SS(value: StringSetAttributeValue): AttributeValue = js.Dynamic.literal("SS" -> value).asInstanceOf[AttributeValue]

  def OptionAsValueOrNull[T](opt: Option[T])(implicit valueType: AttributeValueMapper[T]): AttributeValue = {
    opt match {
      case None => NULL(true)
      case Some(s) => valueType(s)
    }
  }
  def OptionAsList[T](opt: Option[T])(implicit valueType: AttributeValueMapper[T]): AttributeValue = {
    opt match {
      case None => js.Dynamic.literal("L" -> js.Array()).asInstanceOf[AttributeValue]
      case Some(s) => js.Dynamic.literal("L" -> js.Array(valueType(s))).asInstanceOf[AttributeValue]
    }
  }
