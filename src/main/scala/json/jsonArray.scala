package json

// JSON Array Values
private trait JSONArrayValue:
  val value: Any
case class StringJSONArrayValue(value: String) extends JSONArrayValue, JSONStringValuetoString
case class DoubleJSONArrayValue(value: Double) extends JSONArrayValue, JSONValuetoString
case class FloatJSONArrayValue(value: Float) extends JSONArrayValue, JSONValuetoString
case class LongJSONArrayValue(value: Long) extends JSONArrayValue, JSONValuetoString
case class IntJSONArrayValue(value: Int) extends JSONArrayValue, JSONValuetoString
case class ShortJSONArrayValue(value: Short) extends JSONArrayValue, JSONValuetoString
case class BooleanJSONArrayValue(value: Boolean) extends JSONArrayValue, JSONValuetoString
case class NullJSONArrayValue(value: None.type) extends JSONArrayValue, JSONNullValuetoString
case class ObjectJSONArrayValue(value: JSONObject) extends JSONArrayValue, JSONValuetoString
case class ArrayJSONArrayValue(value: JSONArray) extends JSONArrayValue, JSONValuetoString

// JSON Array Helpers
private trait JSONValuetoString:
    val value: Any
    override def toString(): String = s"${this.value.toString()}"

private trait JSONStringValuetoString extends JSONValuetoString:
    override def toString(): String = s"\"${this.value}\""

private trait JSONNullValuetoString extends JSONValuetoString:
    override def toString(): String = s"null"


// JSON Array
case class JSONArray(inputValues: List[Any] = List()):
  private val values: List[JSONArrayValue] = inputValues.map(refineValue)

  private def refineValue(value: Any): JSONArrayValue = 
    value match
        case value: String => StringJSONArrayValue(value)
        case value: Double => DoubleJSONArrayValue(value)
        case value: Float => FloatJSONArrayValue(value)
        case value: Long => LongJSONArrayValue(value)
        case value: Int => IntJSONArrayValue(value)
        case value: Short => ShortJSONArrayValue(value)
        case value: Boolean => BooleanJSONArrayValue(value)
        case value: None.type => NullJSONArrayValue(value)
        case value: JSONObject => ObjectJSONArrayValue(value)
        case value: List[Any] => ArrayJSONArrayValue(JSONArray(value))
        case value: Any => StringJSONArrayValue(value.toString())
    
  override def toString(): String = {
    var output = "["
        values.zipWithIndex.foreach({
            case (value, i) => output += value.toString()
            if (i < values.length -1) output += ", "
        })
        output += "]"
        output
  }
