package json

// JSON Object Members
private trait JSONMember:
  val key: String
case class StringJSONMember(key: String, value: String) extends JSONMember, JSONStringMembertoString
case class DoubleJSONMember(key: String, value: Double) extends JSONMember, JSONMembertoString
case class FloatJSONMember(key: String, value: Float) extends JSONMember, JSONMembertoString
case class LongJSONMember(key: String, value: Long) extends JSONMember, JSONMembertoString
case class IntJSONMember(key: String, value: Int) extends JSONMember, JSONMembertoString
case class ShortJSONMember(key: String, value: Short) extends JSONMember, JSONMembertoString
case class BooleanJSONMember(key: String, value: Boolean) extends JSONMember, JSONMembertoString
case class NullJSONMember(key: String, value: None.type) extends JSONMember, JSONNullMembertoString
case class ObjectJSONMember(key: String, value: JSONObject) extends JSONMember, JSONMembertoString
case class ArrayJSONMember(key: String, value: JSONArray) extends JSONMember, JSONMembertoString

// JSON Object Helpers
private trait JSONMembertoString:
    val value: Any
    val key: String
    override def toString(): String = s"\"${this.key}\": ${this.value.toString()}"

private trait JSONStringMembertoString extends JSONMembertoString:
    override def toString(): String = s"\"${this.key}\": \"${this.value}\""

private trait JSONNullMembertoString extends JSONMembertoString:
    override def toString(): String = s"\"${this.key}\": null"


// JSON Object
class JSONObject(inputMembers: List[(String, Any)] = List()):
    private val members: List[JSONMember] = inputMembers.map(this.refineMember)

    private def refineMember(member: (String, Any)): JSONMember =
    member match
        case (key: String, value: String)    => StringJSONMember(key, value)
        case (key: String, value: Double)    => DoubleJSONMember(key, value)
        case (key: String, value: Float)     => FloatJSONMember(key, value)
        case (key: String, value: Long)      => LongJSONMember(key, value)
        case (key: String, value: Int)       => IntJSONMember(key, value)
        case (key: String, value: Short)     => ShortJSONMember(key, value)
        case (key: String, value: Boolean)   => BooleanJSONMember(key, value)
        case (key: String, value: None.type) => NullJSONMember(key, value)
        case (key: String, value: JSONObject) => ObjectJSONMember(key, value)
        case (key: String, value: List[Any]) => ArrayJSONMember(key, JSONArray(value))
        case (key: String, value: Any) => StringJSONMember(key, value.toString())
    
    def addMembers(members: List[(String, Any)]): Unit = {
        
    }

    override def toString(): String = {
        var output = "{"
        members.zipWithIndex.foreach({
            case (member, i) => output += member.toString()
            if (i < members.length -1) output += ", "
        })
        output += "}"
        output
    }



