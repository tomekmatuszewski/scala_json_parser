import scala.io.Source

sealed trait JsonValue
case class JsonObject(fields: Map[String, JsonValue]) extends JsonValue {
    override def toString: String = fields.map { case (k, v) => s"$k: $v" }.mkString("{", ", ", "}")
}
case class JsonArray(items: List[JsonValue]) extends JsonValue {
    override def toString: String = items.mkString("[", ", ", "]")
}
case class JsonString(value: String) extends JsonValue {
    override def toString: String = s"\"$value\""
}
case class JsonNumber(value: Double) extends JsonValue {
    override def toString: String = value.toString
}
case class JsonBoolean(value: Boolean) extends JsonValue {
    override def toString: String = value.toString
}
case object JsonNull extends JsonValue {
    override def toString: String = "null"
}


object JsonParser {
    def parse(path: String): Unit = {
        val json = open_file(path)
        println(parse_json(json))
    }

    def open_file(path: String): String = {
        val source = Source.fromFile(path)
        val lines = try source.mkString finally source.close()
        lines
    }

    def parse_json(json: String): Option[JsonValue] = {
        if ((json.trim.startsWith("[") && json.trim.endsWith("]")) || (json.trim.startsWith("{") && json.trim.endsWith("}")) && !json.isEmpty) {
            try {
                Some(parse_value(json.trim))
            } catch {
                case e: Exception => println(e); None
            }
        } else {
            throw new Exception("Invalid JSON")
        }
        
    }

    def parse_value(json: String): JsonValue = {
        json.headOption match {
            case Some('{') => parse_object(json)
            case Some('[') => parse_array(json)
            case Some('"') => parse_string(json)
            case Some(c) if c.isDigit || c == '-' => parse_number(json)
            case Some('t') | Some('f') => parse_boolean(json)
            case Some('n') => parse_null(json)
            case _ => throw new Exception("Invalid JSON")
        }
    }

    def parse_object(json: String): JsonObject = {
        if (json.isEmpty || !json.startsWith("{") || !json.endsWith("}")) throw new Exception("Invalid JSON")
        if (json.drop(1).dropRight(1).trim().isEmpty()) return JsonObject(Map.empty[String, JsonValue])
        
        val tokens = getTokens(json.drop(1).dropRight(1)).map(_.trim).foldLeft(Map.empty[String, JsonValue]) { (acc, field) => {
            val key = field.takeWhile(_ != ':').trim
            val value = field.dropWhile(_ != ':').drop(1).trim
            if (!key.startsWith("\"") && !key.endsWith("\"")) {
                throw new Exception("Invalid JSON")
            }
            acc + (key -> parse_value(value))
        }
        }
        JsonObject(tokens)
        
    }

    def getTokens(json: String): List[String] = {
        if (json.isEmpty) List.empty[String]
        var token = ""
        var openBrackets = 0
        var inQuotes = false
        val tokens = json.foldLeft(List.empty[String]) { (acc, char) => {
            char match
                case '\"' => {
                    inQuotes = !inQuotes
                    token += char
                    acc
                }
                case '{' | '[' if !inQuotes => {
                    openBrackets += 1
                    token += char
                    acc
                }
                case '}' | ']' if !inQuotes => {
                    openBrackets -= 1
                    token += char
                    acc
                }   
                case ',' if (openBrackets == 0 & !inQuotes) => {
                    val new_acc = acc :+ token
                    token = ""
                    new_acc
                }
                case _ => {
                    token += char 
                    acc
                }
        }
        }
        tokens :+ token
        //legacy
        // var i = 0
        // var token = ""
        // var openBrackets = 0
        // var inQuotes = false
        // var acc = scala.collection.mutable.ListBuffer.empty[String]
        // while (i < json.length) {
        //     json(i) match {
        //         case '\"' => {
        //             inQuotes = !inQuotes
        //             token += json(i)
        //         }
        //         case '{' | '[' if !inQuotes => {
        //             openBrackets += 1
        //             token += json(i)
        //         }
        //         case '}' | ']' if !inQuotes => {
        //             openBrackets -= 1
        //             token += json(i)
        //         }   
        //         case ',' if (openBrackets == 0 & !inQuotes) => {
        //             acc += token
        //             token = ""
        //         }
        //         case _ => token += json(i)
        //     }
        //     i += 1
        // }
        // acc += token
        // acc.toList
    }
    
    def parse_array(json: String): JsonArray = {
        if (!json.endsWith("]") || !json.startsWith("[")) throw new Exception("Invalid JSON array")
        if (json.drop(1).dropRight(1).trim().isEmpty()) return JsonArray(List.empty)
        val items = getTokens(json.drop(1).dropRight(1)).map(_.trim).map(parse_value).toList
        JsonArray(items)
    }

    def parse_string(json: String): JsonString = {
        if (!json.startsWith("\"") || !json.endsWith("\"")) throw new Exception("Invalid JSON")
        val innerContent = json.drop(1).dropRight(1)
        JsonString(innerContent)
    }

    def parse_number(json: String): JsonNumber = {
        if (json.contains(" ") || (json.length() > 1 && json.head == '0' && !json.contains('.'))) throw new Exception("Invalid JSON")
        val number = json.trim.takeWhile(c => c.isDigit || c == '.' || c == 'e' || c == '+' || c == '-')
        JsonNumber(number.toDouble)
    }

    def parse_boolean(json: String): JsonBoolean = {
        if (json.startsWith("true")) JsonBoolean(true)
        else if (json.startsWith("false")) JsonBoolean(false)
        else throw new Exception("Invalid JSON")
    }

    def parse_null(json: String): JsonNull.type = {
        if (json.startsWith("null")) JsonNull
        else throw new Exception("Invalid JSON")
    }


}
