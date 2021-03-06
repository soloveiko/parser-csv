package ai.parser

import ai.parser.utils.{Format, MalformedLineException}

class Parser()(implicit format: Format) extends Serializable {

  def parseLine(input: String): Option[List[String]] = {
    val parsedResult = Parser.parse(input)(format)
    if (parsedResult.contains(List(""))) Some(Nil)
    else parsedResult
  }

}

object Parser {

  /** @param input    line
    * @param position of current char in input
    * @return
    */
  def parse(input: String, position: Int = 0, fields: Vector[String] = Vector(), leftOver: Boolean = false)(implicit format: Format): Option[List[String]] = {
    val buf: Array[Char] = input.toCharArray
    val buflen = buf.length
    val field = new String
    if (position >= input.length && !field.isEmpty) return Some(fields.toList)
    if (leftOver) return None
    if (buf.length > 0 && buf(0) == '\uFEFF') { // \uFEFF, used to get the difference between big- and little-endian UTF-16 encoding.
      return parse(input, position + 1)
    }

    while (position < input.length) {
      val c = buf(position)
      c match {
        case format.`quoteChar` =>
          return handleQuoteStart(input, position + 1, field, fields)

        case format.`delimiter` =>
          return handleDelimiterChar(input, position + 1, "", fields :+ field)

        case '\n' | '\u2028' | '\u2029' | '\u0085' => // line and paragraph characters.
          return parse(input, position + 1, fields :+ field)

        case '\r' =>
          if (position + 1 < buflen && buf(1) == '\n') {
            return parse(input, position + 1, fields :+ field)
          } else {
            return parse(input, position + 2, fields :+ field)
          }

        case x =>
          val currField = field + x
          return handleField(input, position + 1, currField, fields)

      }
    }
    Some(fields.toList)
  }

  private def handleQuoteStart(input: String, position: Int, currField: String, fields: Vector[String])
                              (implicit format: Format): Option[List[String]] = {
    val buf: Array[Char] = input.toCharArray
    val buflen = buf.length
    while (position < buflen) {
      val c = buf(position)
      c match {
        case format.`escapeChar` if format.escapeChar != format.quoteChar =>
          if (position + 1 < buflen) {
            if (buf(position + 1) == format.escapeChar
              || buf(position + 1) == format.quoteChar) {
              return handleQuotedField(input, currField + buf(position + 1), position + 1, fields)
            } else {
              throw new MalformedLineException(buf.mkString)
            }
          } else {
            throw new MalformedLineException(buf.mkString)
          }
        case format.`quoteChar` =>
          if (position + 1 < buflen && buf(position + 1) == format.quoteChar) {
            return handleQuotedField(input, currField + format.quoteChar, position + 1, fields)
          } else {
            return handleQuotedField(input, currField, position + 1, fields)
          }
        case x =>
          return handleQuotedField(input, currField + x, position + 1, fields)
      }
    }
    parse(input, position, fields)
  }

  private def handleQuotedField(input: String, currField: String, position: Int, fields: Vector[String])
                               (implicit format: Format): Option[List[String]] = {
    val buf: Array[Char] = input.toCharArray
    val buflen = buf.length
    if (position == buflen) return parse(input, position, fields, leftOver = true)
    while (position < buflen) {
      val c = buf(position)
      c match {
        case format.`escapeChar` if format.escapeChar != format.quoteChar =>
          if (position + 1 < buflen) {
            if (buf(position + 1) == format.escapeChar
              || buf(position + 1) == format.quoteChar) {
              return handleQuotedField(input, currField + buf(position + 2), position + 1, fields)
            } else {
              throw new MalformedLineException(buf.mkString)
            }
          } else {
            throw new MalformedLineException(buf.mkString)
          }
        case format.`quoteChar` =>
          if (position + 1 < buflen && buf(position + 1) == format.quoteChar) {
            return handleQuotedField(input, currField + format.quoteChar, position + 2, fields)
          } else {
            return handleQuoteEnd(input, currField, position + 1, fields)
          }
        case x =>
          return handleQuotedField(input, currField + x, position + 1, fields)
      }
    }
    val strings = fields :+ currField
    Some(strings.toList)
  }

  private def handleQuoteEnd(input: String, currField: String, position: Int, fields: Vector[String])
                            (implicit format: Format): Option[List[String]] = {
    val buf: Array[Char] = input.toCharArray
    val buflen = buf.length
    while (position < buflen) {
      val c = buf(position)
      c match {
        case format.`delimiter` =>
          return handleDelimiterChar(input, position + 1, "", fields :+ currField)

        case '\n' | '\u2028' | '\u2029' | '\u0085' => // newline and paragraph characters.
          return parse(input, position + 1, fields :+ currField)

        case '\r' =>
          if (position + 1 < buflen && buf(1) == '\n') {
            return parse(input, position + 1, fields :+ currField)
          } else {
            return parse(input, position + 2, fields :+ currField)
          }

        case x =>
          return handleField(input, position + 1, currField + x, fields)
      }
    }
    val strings = fields :+ currField
    Some(strings.toList)
  }

  private def handleDelimiterChar(input: String, position: Int, currField: String, fields: Vector[String])
                                 (implicit format: Format): Option[List[String]] = {
    val buf: Array[Char] = input.toCharArray
    val buflen = buf.length
    while (position < buflen) {
      val c = buf(position)
      c match {
        case format.`quoteChar` =>
          return handleQuoteStart(input, position + 1, currField, fields)
        case format.`escapeChar` =>
          if (position + 1 < buflen
            && (buf(position + 1) == format.escapeChar || buf(position + 1) == format.delimiter)) {
            return handleField(input, position + 2, currField + buf(position + 1), fields)
          } else {
            throw new MalformedLineException(buf.mkString)
          }
        case format.`delimiter` =>
          return handleDelimiterChar(input, position + 1, "", fields :+ currField)
        case '\r' =>
          if (position + 2 < buflen && buf(1) == '\n') {
            return parse(input, position + 1, fields :+ currField)
          } else {
            return parse(input, position + 2, fields :+ currField)
          }


        case x =>
          return handleField(input, position + 1, currField + x, fields)
      }
    }
    val strings = fields :+ ""
    Some(strings.toList)
  }

  private def handleField(input: String, position: Int, currField: String, fields: Vector[String])
                         (implicit format: Format): Option[List[String]] = {
    val buf: Array[Char] = input.toCharArray
    val buflen = buf.length

    while (position < buflen) {
      val c = buf(position)
      c match {
        case format.`escapeChar` =>
          if (position + 1 < buflen) {
            if (buf(position + 1) == format.escapeChar
              || buf(position + 1) == format.delimiter) {
              return handleField(input, position + 2, currField + buf(position + 1), fields)
            } else {
              throw new MalformedLineException(buf.mkString)
            }
          } else {
            return handleQuoteEnd(input, currField, position + 1, fields)
          }
        case format.`delimiter` =>
          return handleDelimiterChar(input, position + 1, "", fields :+ currField)

        case '\n' | '\u2028' | '\u2029' | '\u0085' => // newline and paragraph characters.
          return parse(input, position + 1, fields :+ currField)

        case '\r' =>
          if (position + 1 < buflen && buf(1) == '\n') {
            return parse(input, position + 1, fields :+ currField)
          } else {
            return parse(input, position + 2, fields :+ currField)
          }

        case x =>
          return handleField(input, position + 1, currField + x, fields)
      }
    }
    val strings = fields :+ currField
    Some(strings.toList)
  }
}
