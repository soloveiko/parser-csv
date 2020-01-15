package ai.parser

import ai.parser.utils.{Format, MalformedLineException}

import scala.annotation.switch

class Parser(format: Format) extends Serializable {

  def parseLine(input: String): Option[List[String]] = {
    val parsedResult = Parser.parse(input, format.escapeChar, format.delimiter, format.quoteChar)
    if (parsedResult.contains(List(""))) Some(Nil)
    else parsedResult
  }

}

object Parser {
  private type State = Int
  private final val Start = 0
  private final val Field = 1
  private final val Delimiter = 2
  private final val End = 3
  private final val QuoteStart = 4
  private final val QuoteEnd = 5
  private final val QuotedField = 6


  def parse(input: String, escapeChar: Char, delimiter: Char, quoteChar: Char): Option[List[String]] = {
    val buf: Array[Char] = input.toCharArray
    var fields: Vector[String] = Vector()
    var field = new StringBuilder
    var state: State = Start
    var pos = 0
    val buflen = buf.length

    if (buf.length > 0 && buf(0) == '\uFEFF') {
      pos += 1
    }

    while (state != End && pos < buflen) {
      val c = buf(pos)
      (state: @switch) match {
        case Start =>
          c match {
            case `quoteChar` =>
              state = QuoteStart
              pos += 1
            case `delimiter` =>
              fields :+= field.toString
              field = new StringBuilder
              state = Delimiter
              pos += 1
            case '\n' | '\u2028' | '\u2029' | '\u0085' =>
              fields :+= field.toString
              field = new StringBuilder
              state = End
              pos += 1
            case '\r' =>
              if (pos + 1 < buflen && buf(1) == '\n') {
                pos += 1
              }
              fields :+= field.toString
              field = new StringBuilder
              state = End
              pos += 1
            case x =>
              field += x
              state = Field
              pos += 1
          }
        case Delimiter =>
          c match {
            case `quoteChar` =>
              state = QuoteStart
              pos += 1
            case `escapeChar` =>
              if (pos + 1 < buflen
                && (buf(pos + 1) == escapeChar || buf(pos + 1) == delimiter)) {
                field += buf(pos + 1)
                state = Field
                pos += 2
              } else {
                throw new MalformedLineException(buf.mkString)
              }
            case `delimiter` =>
              fields :+= field.toString
              field = new StringBuilder
              state = Delimiter
              pos += 1
            case '\n' | '\u2028' | '\u2029' | '\u0085' =>
              fields :+= field.toString
              field = new StringBuilder
              state = End
              pos += 1
            case '\r' =>
              if (pos + 1 < buflen && buf(1) == '\n') {
                pos += 1
              }
              fields :+= field.toString
              field = new StringBuilder
              state = End
              pos += 1
            case x =>
              field += x
              state = Field
              pos += 1
          }
        case Field =>
          c match {
            case `escapeChar` =>
              if (pos + 1 < buflen) {
                if (buf(pos + 1) == escapeChar
                  || buf(pos + 1) == delimiter) {
                  field += buf(pos + 1)
                  state = Field
                  pos += 2
                } else {
                  throw new MalformedLineException(buf.mkString)
                }
              } else {
                state = QuoteEnd
                pos += 1
              }
            case `delimiter` =>
              fields :+= field.toString
              field = new StringBuilder
              state = Delimiter
              pos += 1
            case '\n' | '\u2028' | '\u2029' | '\u0085' =>
              fields :+= field.toString
              field = new StringBuilder
              state = End
              pos += 1
            case '\r' =>
              if (pos + 1 < buflen && buf(1) == '\n') {
                pos += 1
              }
              fields :+= field.toString
              field = new StringBuilder
              state = End
              pos += 1
            case x =>
              field += x
              state = Field
              pos += 1
          }
        case QuoteStart =>
          c match {
            case `escapeChar` if escapeChar != quoteChar =>
              if (pos + 1 < buflen) {
                if (buf(pos + 1) == escapeChar
                  || buf(pos + 1) == quoteChar) {
                  field += buf(pos + 1)
                  state = QuotedField
                  pos += 2
                } else {
                  throw new MalformedLineException(buf.mkString)
                }
              } else {
                throw new MalformedLineException(buf.mkString)
              }
            case `quoteChar` =>
              if (pos + 1 < buflen && buf(pos + 1) == quoteChar) {
                field += quoteChar
                state = QuotedField
                pos += 2
              } else {
                state = QuoteEnd
                pos += 1
              }
            case x =>
              field += x
              state = QuotedField
              pos += 1
          }
        case QuoteEnd =>
          c match {
            case `delimiter` =>
              fields :+= field.toString
              field = new StringBuilder
              state = Delimiter
              pos += 1
            case '\n' | '\u2028' | '\u2029' | '\u0085' =>
              fields :+= field.toString
              field = new StringBuilder
              state = End
              pos += 1
            case '\r' =>
              if (pos + 1 < buflen && buf(1) == '\n') {
                pos += 1
              }
              fields :+= field.toString
              field = new StringBuilder
              state = End
              pos += 1
            case x =>
              field += x
              state = Field
              pos += 1
          }
        case QuotedField =>
          c match {
            case `escapeChar` if escapeChar != quoteChar =>
              if (pos + 1 < buflen) {
                if (buf(pos + 1) == escapeChar
                  || buf(pos + 1) == quoteChar) {
                  field += buf(pos + 1)
                  state = QuotedField
                  pos += 2
                } else {
                  throw new MalformedLineException(buf.mkString)
                }
              } else {
                throw new MalformedLineException(buf.mkString)
              }
            case `quoteChar` =>
              if (pos + 1 < buflen && buf(pos + 1) == quoteChar) {
                field += quoteChar
                state = QuotedField
                pos += 2
              } else {
                state = QuoteEnd
                pos += 1
              }
            case x =>
              field += x
              state = QuotedField
              pos += 1
          }
        case End =>
          sys.error("unexpected error")
      }
    }
    (state: @switch) match {
      case Delimiter =>
        fields :+= ""
        Some(fields.toList)
      case QuotedField =>
        None
      case _ =>
        state match {
          case Field | QuoteEnd =>
            fields :+= field.toString
          case _ =>
        }
        Some(fields.toList)
    }
  }


}
