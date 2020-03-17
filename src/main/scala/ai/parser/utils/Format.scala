package ai.parser.utils

trait Format extends Serializable {
  val delimiter: Char = ','
  val quoteChar: Char = '"'
  val escapeChar: Char = '"'
  val lineTerminator: String = "\r\n"

}
