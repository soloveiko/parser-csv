package ai.parser

import java.io._

import ai.parser.utils.{Format, MalformedLineException}

import util.control.Breaks._


class CsvReader(private val reader: Reader)(implicit format: Format) extends Closeable {

  private val parser = new Parser(format)

  def readNext(): Option[List[String]] = {

    @scala.annotation.tailrec
    def parseNext(readerNext: Reader, leftOver: Option[String] = None): Option[List[String]] = {

      val nextLine = readLineWithTerminator(readerNext)
      if (nextLine == null) {
        if (leftOver.isDefined) {
          throw new MalformedLineException("Malformed Line!: " + leftOver)
        } else {
          None
        }
      } else {
        val line = leftOver.getOrElse("") + nextLine
        parser.parseLine(line) match {
          case None =>
            parseNext(readerNext, Some(line))
          case result => result
        }
      }
    }

    parseNext(reader)
  }

  def streamLines: Stream[List[String]] = Stream.continually(readNext()).takeWhile(_.isDefined).map(_.get)

  def withHeaders: Stream[Map[String, String]] = {
    val mHeaders = readNext()
    mHeaders.map(headers => {
      streamLines.map {
        line => headers.zip(line).toMap
      }
    }).getOrElse(Iterator()).toStream
  }

  private def readLineWithTerminator(bufferedReader: Reader): String = {
    val sb = new StringBuilder
    breakable {
      do {
        var c = bufferedReader.read
        if (c == -1) {
          if (sb.isEmpty) return null
          else break
        }
        sb.append(c.toChar)
        if (c == '\n' || c == '\u2028' || c == '\u2029' || c == '\u0085') break
        if (c == '\r') {
          bufferedReader.mark(1)
          c = bufferedReader.read
          if (c == -1) break
          else if (c == '\n') {
            sb.append('\n')
          }
          else {
            bufferedReader.reset()
          }
          break
        }
      }
      while (true)
    }
    sb.toString
  }

  override def close(): Unit = reader.close()
}

object CsvReader {

  def read(file: File)(implicit format: Format): CsvReader = {
    val reader = new InputStreamReader(new FileInputStream(file))
    read(file, reader.getEncoding)(format)
  }

  def read(file: File, encoding: String)(implicit format: Format): CsvReader = {
    val inputStream = new FileInputStream(file)
    try {
      read(new InputStreamReader(inputStream, encoding))
    } catch {
      case e: UnsupportedEncodingException => inputStream.close(); throw e
    }
  }

  def read(reader: Reader)(implicit format: Format): CsvReader = new CsvReader(reader)(format)

}
