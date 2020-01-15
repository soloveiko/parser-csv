package ai.parser

import java.io.File

import ai.parser.utils.Format

object Main extends App {
  implicit val format: Format = new Format{
    // using default
  }

  val reader = CsvReader.read(new File("file.csv"))
  reader.streamLines.foreach(l => println(l))
}
