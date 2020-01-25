package ai.parser

import java.io.File

import ai.parser.utils.Format
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CsvReaderTest extends AnyFunSpec with Matchers {

  implicit val format: Format = new Format {}

  describe("CsvReader") {
    it("multiline value") {
      val reader = CsvReader.read(new File(getClass.getResource("/multiline.csv").getPath))
      val maybeStrings = reader.readNext()
      maybeStrings.get.size should be(7)
    }

    it("quoted field") {
      val reader = CsvReader.read(new File(getClass.getResource("/quoted_field.csv").getPath))
      val maybeStrings = reader.readNext()
      maybeStrings.get.size should be(3)
    }

    it("empty field") {
      val reader = CsvReader.read(new File(getClass.getResource("/empty_field.csv").getPath))
      val maybeStrings = reader.readNext()
      maybeStrings.get.size should be(4)
    }

    it("second empty field") {
      val reader = CsvReader.read(new File(getClass.getResource("/empty_field_second.csv").getPath))
      val maybeStrings = reader.readNext()
      maybeStrings.get.size should be(4)
    }

  }

}
