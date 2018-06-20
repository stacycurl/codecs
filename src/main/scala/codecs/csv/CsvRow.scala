package codecs.csv

import codecs.{DecodeResult, Error}
import scalaz.Semigroup

case class CsvRow(values: Map[CsvHeader, String]) {
  def &&(other: CsvRow): CsvRow = CsvRow(values ++ other.values)

  def get(header: CsvHeader): DecodeResult[String] = values.get(header) match {
    case None        => DecodeResult.Ko(List(Error(s"No such header: $header, available: ${headers.mkString(", ")}")))
    case Some(value) => DecodeResult.Ok(value)
  }

  def decode[A: DecodeCsv]: DecodeResult[A] = DecodeCsv.of[A].decode(this)

  private lazy val headers: List[CsvHeader] = values.keySet.toList.sorted
}

object CsvRow {
  implicit val semigroup: Semigroup[CsvRow] = Semigroup.instance[CsvRow](_ && _)

  def parse(csv: String): List[CsvRow] = parse(csv.split("\n").toList.map(_.split(",").toList))

  def parse(lines: List[List[String]]): List[CsvRow] = lines match {
    case header :: data => CsvHeaders.parse(header).parseLines(data)
    case _              => sys.error("boom")
  }
}