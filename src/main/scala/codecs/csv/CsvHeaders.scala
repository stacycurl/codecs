package codecs.csv

case class CsvHeaders(headerToIndex: Map[CsvHeader, Int], indexToHeader: Map[Int, CsvHeader]) {
  def asString: String = indexToHeader.toList.sortBy(_._1).map(_._2).mkString(",")

  def encode[A: EncodeCsv](lines: List[A]): Csv =
    Csv(this, lines.map(EncodeCsv.of[A].encode))

  def parseLines(data: List[List[String]]): List[CsvRow] = data.map(parseLine)

  def parseLine(line: List[String]): CsvRow = {
    val parsed: List[(CsvHeader, String)] = for {
      (value, index) <- line.zipWithIndex
      header <- indexToHeader.get(index)
    } yield header -> value

    CsvRow(parsed.toMap)
  }

  def toLine(row: CsvRow): List[String] = for {
    (_, header) <- indexToHeader.toList.sortBy(_._1)
    value       <- row.get(header).toOption
  } yield value
}

object CsvHeaders {
  def parse(line: String): CsvHeaders = parse(line.split(",").toList)

  def parse(line: List[CsvHeader]): CsvHeaders = {
    val headerToIndex: Map[CsvHeader, Int] = line.zipWithIndex.toMap
    val indexToHeader: Map[Int, CsvHeader] = headerToIndex.map(_.swap)

    CsvHeaders(headerToIndex, indexToHeader)
  }
}