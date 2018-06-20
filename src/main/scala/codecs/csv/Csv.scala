package codecs.csv


case class Csv(headers: CsvHeaders, rows: List[CsvRow]) {
  def asString: String =
    (headers.asString :: rows.map(row => headers.toLine(row).mkString(","))).mkString("\n")
}
