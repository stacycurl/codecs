package codecs

import scalaz.Semigroup

package object csv {
  type CsvHeader = String

  type EncodeCsv[A] = Encode[A, CsvRow]
  object EncodeCsv extends Encode.Companion[CsvRow]

  type DecodeCsv[A] = Decode[A, CsvRow]
  object DecodeCsv extends Decode.Companion[CsvRow]

  type CodecCsv[A] = Codec[A, CsvRow]
  object CodecCsv extends Codec.Parameterless[CsvRow] {
    protected implicit def repSemigroup: Semigroup[CsvRow] = CsvRow.semigroup

    abstract class Enum[A](header: CsvHeader, fromString: String => A, toString: A => String) {
      implicit val codec: CodecCsv[A] = CodecCsv.enum[A](header, fromString, toString)
    }

    def enum[A](header: CsvHeader, asA: String => A, asString: A => String): CodecCsv[A] = of[A](
      (a: A) => CsvRow(Map(header -> asString(a))),
      (row: CsvRow) => row.get(header).map(asA)
    )
  }
}
