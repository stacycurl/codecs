package codecs.avro

import org.apache.avro.generic.GenericData
import scalaz.Semigroup


case class AvroRow(record: GenericData.Record) {
  def &&(rhs: AvroRow): AvroRow = ???
}

object AvroRow {
  implicit val semigroup: Semigroup[AvroRow] = Semigroup.instance[AvroRow](_ && _)
}
