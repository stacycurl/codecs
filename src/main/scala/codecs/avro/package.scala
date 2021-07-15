package codecs

import scala.language.implicitConversions

import codecs.avro.CodecAvro
import org.apache.avro.Schema
import org.apache.avro.generic.GenericData
import scalaz.{Semigroup, Zip}

import scala.reflect.ClassTag


package object avro {
  type EncodeAvro[A] = Encode[A, AvroRow]
  object EncodeAvro extends Encode.Companion[AvroRow]

  type DecodeAvro[A] = Decode[A, AvroRow]
  object DecodeAvro extends Decode.Companion[AvroRow]

  type CodecAvro[A] = Codec[A, AvroRow]
  object CodecAvro extends Codec.Parameter[AvroRow, Names] {
    implicit protected def repSemigroup: Semigroup[AvroRow] = AvroRow.semigroup

    protected def caseClassCodec[CC: ClassTag, A](qa: Names[A])(toCC: A => CC, fromCC: CC => A): CodecAvro[CC] = {
      val codecAvroA: CodecAvro[A] = ???

      of[CC](
        (cc: CC) => {
          codecAvroA.encode(fromCC(cc))
        },
        (avroRow: AvroRow) => {
          codecAvroA.decode(avroRow).map(toCC)
        }
      )
    }

    implicit protected def paremeterZip: Zip[Names] = new Zip[Names] {
      def zip[A, B](a: => Names[A], b: => Names[B]): Names[(A, B)] =
        Names[(A, B)](a.names ++ b.names)
    }
  }

  implicit def stringToNames[A](name: String): Names[A] = Names(name :: Nil)

  implicit def stringCodec(name: String): CodecAvro[String] = CodecAvro.of[String](
    (value: String) => {
      val record = new GenericData.Record(Schema.create(Schema.Type.STRING))

      record.put(name, value)

      AvroRow(???)
    },
    (avroRow: AvroRow) => ???
  )

  case class Names[A](names: List[String])
}

case class Foo(name: String, age: Int)

object Foo {
  implicit val codecAvro: CodecAvro[Foo] =  CodecAvro(Foo.apply _ , Foo.unapply _)("name", "age")
}