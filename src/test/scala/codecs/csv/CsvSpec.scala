package codecs.csv

import codecs.DecodeResult
import org.scalatest.FreeSpec

class CsvSpec extends FreeSpec {
  import Colour._
  import ShapeType._

  "decode" in {
    val rows: List[CsvRow] = CsvRow.parse(
      """Colour,ShapeType,Area
        |red,triangle,100
        |green,square,20""".stripMargin)

    assert(rows.map(_.decode[Diagram]) === List(
      DecodeResult.Ok(Diagram(Red,   Shape(Triangle, Area("100")))),
      DecodeResult.Ok(Diagram(Green, Shape(Square, Area("20"))))
    ))
  }

  "encode" in {
    assert(CsvHeaders.parse("ShapeType,Colour,Area").encode(List(
      Diagram(Red,   Shape(Triangle, Area("100"))),
      Diagram(Green, Shape(Square, Area("20")))
    )).asString ===
      """|ShapeType,Colour,Area
        |triangle,red,100
        |square,green,20""".stripMargin)
  }
}

case class Diagram(colour: Colour, shape: Shape)

object Diagram {
  implicit val codec: CodecCsv[Diagram] = CodecCsv(Diagram.apply _, Diagram.unapply _)
}

case class Colour(value: String)

object Colour extends CodecCsv.Enum[Colour]("Colour", new Colour(_), _.value) {
  val Red: Colour = Colour("red")
  val Green: Colour = Colour("green")
}

case class Shape(shapeType: ShapeType, area: Area)

object Shape {
  implicit val codec: CodecCsv[Shape] = CodecCsv(Shape.apply _, Shape.unapply _)
}

case class ShapeType(value: String)

object ShapeType extends CodecCsv.Enum[ShapeType]("ShapeType", new ShapeType(_), _.value) {
  val Triangle: ShapeType = ShapeType("triangle")
  val Square: ShapeType = ShapeType("square")
}

case class Area(value: String)

object Area extends CodecCsv.Enum[Area]("Area", new Area(_), _.value)

