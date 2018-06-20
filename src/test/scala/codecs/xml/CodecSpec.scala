package codecs.xml

import codecs.DecodeResult.Ok
import codecs.xml.DecodeNode.decode
import codecs.xml.EncodeNode.encode
import codecs.xml.internal.DecodeN
import org.scalatest.{FreeSpec, Matchers}
import sjc.delta.matchers.beIdenticalTo
import sjc.delta.std.xml.actualExpected._

import scala.{xml => X}


class CodecSpec extends FreeSpec with Matchers {
  private implicit class NodeSyntax(val self: X.Node) {
    def toElem: X.Elem = self.asInstanceOf[X.Elem]
  }

  "decodeOne" - {
    "string" in {
      assert(decode[String](X.Text("text")) === Ok("text"))
    }

    "element" in {

    }
  }

  "has-attribute" in {
    assert(decode[HasAttribute](<to value="something"/>) === Ok(HasAttribute("something")))

    encode(HasAttribute("something")).toElem should beIdenticalTo(<HasAttribute value="something"/>)
  }

  "has-text" in {
    assert(decode[HasText](<to>something</to>) === Ok(HasText("something")))

    encode(HasText("something")).toElem should beIdenticalTo(<HasText>something</HasText>)
  }

  "has-element" in {
    assert(decode[Human](<root><human>something</human></root>) === Ok(Human("something")))

    encode(Human("something")).toElem should beIdenticalTo(<Human><human>something</human></Human>)
  }

  "has-elements" in {
    assert(decode[HasElements](<to><e>1</e><e>2</e></to>) === Ok(HasElements(List("1", "2"))))

    encode(HasElements(List("1", "2"))).toElem should beIdenticalTo(<HasElements><e>1</e><e>2</e></HasElements>)
  }

  "has-different-elements" in {
    assert(decode[Beings](<Beings>
      <human>Rick</human>
      <alien>Birdperson</alien>
      <human>Morty</human>
      <alien>Squanchy</alien>
    </Beings>) === Ok(Beings(humans = List("Rick", "Morty"), aliens = List("Birdperson", "Squanchy"))))

    encode(Beings(humans = List("Rick", "Morty"), aliens = List("Birdperson", "Squanchy"))).toElem should beIdenticalTo(
      <Beings>
        <human>Rick</human>
        <human>Morty</human>
        <alien>Birdperson</alien>
        <alien>Squanchy</alien>
      </Beings>
    )
  }

  "nested" in {
    assert(decode[Universes](<Universes>
      <c132 id="132">
        <beings>
          <human>Rick</human>
          <alien>Birdperson</alien>
          <human>Morty</human>
          <alien>Squanchy</alien>
        </beings>
      </c132>
      <c137 id="137">
        <beings>
          <human>Jerry</human>
          <alien>Cronenberg Rick</alien>
          <human>Beth</human>
          <alien>Cronenberg Morty</alien>
        </beings>
      </c137>
    </Universes>) === Ok(Universes(
      C132("132", Beings(humans = List("Rick", "Morty"), aliens = List("Birdperson", "Squanchy"))),
      C137("137", Beings(humans = List("Jerry", "Beth"), aliens = List("Cronenberg Rick", "Cronenberg Morty")))
    )))

    encode(Universes(
      C132("132", Beings(humans = List("Rick", "Morty"), aliens = List("Birdperson", "Squanchy"))),
      C137("137", Beings(humans = List("Jerry", "Beth"), aliens = List("Cronenberg Rick", "Cronenberg Morty")))
    )).toElem should beIdenticalTo(
      <Universes>
        <c132 id="132">
          <beings>
            <human>Rick</human>
            <human>Morty</human>
            <alien>Birdperson</alien>
            <alien>Squanchy</alien>
          </beings>
        </c132>
        <c137 id="137">
          <beings>
            <human>Jerry</human>
            <human>Beth</human>
            <alien>Cronenberg Rick</alien>
            <alien>Cronenberg Morty</alien>
          </beings>
        </c137>
      </Universes>
    )
  }

  "decodeList" in {
    val decode = DecodeN.decodeList[String](codecs.xml.stringCodecXml).decodeNodes _

    assert(decode(Nil)                            === Ok(Nil))
    assert(decode(X.Text("text"))                 === Ok(List("text")))
    assert(decode(List(X.Text("1"), X.Text("2"))) === Ok(List("1", "2")))
  }

  "decodeOption" in {
    val decode = DecodeN.decodeOption[String](codecs.xml.stringCodecXml).decodeNodes _

    assert(decode(Nil)                            === Ok(None))
    assert(decode(X.Text("text"))                 === Ok(Some("text")))
    assert(decode(List(X.Text("1"), X.Text("2"))) === Ok(Some("1")))
  }
}

case class Value[A](value: A) {
  def &[B](other: Value[B]): Value[(A, B)] = Value[(A, B)]((value, other.value))
  def &:[B](other: Value[B]): Value[(A, B)] = Value[(A, B)]((value, other.value))



  def &&[B](other: Value[B]): Value[(B, A)] = Value[(B, A)]((other.value, value))
  def &&:[B](other: Value[B]): Value[(B, A)] = Value[(B, A)]((other.value, value))
}

object Value {
  type A = Int
  type B = String
  type C = Boolean
  type D = Double

  val a: Value[A] = Value[Int](123)
  val b: Value[B] = Value[String]("abc")
  val c: Value[C] = Value[Boolean](false)
  val d: Value[D] = Value[Double](456.0)

  val one:   Value[(((A, B), C), D)] = a &   b &   c &   d
  val two:   Value[(D, (C, (B, A)))] = a &&  b &&  c &&  d

  val three: Value[(((D, C), B), A)] = a &:  b &:  c &:  d


  val four:  Value[(A, (B, (C, D)))] = a &&: b &&: c &&: d

  println("foo")
}


case class HasAttribute(value: String)

object HasAttribute {
  implicit val codec: CodecXml[HasAttribute] = CodecXml(HasAttribute.apply _, HasAttribute.unapply _)(_.attribute("value"))
}

case class HasText(value: String)

object HasText {
  implicit val codec: CodecXml[HasText] = CodecXml(HasText.apply _, HasText.unapply _)(_.text)
}

case class Human(value: String)

object Human {
  implicit val codec: CodecXml[Human] = CodecXml(Human.apply _, Human.unapply _)(_.element("human"))
}

case class HasElements(values: List[String])

object HasElements {
  implicit val codec: CodecXml[HasElements] = CodecXml(HasElements.apply _, HasElements.unapply _)(_.element("e"))
}

case class Universes(c132: C132, c137: C137)

object Universes {
  implicit val codec: CodecXml[Universes] = CodecXml(Universes.apply _, Universes.unapply _)(
    _.element("c132"), _.element("c137")
  )
}

case class C132(id: String, beings: Beings)

object C132 {
  implicit val codec: CodecXml[C132] = CodecXml(C132.apply _, C132.unapply _)(_.attribute("id"), _.element("beings"))
}

case class C137(id: String, beings: Beings)

object C137 {
  implicit val codec: CodecXml[C137] = CodecXml(C137.apply _, C137.unapply _)(_.attribute("id"), _.element("beings"))
}

case class Beings(humans: List[String], aliens: List[String])

object Beings {
  implicit val codec: CodecXml[Beings] = CodecXml(Beings.apply _, Beings.unapply _)(
    _.element("human"), _.element("alien")
  )
}

