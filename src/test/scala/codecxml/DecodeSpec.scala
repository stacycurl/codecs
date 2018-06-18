package codecxml

import codecxml.Decode.decode
import codecxml.Encode.encode
import codecxml.DecodeResult.Ok
import codecxml.internal.DecodeN

import scala.{xml => X}
import org.scalatest.FreeSpec

import scala.reflect.ClassTag

class DecodeSpec extends FreeSpec {
  "decodeOne" - {
    "string" in {
      assert(decode[String](X.Text("text")) === Ok("text"))
    }

    "element" in {

    }
  }



  "has-attribute" in {
    assert(decode[HasAttribute](<to value="something"/>) === Ok(HasAttribute("something")))

    log(HasAttribute("something"))
  }

  private def log[A: Encode: ClassTag](value: A): Unit = {
    val pretty = new X.PrettyPrinter(100, 2)

    val encoded = encode(value)
    println(pretty.formatNodes(encoded.toNode))
  }

  "has-text" in {
    assert(decode[HasText](<to>something</to>) === Ok(HasText("something")))

    log(HasText("something"))
  }

  "has-element" in {
    assert(decode[Human](<root><human>something</human></root>) === Ok(Human("something")))

    log(Human("something"))
  }

  "has-elements" in {
    assert(decode[HasElements](<to><e>1</e><e>2</e></to>) === Ok(HasElements(List("1", "2"))))

    log(HasElements(List("1", "2")))
  }

  "has-different-elements" in {
    assert(decode[Beings](
      <beings>
        <human>Rick</human>
        <alien>Birdperson</alien>
        <human>Morty</human>
        <alien>Squanchy</alien>
      </beings>
    ) === Ok(Beings(humans = List("Rick", "Morty"), aliens = List("Birdperson", "Squanchy"))))

    log(Beings(humans = List("Rick", "Morty"), aliens = List("Birdperson", "Squanchy")))
  }

  "nested" in {
    val universes = <universes>
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
    </universes>

    assert(Decode.decode[Universes](universes) === Ok(Universes(
      C132("132", Beings(humans = List("Rick", "Morty"), aliens = List("Birdperson", "Squanchy"))),
      C137("137", Beings(humans = List("Jerry", "Beth"), aliens = List("Cronenberg Rick", "Cronenberg Morty")))
    )))

//    log(Universes(
//      C132(Beings(humans = Nil, aliens = Nil)),
//      C137(Beings(humans = Nil, aliens = Nil))
//    ))
//
    log(Universes(
      C132("132", Beings(humans = List("Rick", "Morty"), aliens = List("Birdperson", "Squanchy"))),
      C137("137", Beings(humans = List("Jerry", "Beth"), aliens = List("Cronenberg Rick", "Cronenberg Morty")))
    ))
  }

  "decodeList" in {
    val decode = DecodeN.decodeList[String](Decode.stringDecode).decode _

    assert(decode(Nil)                            === Ok(Nil))
    assert(decode(X.Text("text"))                 === Ok(List("text")))
    assert(decode(List(X.Text("1"), X.Text("2"))) === Ok(List("1", "2")))
  }

  "decodeOption" in {
    val decode = DecodeN.decodeOption[String](Decode.stringDecode).decode _

    assert(decode(Nil)                            === Ok(None))
    assert(decode(X.Text("text"))                 === Ok(Some("text")))
    assert(decode(List(X.Text("1"), X.Text("2"))) === Ok(Some("1")))
  }
}

case class HasAttribute(value: String)

object HasAttribute {
  implicit val codec: Codec[HasAttribute] = Codec(HasAttribute.apply _, HasAttribute.unapply _)(_.attribute("value"))
}

case class HasText(value: String)

object HasText {
  implicit val codec: Codec[HasText] = Codec(HasText.apply _, HasText.unapply _)(_.text)
}

case class Human(value: String)

object Human {
  implicit val codec: Codec[Human] = Codec(Human.apply _, Human.unapply _)(_.element("human"))
}

case class HasElements(values: List[String])

object HasElements {
  implicit val codec: Codec[HasElements] = Codec(HasElements.apply _, HasElements.unapply _)(_.element("e"))
}

case class Universes(c132: C132, c137: C137)

object Universes {
  implicit val codec: Codec[Universes] = Codec(Universes.apply _, Universes.unapply _)(
    _.element("c132"), _.element("c137")
  )
}

case class C132(id: String, beings: Beings)

object C132 {
  implicit val codec: Codec[C132] = Codec(C132.apply _, C132.unapply _)(_.attribute("id"), _.element("beings"))
}

case class C137(id: String, beings: Beings)

object C137 {
  implicit val codec: Codec[C137] = Codec(C137.apply _, C137.unapply _)(_.attribute("id"), _.element("beings"))
}

case class Beings(humans: List[String], aliens: List[String])

object Beings {
  implicit val codec: Codec[Beings] = Codec(Beings.apply _, Beings.unapply _)(
    _.element("human"), _.element("alien")
  )
}

