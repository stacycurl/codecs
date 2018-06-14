package codecxml

import org.scalatest.FreeSpec

import scala.{xml => X}

class Example extends FreeSpec {
  "foo" - {
    val pretty = new X.PrettyPrinter(100, 2)

    val noteResult = Note.nodeCodec.decode(
      <note heading="Reminder">
        <to>Rick</to>
        <from>Jerry</from>
        <body>Hungry for apples ?</body>
      </note>
    )

    println(noteResult)
    println(pretty.format(Note.nodeCodec.encode(Note("Rick", "Jerry", "Reminder", "Hungry for apples ?"))))

    val notesResult = Notes.notes.decode(
      <thing topic="Notes">
        <note heading="Reminder">
          <to>Rick</to>
          <from>Jerry</from>
          <body>Hungry for apples ?</body>
        </note>
        <note heading="2nd reminder">
          <to>Jerry</to>
          <from>Rick</from>
          <body>Go away</body>
        </note>
      </thing>
    )

    println(notesResult)

    println(pretty.format(Notes.notes.encode(Notes("Notes", List(
      Note("Rick", "Jerry", "Reminder", "Hungry for apples ?"),
      Note("Jerry", "Rick", "2nd reminder", "Go away")
    )))))
  }
}

case class Notes(topic: String, notes: List[Note])

object Notes {
  implicit val notes: CodecElem[Notes] = CodecElem.codec2(Notes.apply, Notes.unapply)(
    _.attribute("topic"), _.element("note")
  )
}

case class Note(to: String, from: String, heading: String, body: String)

object Note {
  implicit val nodeCodec: CodecElem[Note] = CodecElem.codec4(Note.apply, Note.unapply)(
    _.element("to"), _.element("from"), _.attribute("heading"), _.element("body")
  )
}

case class Foo(name: String)

object Foo {
  val fooCodec: CodecElem[Foo] =
    CodecElem.codec1(Foo.apply, Foo.unapply)(_.attribute("name"))
}