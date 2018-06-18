package codecxml

import org.scalatest.FreeSpec

import scala.{xml => X}

class Example extends FreeSpec {
  "foo" - {
    val pretty = new X.PrettyPrinter(100, 2)

//    val noteResult = Note.nodeCodec.decode(
//      <note heading="Reminder">
//        <to>Rick</to>
//        <from>Jerry</from>
//        <body>Hungry for apples ?</body>
//      </note>
//    )
//
//    println(noteResult)
//    println(pretty.format(Note.nodeCodec.encode(Note("Rick", "Jerry", "Reminder", "Hungry for apples ?"))))

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
    ))).toNode))
  }
}

case class Notes(topic: String, notes: List[Note])

object Notes {
  implicit val notes: Codec[Notes] = Codec(Notes.apply _, Notes.unapply _)(
    _.attribute("topic"), _.element("note")
  )
}

case class Note(to: String, from: String, heading: String, body: String)

object Note {
  implicit val nodeCodec: Codec[Note] = Codec(Note.apply _, Note.unapply _)(
    _.element("to"), _.element("from"), _.attribute("heading"), _.element("body")
  )
}

case class Foo(name: String)

object Foo {
  val fooCodec: Codec[Foo] =
    Codec(Foo.apply _, Foo.unapply _)(_.attribute("name"))
}