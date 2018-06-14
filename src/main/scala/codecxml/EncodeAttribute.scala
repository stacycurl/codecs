package codecxml

trait EncodeAttribute[A] {
  def encode(name: String, a: A): Attribute
}

object EncodeAttribute extends EncodeAttributeInstances {

}

trait EncodeAttributeInstances {
  implicit val stringEncodeAttribute: EncodeAttribute[String] =
    (name: String, value: String) => Attribute(name, value)
}
