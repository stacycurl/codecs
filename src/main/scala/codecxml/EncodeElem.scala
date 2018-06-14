package codecxml

import scala.reflect.ClassTag
import scala.{xml => X}

trait EncodeElem[A] {
  def encode(a: A)(implicit C: ClassTag[A]): X.Elem = encode(EncodeElem.tagFor[A], a)

  def encode(name: String, a: A): X.Elem
}

object EncodeElem extends EncodeElemInstances {
  def tagFor[A: ClassTag]: String = {
    def uncapitalise(value: String): String =
      s"${value.charAt(0).toLower}${value.substring(1)}"

    uncapitalise(implicitly[ClassTag[A]].runtimeClass.getSimpleName)
  }
}

trait EncodeElemInstances {
//  implicit def listEncodeElem[A: EncodeElem]: EncodeElem[List[A]] =
//    (name: String, values: List[A]) => ???
//
  implicit val stringEncodeElem: EncodeElem[String] =
    (name: String, value: String) => {
      X.Elem(null, name, X.Null, X.TopScope, false, X.Text(value))
    }
}
