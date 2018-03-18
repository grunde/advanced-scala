package day1

object Monoids extends App {

  trait Monoid[M] {
    def zero: M

    def append(a: M, b: M): M
  }

  implicit def mapMonoid[K,V](implicit vmono: Monoid[V]) = {
    new Monoid[Map[K, V]] {
      override def zero: Map[K, V] = Map.empty[K,V]

      override def append(a: Map[K, V], b: Map[K, V]): Map[K, V] = {
        val keys = a.keySet ++ b.keySet
        val x = keys.map((key: K) =>
          key -> vmono.append(a.getOrElse(key, vmono.zero), b.getOrElse(key, vmono.zero))
        ).toMap
        x
      }
    }
  }


  implicit object StringMonoid extends Monoid[String] {
    override def zero: String = ""

    override def append(a: String, b: String): String = a + b
  }

  implicit object IntMonoid extends Monoid[Int] {
    override def zero: Int = 0

    override def append(a: Int, b: Int): Int = a + b
  }


  private val stringToInt: Map[String, Int] = Map("a" -> 1)
  private val stringToInt1: Map[String, Int] = Map("a" -> 1, "b" -> 2)
  println(mapMonoid[String, Int].append(stringToInt, stringToInt1))

}
