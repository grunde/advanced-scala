package instructor

object Monoids extends App {

  // Monoid[Map[K,V]]

  implicit object IntMonoid extends Monoid[Int] {
    override def zero: Int = 0

    override def append(a: Int, b: Int): Int = a + b
  }

  implicit object StringMonoid extends Monoid[String] {
    override def zero: String = ""

    override def append(a: String, b: String): String = a + b
  }
  trait Monoid[M] {
    def zero: M
    def append(a: M, b:M): M
  }

  implicit def mapMonoid[K,V](implicit vMono: Monoid[V]):
  Monoid[Map[K,V]] =
    new Monoid[Map[K,V]] {
      override def zero = Map.empty

      override def append(a: Map[K, V], b: Map[K, V]) = {
        val smaller = if (a.size > b.size) b else a
        val bigger = if (a.size > b.size) a else b
        smaller.foldLeft(bigger) {
          case (acc, (k,v)) =>
            acc + (k -> vMono.append(v, acc.getOrElse(k, vMono.zero)))
        }
      }

      def append1(a: Map[K, V], b: Map[K, V]) = {
        val keys = a.keySet ++ b.keySet
        (for {
          k <- keys
        } yield (k, vMono.append(a.getOrElse(k, vMono.zero),
          b.getOrElse(k,vMono.zero)))).toMap
      }
    }

  implicit def optionMonoid[T](implicit tmono: Monoid[T]) =
    new Monoid[Option[T]] {
      override def zero = Some(tmono.zero)

      override def append(a: Option[T], b: Option[T]) = {
        for {
          aValue <- a
          bValue <- b
        } yield tmono.append(aValue, bValue)
      }
    }

  println(List(1,2,3).reduce(IntMonoid.append))
  println(List(Option(1),Option(2),Option(3))
    .reduce(optionMonoid(IntMonoid).append))

  implicit class RichList[T](list: List[T]) {
    def richReduce(implicit monoid: Monoid[T]): T = {
      list.reduce(monoid.append)
    }

    def mapreduce[B](f: T => B)(implicit mb: Monoid[B]): B =
      list.foldLeft(mb.zero){
        (acc, elem) => mb.append(acc, f(elem))
      }

  }

  println(List(1,2,3).richReduce)
  println(List(Option(1),Option(2),Option(3)).richReduce)

  println(Map("a" ->1, "b" -> 2) ++ Map("a" -> 3, "c" -> 5))

  println(Map("a" -> 4, "b" -> 2 , "c" -> 5))



}
