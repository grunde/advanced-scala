package day1



class Dumb(val name: String)

trait Equal[-T] {
  def areEqual(a: T, b: T): Boolean
}


object Implicits {
  implicit object DumbEq extends Equal[Dumb] {
    override def areEqual(a: Dumb, b: Dumb): Boolean = a.name == b.name
  }
}

object Day1 extends App {

  def areAllEqual[T](l: List[T]): Boolean = {
    l.toSet.size <= 1
  }

  def areAllEqual1[T](l: List[T]): Boolean = {
    l.headOption.forall { first =>
      l.foldLeft(true) { case (acc: Boolean, x: T) =>
        acc && x == first
      }
    }
  }

  def areAllEqual2[T](l: List[T]): Boolean = {
    l.headOption.forall { first =>
      l.forall(_ == first)
    }
  }



  def areAllEqualWithTrait[T](l: List[T])(implicit eq: Equal[T]): Boolean = {
    l.headOption.forall { first =>
      l.forall(eq.areEqual(_, first))
    }
  }

  println(areAllEqual(List(1,1,1)))
  println(areAllEqual(List("1","1","1")))
  println(areAllEqual1(List(1,1,1)))
  println(areAllEqual1(List("1","1","2")))

  import Implicits._
  println(areAllEqualWithTrait(List(new Dumb("af"), new Dumb("af"), new Dumb("af"))))


}