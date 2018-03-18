package day3

import day1.Monoids.Monoid
import instructor.Monad
import instructor.Monad.Monad


object WriterExercise extends App {

  implicit def writerMonad[W](implicit monoid: Monoid[W]): Monad[Lambda[A => Writer[W, A]]] = new Monad[Lambda[A => Writer[W, A]]] {
    override def pure[A](a: A): Writer[W, A] = Writer[W, A](monoid.zero, a)

    override def flatMap[A, B](ma: Writer[W, A])(f: (A) => Writer[W, B]): Writer[W, B] = {
      val newWriter = f(ma.a)
      Writer(monoid.append(ma.w, newWriter.w), newWriter.a)
    }
  }

  implicit class WriterOps[W, A](writer: Writer[W, A])(implicit monad: Monad[Writer[W, ?]]) {
    def flatMap[B](f: A => Writer[W, B]): Writer[W, B] = monad.flatMap(writer)(f)

    def map[B](f: A => B): Writer[W, B] = monad.map(writer)(f)
  }

  case class Writer[W, A](w: W, a: A)

  def foo1(input: String): Writer[Int, Int] = Writer(1, input.length)

  def foo2(input: Int): Writer[Int, Int] = Writer(1, input * 80)


  val x = for {
    r1 <- foo1("etan")
    r2 <- foo1("yaron")
    r3 <- foo2(123)
    r4 <- foo1("sasha")
  } yield (r1, r2, r3, r4)

  println(x)

}
