package day2

import scala.concurrent.{ExecutionContext, Future}

object Functors extends App {

  trait Functor[F[_]] {
    def map[A,B](ma: F[A])(f: A => B): F[B]

//    def compose[G[_]](implicit FG: Functor[G]): Functor[F[G]]
  }

  // ########################################################

  // This will not allow us to control the execution context
//  object FutureFunctor extends Functor[Future] {
//    override def map[A, B](ma: Future[A])(f: (A) => B): Future[B] = {
//      ma.map(f)
//    }
//  }

  implicit def futureFunctor(implicit ec: ExecutionContext) = new Functor[Future] {
    override def map[A, B](ma: Future[A])(f: (A) => B): Future[B] = ma.map(f)
  }


  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
  futureFunctor.map(Future(1))(_ + 1).onComplete(println)
  Thread.sleep(100)

  // ########################################################





}
