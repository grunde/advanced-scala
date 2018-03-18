package instructor

object Monad {
  trait Functor[F[_]] { self =>
    def map[A,B](ma: F[A])(f: A => B): F[B]
    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def compose[G[_]](implicit FG: Functor[G]):
    Functor[Lambda[A => F[G[A]]]] =
      new Functor[Lambda[A => F[G[A]]]] {
        override def map[A, B](ma: F[G[A]])(f: A => B): F[G[B]] =
          self.map(ma)(ga => FG.map(ga)(f))
      }
  }


  trait Monad[M[_]] extends Functor[M] {
    def pure[A](a: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
    def sequence[A](lma: List[M[A]]) : M[List[A]] =
      lma.foldLeft(pure(List.empty[A])){
        (mla: M[List[A]], ma: M[A]) =>
          flatMap(mla){l:List[A] => map(ma){a:A => l.+:(a)}}
      }
  }
}
