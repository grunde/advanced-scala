package instructor

object Helper {
  case class State[S,A](func: S => (S, A)) {
    def apply(s: S): (S, A) = func(s)
    def map[B](f: A => B): State[S, B] = State { s: S =>
      val (newS, a) = apply(s)
      (newS, f(a))
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s: S =>
      val (newS, a) = apply(s)
      f(a).apply(newS)
    }
  }
}
