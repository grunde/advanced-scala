package homework

import instructor.Helper.State

object FunctionCallMonitor extends App {
  implicit class Counts(map: Map[String, Int]) {
    def inc(key: String, value: Int = 1) = {
      map.updated(key, map.getOrElse(key, 0) + value)
    }
  }

  type MethodCounter[A] = State[Map[String, Int], A]

  object MethodCounter {
    def apply[A](fn: Map[String, Int] => (Map[String, Int], A)): MethodCounter[A] = {
      State[Map[String, Int], A](fn)
    }
  }

  class ApiExecutor {
    def foo(x: Int): MethodCounter[String] = MethodCounter[String] {
      counts => (counts.inc("foo"), x.toString)
    }

    def bar(x: Int, y: Int): MethodCounter[String]= MethodCounter[String] {
      counts => (counts.inc("bar"), (x + y).toString)
    }
  }

  val api = new ApiExecutor
  val result: MethodCounter[String] = for {
    _ <- api.foo(1)
    _ <- api.bar(1,3)
    x <- api.bar(1,1)
  } yield x

  println(result)



}
