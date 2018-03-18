package day3

import cats.data.Writer
import cats.implicits._

object WriterWithCats extends App {


  def gcd(a: Int, b: Int): Writer[List[(Int, Int)], Int] = {
    if (a % b == 0) Writer(List((a, b)), b)
    else for {
      _ <- Writer.tell(List((a,b)))
      r <- gcd(b, a % b)
    } yield r
  }

  println(gcd(100, 16).run)
}


