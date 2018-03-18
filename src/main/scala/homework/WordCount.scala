package homework
import instructor.Monoids._

object WordCount extends App {
  def wordCount(str: String): Map[String, Int] = {
    val words = str.split("\\s+").toList.mapreduce(x => Map(x -> 1))
    words
  }

  val story =
    """gil is your friend he is also my friend too
      |but I love you so why you don't love me
    """.stripMargin

  println(wordCount(story))
}
