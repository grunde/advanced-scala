package misc

object FlattenMap extends App {
  val map: Map[String, Any] = Map("a" -> "b", "c" -> Map("d" -> "e", "f" -> Map("g" -> "h")))

  def flattenMap(m: Map[String, Any]): Map[String, String] = {
    def flatten(m: Map[String, Any]): Vector[Vector[String]] = {
      m.foldLeft(Vector.empty[Vector[String]]) {
        case (acc, (key: String, value: String)) =>
          val strings: Vector[String] = Vector.apply[String](key, value)
          val v: Vector[Vector[String]] = acc.:+(strings)
          v
        case (acc, (key, value: Map[String, Any])) =>
          val seq =  flatten(value)
          acc ++ seq.map(_.+:(key))
      }
    }

    flatten(m) map { seq =>
      val (keys, vals) = seq.splitAt(seq.length - 1)
      keys.mkString(".") -> vals.head
    } toMap
  }

  println(flattenMap(map))
}
