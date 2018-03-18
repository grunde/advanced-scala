package day3

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object SentimentExercise extends App {
  def getUserIdFromToken(token: String): EitherT[Future, Exception, String] = EitherT.right(Future.successful("id"))

  def getUserDescriptionById(userId: String): EitherT[Future, Exception, String] = EitherT.fromEither(Right("description"))

  def calcSentiment(description: String): EitherT[Future, Exception, String] = EitherT.fromEither(Right("sentiment"))

  def getSentimentForUserToken(token: String): EitherT[Future, Exception, String] = {
    for {
      id <- getUserIdFromToken(token)
      description <- getUserDescriptionById(id)
      sentiment <- calcSentiment(description)
    } yield sentiment
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = (aOpt: Option[A]) => aOpt.map(f)

}
