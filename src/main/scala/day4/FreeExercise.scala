package day4

import cats._
import cats.free.Free
import cats.free.Free.liftF
import cats.implicits

import scala.collection.mutable

/**
  * Your goal is to implement a security DSL using free monads.
  * The basic operations are:
  * Login(username, password) -- the outcome is a Option[User] object with (name, title and age)
  * IsAutorized(user : User, permission : Permission) with a Boolean outcome.
  * *
  * Please provide a simple Id interpreter using mutable authentication db (some hashmap).
  * Please provide an interpreter for testing which will use predefined results for known test users.
  */
object FreeExercise extends App {


  case class User(name: String, title: String, age: Int)

  sealed trait Permission {
    val level: Int
  }

  case object Admin extends Permission {
    override val level: Int = 1
  }

  // ADT

  sealed trait UserServiceADT[A]

  case class Login(username: String, password: String) extends UserServiceADT[Option[User]]

  case class IsAuthorized(user: User, permission: Permission) extends UserServiceADT[Boolean]

  case class AddUser(user: User, username: String, password: String, permission: Permission) extends UserServiceADT[Boolean]

  object UserService {
    type UserService[A] = Free[UserServiceADT, A]

    def login(username: String, password: String): UserService[Option[User]] =
      liftF[UserServiceADT, Option[User]](Login(username, password))

    def isAuthorized(user: User, permission: Permission): UserService[Boolean] =
      liftF[UserServiceADT, Boolean](IsAuthorized(user, permission))

    def addUser(user: User, username: String, password: String, permission: Permission): UserService[Boolean] =
      liftF[UserServiceADT, Boolean](AddUser(user, username, password, permission))
  }

  // Implementation

  def userServiceInterpreter: (UserServiceADT ~> Id) = new (UserServiceADT ~> Id) {
    val users: mutable.Map[(String, String), User] = mutable.Map.empty
    val permissions: mutable.Map[User, Permission] = mutable.Map.empty

    override def apply[A](fa: UserServiceADT[A]): Id[A] = {
      fa match {
        case Login(username, password) => users.get((username, password))
        case IsAuthorized(user, permission) => permissions.get(user).exists(_.level >= permission.level)
        case AddUser(user, username, password, permission) =>
          users.+=((username, password) -> user)
          permissions.+=(user -> permission)
          true
      }

    }
  }

  import UserService._

  val program: Free[UserServiceADT, Boolean] = for {
    _ <- addUser(User("yaron", "GM", 100), "yarons", "Suson@1478", Admin)
    yaronOpt <- login("yarons", "Suson@1478")
    isAuthorizedAns <- yaronOpt.fold(Free.pure[UserServiceADT,Boolean](false))(yaron => isAuthorized(yaron, Admin))
  } yield isAuthorizedAns

  println(program.foldMap(userServiceInterpreter))
}
