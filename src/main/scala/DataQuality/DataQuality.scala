package DataQuality

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object DataQuality {
  @tailrec
  def getValue[T](value: Either[QualityError, T],
                  checks: List[T => Either[QualityError, T]])
                  : Either[QualityError, T] = {
//    checks match {
//      case Nil => Right(value)
//      case fun :: _ => {
//        val newValue = checks.head(value)
//        newValue match {
//          case Right(value) => getValue(value, checks.tail)
//          case Left(value) => Left(value)
//        }
//      }
//    }
    value match {
      case Left(exception) => Left(exception)
      case Right(value) =>
        checks match {
          case Nil => Right(value)
          case x :: xs => getValue(x(value), xs)
        }
    }
  }
}
