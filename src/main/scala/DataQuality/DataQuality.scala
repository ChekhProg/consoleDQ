package DataQuality

import scala.annotation.tailrec

object DataQuality {
  @tailrec
  def getValue(string: String, checks: List[String => Either[QualityError, String]]): Either[QualityError, String] = {
    checks match {
      case Nil => Right(string)
      case fun :: _ => {
        val newString = checks.head(string)
        newString match {
          case Right(value) => getValue(value, checks.tail)
          case Left(value) => Left(value)
        }
      }
    }
  }
}
