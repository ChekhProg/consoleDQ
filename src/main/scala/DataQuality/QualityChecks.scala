package DataQuality

import scala.util.control.Exception.allCatch

class QualityChecks
object QualityChecks {
  def isLetters(string: String): Either[QualityError, String] = {
    if (string.forall(_.isLetter))
      Right(string)
    else
      Left(QualityError("Not all is letter symbols"))
  }
  def startsWithCapital(string: String): Either[QualityError, String] = {
    if (string.head.isUpper)
      Right(string)
    else
      Left(QualityError("Not starts with capital"))
  }
  def isInt(string: String): Either[QualityError, String] = {
    allCatch.opt(string.toInt) match {
      case Some(value) => Right(string)
      case None => Left(QualityError("Can't cast to int, incorrect symbols"))
    }
  }
  def isPositiveInt(string: String): Either[QualityError, String] = {
    if (string.toInt >= 0)
      Right(string)
    else
      Left(QualityError("Can't be a negative number"))
  }
}
