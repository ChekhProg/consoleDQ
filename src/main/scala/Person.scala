case class Person(name: String, age: Int)

object Person {
  import DataQuality.QualityChecks._
  import DataQuality._
  def apply(ls: List[String]): Person = {
    var name = ""
    var age = 0
    DataQuality.getValue(ls(0), List(isLetters, startsWithCapital)) match {
      case Right(value) => name = value
      case Left(exception) => throw exception
    }
    DataQuality.getValue(ls(1), List(isInt, isPositiveInt)) match {
      case Right(value) => age = value.toInt
      case Left(exception) => throw exception
    }
    new Person(name, age)
  }
}