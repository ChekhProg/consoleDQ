import java.util.logging.Logger

case class Person(name: String, age: Int)

object Person {
  import DataQuality.QualityChecks._
  import DataQuality.DataQuality._
  def fromStrings(ls: List[String]): Option[Person] = {
    val log = Logger.getLogger("DataQuality")
    val name = getValue(Right(ls(0)), List(isLetters, startsWithCapital))
    val age = getValue(castToInt(ls(1)), List(isPositiveInt))
    if (name.isRight && age.isRight)
      Some(Person(name.getOrElse("undefined"), age.getOrElse(0)))
    else {
      log.info("Can't create object Person(name, age):\n" +
        s"name: $name\n" +
        s"age: $age\n")
      None
    }
  }
}