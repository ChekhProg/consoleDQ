import java.util.logging.Logger

case class Person(name: String, age: Int)

object Person {
  import DataQuality.QualityChecks._
  import DataQuality.DataQuality._
  def fromStrings(ls: List[String]): Option[Person] = {
    val log = Logger.getLogger("DataQuality")
    val name = getValue(Right(ls(0)), List(isLetters, startsWithCapital))
    val age = getValue(castToInt(ls(1)), List(isPositiveInt))
    val person = for {x <- name; y <- age} yield Person(x, y)
    person.toOption match {
      case None => {
        log.info("Can't create object Person(name, age):\n" +
                      s"name: $name\n" +
                      s"age: $age\n")
        None
      }
      case Some(value) => Some(value)
    }
  }
}