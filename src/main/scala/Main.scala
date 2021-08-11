object Main extends App {
  print("Введите строку CSV: ")
  val s = scala.io.StdIn.readLine()
  val arr = s.split(",")
  val person = Person(arr.toList)
  println(person)
}
