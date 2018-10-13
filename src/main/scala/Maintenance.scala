
object Maintenance extends App {

  abstract class JSON
  case class JSeq (elems: List[JSON])             extends JSON
  case class JObj (bindings: Map[String, JSON])   extends JSON
  case class JNum (num: Double)                   extends JSON
  case class JStr (str: String)                   extends JSON
  case class JBool (b: Boolean)                   extends JSON
  case object JNull                               extends JSON

  val data = JObj(Map(
    "firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map(
      "street" -> JStr("21 st"),
      "state" -> JStr("NY"),
      "postalcode" -> JNum(10021)
    )),
    "phone" -> JSeq(List(
      JObj(Map(
        "type" -> JStr("home"),
        "number" -> JStr("21212121")
      )),
      JObj(Map(
        "type" -> JStr("fax"),
        "number" -> JStr("21212121")
      ))
    ))
  ))

  def show(json: JSON): String = json match {
    case JSeq(elems) =>
      "[" + (elems map show mkString ", ") + "]"
    case JObj(bindings) =>
      val assocs = bindings map {
        case (key, value) => "\"" + key + "\": " + show(value)
      }
      "{" + (assocs mkString ", ") + "}"
    case JNum(num) => num toString
    case JStr(str) => "\"" + str +"\""
    case JBool(b)  => b.toString
    case JNull     => "null"
  }

  println(show(data))

  val f: PartialFunction[String, String] = { case "ping" => "pong" }

  println(f.isDefinedAt("abc"))

  /*
  val account = for {
    JObj(bindings) <- data
    JSeq(phones) = bindings("phone")
    JObj(phone) <- phones
    JStr(digits) = phone("number")
    if digits.startsWith("212")
  } yield (bindings("firstName"), bindings("lastName"))
*/

  case class Book(title: String, authors: List[String])

  val books: Set[Book] = Set(
    Book("Structure and interpretation of Compupter", List("Abelson, Harald", "Sussman, Gerald J.")),
    Book("Introduction to functional programming", List("Bird, Richard", "Wadler, Phil")),
    Book("Effective Java", List("Bloch, Joshua")),
    Book("Java Puzzlers", List("Bloch, Joshua", "Gafter, Neal")),
    Book("Programming in Scala", List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")),
    Book("Programming in Scala 2", List("Bloch, Joshua"))
  )

  val b = for {
    b <- books
    if(b.title.contains("Program"))
  } yield b.title

  println(b)

  val twoers = for {
    b1 <- books
    b2 <- books
    if (b1.title < b2.title)
    a1 <- b1.authors
    a2 <- b2.authors
    if(a1 == a2)
  } yield a1

  println(twoers)


  for ( b <- books; a <- b.authors if a startsWith "Bird" ) yield b.title

  books flatMap(b =>  b.authors filter(a => a startsWith "Bird") map (y => y.title))
}
