object test {

  val f: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x::y::rest => "two"
  }
  f(List(1, 2, 3))
  val g: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x::rest =>
      rest match {
        case Nil => "two"
      }
  }
  g.isDefinedAt(List(1,2,3))
  g(List(1,2,3))

}