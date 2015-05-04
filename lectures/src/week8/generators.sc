object generators {
  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }

  val booleans = integers.map(_ >= 0)

}