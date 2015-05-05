package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {

    val delta: Double = b() * b() - 4 * a() * c()
    Signal(delta)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    val solutionSet: Set[Double] = Set()
    val delta = computeDelta(a, b, c)

    if (delta() < 0)
      Signal(solutionSet)
    else if (delta() == 0) {
      val root = -b() / (2.0 * a())
      Signal(solutionSet + root)
    }
    else {
      val root1 = (-b() + scala.math.sqrt(delta())) / (2 * a())
      val root2 = (-b() - scala.math.sqrt(delta())) / (2 * a())
      Signal(solutionSet + root1 + root2)
    }

  }
}
