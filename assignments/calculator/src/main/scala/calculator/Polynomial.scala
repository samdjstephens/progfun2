package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var({
      val dlt = delta()
      if (dlt < 0) Set.empty
      else Set(
        -b() + Math.sqrt(delta())/ (2 * a()),
        -b() - Math.sqrt(delta())/ (2 * a())
      )
    })
  }
}
