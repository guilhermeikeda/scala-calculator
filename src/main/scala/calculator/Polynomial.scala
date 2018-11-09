package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      (b() * b()) - (4 * c() * a())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // (-b ± √Δ) / 2a
    val sqrtDelta = Signal(math.sqrt(delta()))
    Signal {
        if(sqrtDelta() < 0)
          Set()
        else
            Set(((-b() + sqrtDelta()) / (2 * a())),(-b() - sqrtDelta()) / (2 * a()))
    }
  }
}
