package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      val _a = a.apply()
      val _b = b.apply()
      val _c = c.apply()
      (_b * _b) - 4 * _a * _c
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val _a = a.apply()
      val _b = b.apply()
      val _delta = delta.apply()
      Set((-_b+Math.sqrt(_delta))/2.0*_a,(-_b - Math.sqrt(_delta))/2.0*_a)
    }
  }
}
