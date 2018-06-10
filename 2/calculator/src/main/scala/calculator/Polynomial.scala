package calculator

import Math._

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      Var(pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      Var({
        val d = delta()
        if (d < 0) Set()
        else {
          val a1 = a()
          val b1 = b()
          Set((-b1 + sqrt(d)) / (2 * a1), (-b1 - sqrt(d)) / (2 * a1))
        }
      })
  }
}
