package reductions

import scala.annotation._
import org.scalameter._
import common._
import Math.min

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var i = 0
    var acc = 0
    while (i < chars.length) {
      acc += (chars(i) match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      })
      if (acc < 0) {
        return false
      }
      i += 1
    }
    return acc == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int) : (Int, Int) = {
      var i = idx
      var acc = 0
      var minimum = 0
      while (i < until) {
        chars(i) match {
          case '(' => acc += 1
          case ')' => {
            acc -= 1
            if (acc < minimum) {
              minimum = acc
            }
          }
          case _ => {}
        }
        i += 1
      }
      (acc, minimum)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until)
      else {
        val mid = (from + until) / 2
        val ((x1, y1), (x2, y2)) = parallel(reduce(from, mid), reduce(mid, until))
        (x1 + x2, min(y1, y2))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
