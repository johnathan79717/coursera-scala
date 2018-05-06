import scala.annotation.tailrec

def factorial(x: Int) = {
  @tailrec
  def factorial(x: Int, acc: Int): Int =
    if (x == 1) acc
    else factorial(x-1, acc * x)
  factorial(x, 1)
}

println(factorial(10))
