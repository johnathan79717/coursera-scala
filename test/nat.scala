abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("Zero.predecessor")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) Zero else throw new Error("Zero -")
  override def toString = "Zero"
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def + (that: Nat) = (n + that).successor
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
  override def toString = "Succ(" + n + ")"
}

val one = new Succ(Zero)
val two = one + one
println(two)
println(two - Zero)
println(one)
println(two - one)
