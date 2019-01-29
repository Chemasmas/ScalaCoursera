package objsets

abstract class Nat {
  def isZero:Boolean
  def predecesor:Nat
  def succesor:Nat = new Succ(this)
  def +(that:Nat):Nat
  def -(that:Nat):Nat
}

object Zero extends Nat{
  def isZero: Boolean = true
  def predecesor: Nat = throw new Error("El zero no tiene predecesor")

  def +(that:Nat):Nat = that
  def -(that:Nat):Nat = if(that.isZero) this else throw new Error("El zero no tiene predecesor")
}

class Succ(n:Nat) extends Nat {
  def isZero: Boolean = false
  def predecesor:Nat = n

  def +(that:Nat):Nat = new Succ(n + that)
  def -(that:Nat):Nat = if(that.isZero) this else n - that.predecesor
}