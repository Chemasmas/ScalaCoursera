package objsets

trait List[T]{
  def isEmpty:Boolean
  def head:T
  def tail:List[T]
}

class EmptyL[T] extends List[T]{
  def isEmpty:Boolean = true;
  def head:T = throw new Error("No head in empty List")
  def tail:List[T] = throw new Error("No tail in empty List")

  //override def toString():String = head + " -> " + tail
}

class NonEmptyL[T](val head:T,val tail:List[T]) extends List[T]{
  def isEmpty:Boolean = false;
  //override def toString():String = "Vacia"
}

object List {
  def apply[T](a:T,b:T): List[T] = new NonEmptyL( a, new NonEmptyL(b,new EmptyL ) )

  def apply[T](a:T): List[T] = new NonEmptyL(a,new EmptyL)

  def apply[T]: List[T] = new EmptyL
}