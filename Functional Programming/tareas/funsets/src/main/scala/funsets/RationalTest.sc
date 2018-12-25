class Rational(x:Int,y:Int){
  require(y!=0,"denominador debe de ser diferente de cero")

  def this(x:Int) = this(x,1)

  private def gcd(a:Int,b:Int):Int = if(b==0) a else gcd(b,a%b)
  private def g = gcd(x,y)
  def num = x/g
  def denom = y/g

  def +(otro:Rational):Rational = new Rational(num*otro.denom+otro.num*denom,denom*otro.denom)
  def -(otro:Rational):Rational = this + - otro

  def *(otro:Rational):Rational = new Rational(num*otro.num,denom*otro.denom)

  def unary_- :Rational = new Rational(-num,denom)

  override def toString:String = {s"${num}/${denom}"}

  def <(otro:Rational):Boolean = num * otro.denom < otro.num * denom
  def max(otro:Rational):Rational = if(this < otro ) otro else this

}

new Rational(1,2)
new Rational(1,2) + new Rational(2,3)
- new Rational(1,2)
new Rational(1,2) - new Rational(1,4)

val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)
val zz = new Rational(2,256)

zz
x - y - z + zz

new Rational(1,2)<new Rational(1,3)
new Rational(1,3)< new Rational(1,2)
new Rational(1,3) max new Rational(1,2)

new Rational(2) + new Rational(3) * new Rational(4)

