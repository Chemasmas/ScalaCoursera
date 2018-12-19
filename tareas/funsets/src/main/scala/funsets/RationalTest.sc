class Rational(x:Int,y:Int){
  private def gcd(a:Int,b:Int):Int = if(b==0) a else gcd(b,a%b)
  private def g = gcd(x,y)
  def num = x / g
  def denom = y / g

  def +(otro:Rational):Rational = new Rational(num*otro.denom+otro.num*denom,denom*otro.denom)
  def -(otro:Rational):Rational = new Rational(num*otro.denom-otro.num*denom,denom*otro.denom)

  def *(otro:Rational):Rational = new Rational(num*otro.num,denom*otro.denom)

  def negate():Rational = new Rational(-num,denom)

  override def toString:String = {s"$num/$denom"}

}

new Rational(1,2)
new Rational(1,2) + new Rational(2,3)
new Rational(1,2).negate()
new Rational(1,2) - new Rational(1,4)

val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

x - y - z
