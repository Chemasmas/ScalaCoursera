def sum1(a:Int,b:Int):Int = {
  def accf(a:Int,acc:Int):Int ={
    if( a > b) acc else accf(a + 1, acc + a)
  }
  accf(a,0)
}

def sum2(f:Int => Int,a:Int,b:Int):Int = {
  def accf(a:Int,acc:Int):Int ={
    if( a > b) acc else accf(a + 1, acc + f(a) )
  }
  accf(a,0)
}

def sum3(f:Int=>Int)(a:Int,b:Int):Int = if( a > b) 0 else f(a) + sum3(f)(a+1,b)

def prod(f:Int=>Int)(a:Int,b:Int):Int = if( a> b) 1 else f(a) * prod(f)(a+1,b)

def fact(n:Int):Int = prod(x=>x)(1,n)

def mapReduce(f:Int=>Int, combine:(Int,Int)=>Int ,inv:Int)(a:Int,b:Int):Int = if (a>b) inv else combine(f(a),mapReduce(f,combine,inv)(a+1,b))


fact(6)
sum1(1,10)
mapReduce(x=>x,(a,b)=>a+b,0)(1,10)
sum2(x=>x,1,10)
sum2(x=>x*x,1,10)
sum2(x=>x+2,1,10)

def cube(x:Int):Int = x*x*x
def id(x:Int):Int = x

sum3(id)(1,10)
sum3(x=>x*x)(1,10)
sum3(x=>x+2)(1,10)

sum3(x=>x*x*x)(1,10)
sum3(cube)(1,10)

prod(id)(1,3)
mapReduce(x=>x,(a,b)=>a*b,1)(1,3)