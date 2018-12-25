def sum1(a:Int,b:Int):Int = {
  def acc(a:Int,acc:Int):Int = if (a>b) 0 else acc(a+1,acc+a)
  acc(a,0)
}


sum1(1,10)