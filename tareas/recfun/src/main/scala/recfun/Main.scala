package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if (c==0 || c==r) 1 else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def clean(charsL: List[Char] ): List[Char] = charsL.filter( p => p == '(' || p == ')')

      def evalRec(i: Int, chars: List[Char]):Boolean =  if (i<0) false else if(chars.isEmpty) if( i == 0) true else false
                                                        else if (chars.head == '(') evalRec(i+1,chars.tail) else evalRec(i-1,chars.tail)

      evalRec(0,clean(chars) )
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def hasOneAtLeast(money:Int ,coins:List[Int]):Boolean = coins.map( x => money%x == 0 ).reduce( (a,b) => a | b )

      def clean(money:Int,coins:List[Int]):List[Int] = coins.filter(x=>x<=money)

      def generateWays(money:Int,coin:Int):List[Int] = {println(money);println(coin);println(money/coin); 0 to (money/coin) toList}

      //def calculate(money:Int,coins:List[Int]):Int => generateWays(money,coins.head)

      println( generateWays(money,clean(money,coins).sorted.reverse.head)  )
      //println(generateWays(300,200))
      //print(clean(money,coins).sorted.reverse)
      if (!hasOneAtLeast(money,clean(money,coins))) 0
      else -1
    }
  }
