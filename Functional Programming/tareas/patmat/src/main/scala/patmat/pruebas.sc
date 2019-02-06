val a = List('d','c','a', 'b', 'a')

a.toString

a.toSet[Char]

a.count(x => x == 'a')

val b = ( for( x <- a.toSet[Char] ) yield ( x,a.count(y => y == x) ) ).toList


b.sortBy(  x => -x._2)