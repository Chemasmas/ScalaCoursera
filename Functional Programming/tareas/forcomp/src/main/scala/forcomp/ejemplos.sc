import forcomp. Anagrams.Occurrences

val a:Occurrences = List(('a', 2), ('b', 2))
println(a)
//List[Occurrences]
//def combinations(occurrences: Occurrences): List[Occurrences] = ???
val b:List[Occurrences] = a.map( x=>(0 to x._2).map( y => (x._1,y)).toList )

def combinations(occurrences: Occurrences): List[Occurrences] = {
  occurrences match {
    case  Nil => List(List())
  }

  def aux(tup:Tuple2[List[Occurrences],Occurrences]): List[Occurrences] = {
    tup match {
      case (lista,Nil) => lista
        
    }
  }
}


combinations(a);

