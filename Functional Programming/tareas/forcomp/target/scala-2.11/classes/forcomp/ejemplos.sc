import forcomp. Anagrams.Occurrences

val jimmy = List(('o',1),('k',1))
val my = List(('o',1),('k',1))

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val a = y.map(x1 => {
    (x1, x.map( _._1 ).indexOf( x1._1 ) )
  }).toList
  def aux(occ: Occurrences, elim: List[((Char, Int), Int)]): Occurrences = {
    occ match {
      case Nil => occ
      case lista => {
        elim match {
          case Nil => occ
          case (xu, pos) :: xs => {
            //debo actualizar la posicion
            val pos2 = occ.map( _._1 ).indexOf( xu._1 )

            val (oc, ov) = lista(pos2)
            val (_, sv) = xu
            if (ov - sv == 0) {
              aux(lista.filter(z => z._1 != oc), xs)
            } else {
              aux(lista.updated(pos2, (oc, ov - sv)), xs)
            }
          }
        }
      }
    }
  }

  aux(x, a)
}

subtract(jimmy,my)



//def subtract(x: Occurrences, y: Occurrences): Occurrences = List

/*
val abba = List(('a', 2), ('b', 2))
val abbacomb = List(
  List(),
  List(('a', 1)),
  List(('a', 2)),
  List(('b', 1)),
  List(('a', 1), ('b', 1)),
  List(('a', 2), ('b', 1)),
  List(('b', 2)),
  List(('a', 1), ('b', 2)),
  List(('a', 2), ('b', 2))
)

val abbacomb2 = List(
  List(),
  List(('a', 2), ('b', 2)),
  List(('a', 1), ('b', 1)),
  List(('a', 2), ('b', 1)),
  List(('a', 1)),
  List(('a', 2)),
  List(('b', 1)),
  List(('b', 2)),
  List(('a', 1), ('b', 2)),
  List(('a', 2), ('b', 2))
)

def combinations(occurrences: Occurrences): List[Occurrences] = {


  def mapPos(occurrence:(Char,Int)): Occurrences = {
    occurrence match {
      case (c,x) => (0 to x).map( (c,_) ).toList
    }
  }

  def aux(tup:(List[Occurrences], Occurrences)): List[Occurrences] = {
    tup match {
      case (lista,Nil) => lista
      case (Nil,x) => {
        val a = for(
          x <- mapPos(x.head)
        ) yield {
          x :: Nil
        }
        aux( (a.toList,x.tail) )
      }
      case (lista, x ) => {
        val a = for(
          y <- lista;
          x <- mapPos(x.head)
        ) yield {
          x :: y
        }
        aux( (a.toList,x.tail) )
      }
    }
  }


  def eraseZeros(orig:List[Occurrences]):List[Occurrences] = {
      orig.map( x => x.filter( y => y._2!=0 ).reverse ).toList
  }

  eraseZeros( aux(Nil,occurrences) )
}


val c = combinations(abba).toSet
c.foreach(println)
c==abbacomb.toSet
abbacomb.toSet==abbacomb.toSet
c==c
abbacomb.toSet==abbacomb2.toSet
*/