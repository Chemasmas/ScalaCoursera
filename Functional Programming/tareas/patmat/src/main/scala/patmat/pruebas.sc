import patmat.Huffman._

def aux(tree:CodeTree)(bits:List[Bit]):(Char,List[Bit]) = {
  tree match {
    case Leaf(c, _) => (c, bits)
    case Fork(l, r, cs, _) => if (bits.head == 0) {
      aux(l)(bits.tail)
    } else {
      aux(r)(bits.tail)
    }
  }
}

def aux2(tree: CodeTree,bits:List[Bit])(acc:List[Char]):List[Char]= {
  if(bits.isEmpty) acc
  else {
    val a = aux(tree)(bits)
    aux2(tree,a._2)(a._1::acc)
  }
}


//def decode(tree: CodeTree, bits: List[Bit]): List[Char]
def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  val a = aux2(tree,bits)(Nil)
  a.reverse
}



val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

//def decodedSecret: List[Char] = decode(frenchCode,secret)
def decodedSecret: List[Char] = decode(frenchCode,secret)
decodedSecret.mkString

val izq = 0
val der = 1
def auxE(tree:CodeTree)(acc:List[Bit],text: List[Char]) : (List[Bit],List[Char]) = {
  //println(text.head)
  tree match {
    //case Leaf(c,_) => if(text.isEmpty) (acc,Nil) else (acc,text.tail)
    case Fork(Leaf(c1,_),Leaf(c2,_),_,_) => if(text.head==c1) (izq::acc,text.tail) else (der::acc,text.tail)
    case Fork(Fork(l1,r1,cs1,ws1),Fork(l2,r2,cs2,ws2),_,_) => if(cs1.contains(text.head)) auxE( Fork(l1,r1,cs1,ws1) )(izq::acc,text) else auxE( Fork(l2,r2,cs2,ws2) )(der :: acc,text)
    case Fork(Leaf(c,_),Fork(l2,r2,cs2,ws2),_,_) => if(cs2.contains(text.head)) auxE( Fork(l2,r2,cs2,ws2) )(der :: acc,text) else (izq::acc,text.tail)
    case Fork(Fork(l1,r1,cs1,ws1),Leaf(c,_),_,_) => if(cs1.contains(text.head)) auxE( Fork(l1,r1,cs1,ws1) )(izq :: acc,text) else (der::acc,text.tail)
  }
}
def auxE2(tree:CodeTree,text:List[Char])(acc:List[Bit]):List[Bit] = {
  if(text.isEmpty) acc
  else{
    val a = auxE(tree)(acc,text)
    auxE2(tree,a._2)(a._1)
  }
}

def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  val a = auxE2(tree,text)(Nil)
  a.reverse
}


encode(frenchCode)(string2Chars("huffmanestcool") )

def codeBits(table: CodeTable)(char: Char): List[Bit] = {
  table.filter( x => x._1==char ) match {
    case Nil => Nil
    case a :: _ => a._2
  }
}

def auxC(tree: CodeTree,char: Char)(acc:List[Bit]):List[Bit] = {
  tree match {
    //case Leaf(c,_) => if(text.isEmpty) (acc,Nil) else (acc,text.tail)
    case Fork(Leaf(c1,_),Leaf(c2,_),_,_) => if(char==c1) izq::acc else der::acc
    case Fork(Fork(l1,r1,cs1,ws1),Fork(l2,r2,cs2,ws2),_,_) => if(cs1.contains(char)) auxC( Fork(l1,r1,cs1,ws1),char )(izq::acc) else auxC( Fork(l2,r2,cs2,ws2),char )(der :: acc)
    case Fork(Leaf(c,_),Fork(l2,r2,cs2,ws2),_,_) => if(cs2.contains(char)) auxC( Fork(l2,r2,cs2,ws2),char )(der :: acc) else izq::acc
    case Fork(Fork(l1,r1,cs1,ws1),Leaf(c,_),_,_) => if(cs1.contains(char)) auxC( Fork(l1,r1,cs1,ws1),char )(izq :: acc) else der::acc
  }
}


/*val test4 = string2Chars("huffmanestcool").distinct
for(x<-test4) yield {
  (x,auxC(frenchCode,x)(Nil).reverse)
}*/

//type CodeTable = List[(Char, List[Bit])]

def convert(tree: CodeTree): CodeTable = {
  tree match {
    case Fork(l,r,cs,ws) => for(x<-cs) yield {(x,auxC(Fork(l,r,cs,ws),x)(Nil).reverse)}
  }
}


println(convert(frenchCode))



