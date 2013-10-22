package patmat

import common._

object Huffman {
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_,w) => w
    case Fork(_,_,_,w) => w 
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c,_) => List(c)
    case Fork(_,_,cs,_) => cs
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def countOccurances(char: Char) = chars.count(_ == char)
    val uniqueChars = chars.distinct
    uniqueChars.map(c => (c -> countOccurances(c)))
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    val leafs = freqs.map(elem => new Leaf(elem._1, elem._2))
    leafs.sortWith(_.weight <= _.weight )
  }

  def singleton(trees: List[CodeTree]): Boolean = trees.size == 1


  def insert(tree: CodeTree, trees: List[CodeTree]): List[CodeTree] = trees match {
    case List() => List(tree)
    case y :: ys => if (weight(tree) <= weight(y)) tree :: trees else y :: insert(tree, ys)
  }

  def combine(trees: List[CodeTree]): List[CodeTree] =  {
    if (trees.size < 2) trees
    else insert(makeCodeTree(trees(0), trees(1)),trees.drop(2))
  }

  def until(comparison_method: List[CodeTree] => Boolean, transformation_method: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
      if (comparison_method(trees)) trees
      else until(singleton, combine)(transformation_method(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = {
    val frequencies = times(chars)
    val leafsList = makeOrderedLeafList(frequencies)
    until(singleton,combine)(leafsList).head
  }

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = 
  {
    def accum(t: CodeTree, b: List[Bit], acc: List[Char]): List[Char] = 
    (t, b) match 
    {
      case (Leaf(c,_),head::tail) => accum(tree, b, acc :+ c)
      case (Leaf(c,_),_) => acc :+ c
      case (Fork(l,_,_,_),0::tail) => accum(l,tail,acc)
      case (Fork(_,r,_,_),1::tail) => accum(r,tail,acc)
      case _ => throw new IllegalArgumentException("improper tree")
    }
    accum(tree, bits, List())
  }


  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode,secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def accum(tree: CodeTree, char: Char, acc: List[Bit]): List[Bit] =  
    tree match {
      case Leaf(c,_) => acc
      case Fork(l,r,_,_) => 
        if (chars(l).contains(char)) accum(l,char, acc :+ 0)
        else accum(r,char,acc :+ 1)
    }
    text.foldLeft(List[Bit]())((encoded,char) => accum(tree,char,encoded))
  }

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.find(_._1 == char).get._2

  def convert(tree: CodeTree): CodeTable = {
    def accum(tree: CodeTree, acc: List[Bit]): CodeTable = tree match {
      case Leaf(c,_) => List((c -> acc))
      case Fork(l,r,_,_) => mergeCodeTables(accum(l,acc :+ 0), accum(r,acc :+ 1))
    }
    accum(tree, List[Bit]())
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ++ b

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)
    val encoded = List[Bit]()
    text.foldLeft(encoded)((encoded,char)=> encoded ::: codeBits(table)(char)) 
  }
}
