package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val l1 = Leaf('a',1)
    val l2 = Leaf('b',2)
    val s1 = "abbccc"
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("makeCodeTree from two leafs") {
    new TestTrees {
      assert(makeCodeTree(l1,l2) === Fork(l1,l2,List('a','b'),3))
    }
  }

  test("string2Chars from a string to a list") {
    new TestTrees {
      assert(string2Chars(s1) === List('a','b','b','c','c','c'))
    }
  }

  test("times: single character") {
    new TestTrees {
      assert(times(List('a')) === List(('a', 1)))
    }  
  }
  
  test("times: twice the same character") {
    new TestTrees {
      assert(times(List('a', 'a')) === List(('a', 2)))
    }  
  }
  
  test("times: different characters") {
    new TestTrees {
      val result = times(List('a', 'b', 'a', 'c'))
      assert(result.size === 3)
      assert(result.contains(('a', 2)))
      assert(result.contains(('b', 1)))
      assert(result.contains(('c', 1)))
    }  
  }
  
  test("times: empty list") {
    new TestTrees {
      assert(times(List()) === List())
    }  
  }
  
  test("times: nil") {
    assert(times(Nil) === List())
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton tree is a singleton") {
    new TestTrees {
      assert(singleton(List(t1)) == true)
    }
  }

  test("non-singleton tree is not a singleton") {
    new TestTrees {
      assert(singleton(List(t1,t2)) == false)
    }
  }

  test("insert: append to tail") {
    val list = List(new Leaf('a', 2))
    val result = insert(new Leaf('b', 3), list)
    assert(chars(result(1)) === List('b'))
  }
  
  test("insert: prepend to head") {
    val list = List(new Leaf('a', 2))
    val result = insert(new Leaf('b', 1), list)
    assert(chars(result(0)) === List('b'))
  }
  
  test("insert: insert in the middle") {
    val list = List(new Leaf('a', 2), new Leaf('c', 4))
    val result = insert(new Leaf('b', 3), list)
    assert(chars(result(1)) === List('b'))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quickEncode: encode then decode gives identity") {
    val text = string2Chars("Will this work on the first try? No. Got the reversed text as a result ;-)")
    val codeTree = createCodeTree(text)
    val encodedText = quickEncode(codeTree)(text)
    val decodedText = decode(codeTree, encodedText)
    assert(decodedText === text)
  }
}
