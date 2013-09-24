package example

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExampleSuite extends FunSuite {

  import Examples._

  test("product of a few numbers") {
    assert(sum(List(1,2,0,-3,-5,10)) === 5)
  }

}
