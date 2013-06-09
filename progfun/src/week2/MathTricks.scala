// Notes on higher order functions

object MathTricks {
  // def sumInts(a: Int, b: Int): Int = 
  //   if (a > b) 0 else a + sumInts(a+1, b)

  // def cubes(x: Int): Int = x*x*x

  // def sumCubes(a: Int, b: Int): Int = 
  //   if (a > b) 0 else cube(a) + sumCubes(a+1,b)

  // def sumFactorials(a: Int, b: Int): Int = 
  //   if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)

  // higher-order functions
  def sum(f: Int => Int,a: Int, b: Int): Int = 
    if (a > b) 0 
    else f(a) + sumAny(f, a + 1, b)

  // def sumInts(a: Int, b: Int) = sum(id, a, b)
  // def sumCubes(a: Int, b: Int) = sum(cube, a, b)
  // def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

  def id(x: Int): Int = x
  def cube(x: Int): Int = x*x*x
  def fact(x: Int): Int = if (x == 0) 1 else fact(x-1)

  // anonymous functions
  def sumInts(a: Int, b: Int) = sum(x => x, a, b)
  def sumCubes(a: Int, b: Int) = sum(x => x*x*x, a, b)
  def sumFactorials(a: Int, b: Int) = 
    sum(x => if (x == 0) 1 else fact(x-1), a, b)

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)    
    }
    loop(a,0)
  }

  //currying
  // def sum(f: Int => Int): (Int, Int) => Int = {
  //   def sumF(a: Int, b: Int): Int = 
  //     if (a > b) 0
  //     else f(a) + sumF(a+1,b)
  //   sumF
  // }

  def sumInts = sum(x => x)
  def sumCubes = sum(x => x*x*x)
  def sumFactorials = sum(fact)

  // sum (cube) (1,10) == (sum(cube)) (1, 10)

  def sum(f: Int => Int)(a: Int, b: Int): Int = 
    if (a > b) 0 else f(a) + sum(f)(a+1,b)
    // type: (Int => Int) => (Int, Int) => Int

  //Exercises
  def product(f: Int => Int)(a: Int, b: Int): Int = 
    if (a > b) 1 else f(a) * product(f)(a+1,b)

  def factorial(n: Int) = product(x=>x)(1,n)

  // MapReduce generalizations
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int = 
    if (a > b) unit
    else combine(f(a),mapReduce(f, combine, zero)(a+1,b))

  def product(f: Int => Int)(a:Int, b: Int): Int = mapReduce(f,(x,y) => x*y, 1)(a, b)

  // Fixed point of a function

}