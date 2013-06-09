object mathtricks {
  def gcd(a: Int, b:Int): Int = 
    if (b == 0) a else gcd(b, a % b)

  // regular recursion
  def factorial(n: Int): Int = 
    if (n == 0) 1 else n * factorial(n-1)

  // tail recursive version
  def factorial_tail(n: Int): Int = {
    def factorial(n: Int, total: Int): Int =
      if (n == 0) total
      else factorial(n-1, total*n)
    factorial(n, 1)
  }

  def sumInts(a: Int, b: Int): Int = 
    if (a > b) 0 else a + sumInts(a + 1, b)

  def cube(x: Int): Int = x * x * x

  def sumCubes(a: Int, b: Int): Int = 
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

  def sumFactorials(a: Int, b: Int): Int = 
    if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)


}