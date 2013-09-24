object exercise {
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a+1,b)

  def factorial(n: Int): Int = product(x => x)(1,n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b:Int): Int = 
      if (a > b) unit
      else combine(f(a),mapReduce(f,combine, unit)(a+1,b))

  def product_mr (f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x,y) => x * y, 1)(a,b)
  
  def sum_mr (f: Int => Int)(a: Int, b: Int): Int = 
    mapReduce(f, (x,y) => x + y, 0)(a,b)

}