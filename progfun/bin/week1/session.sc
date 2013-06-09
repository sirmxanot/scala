object session {
  def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double = 
      if (isGoodEnough(guess)) guess else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) = 
      abs(guess*guess - x) / x < 0.0001

    def abs(x:Double) = if (x >= 0) x else -x

    def improve(guess: Double) = ((x/guess) + guess)/2

    sqrtIter(1.0)
  }
}