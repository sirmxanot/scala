object rationals {
  val x = new Rational(1,3)
  val x = new Rational(5,7)
  val x = new Rational(3,2)
  x.numer 
  x.denom
  x - y - z
  y + y
  x < y
  x > y

}

class Rational(x: Int, y: Int) {
  //require is used to enforce a precondition on the caller of a function
  //assert is used to check the code of the function itself
  require(y > 0, "denominator must be positive")

  // if 'this' is used as a function, it means the constructor of the
  //class
  def this(x: Int) = this(x,1)

  // Private functions and values are only accessable from within the
  // enclosing class
  // gcd function simplifies rational objects when they're initialized
  private def gcd(a: Int, b: Int): Int = 
    if (b == 0) a
    else gcd(b,a % b)
  private val g = gcd(x,y)
  def numer  = x / g
  def denom  = y / g

  // numer in the below function is equivalent to this.numer
  def < (r2: Rational) = numer * r2.denom < r2.numer * denom

  // Inside a class, the name "this" represents the object on which the 
  // current method is executed
  def > (r2: Rational) = if (this < r2) r2 else this

  //def toFloat: Float = numer / denom

  def unary_- : Rational = new Rational(-numer,denom)

  def + (r2: Rational): Rational = 
    new Rational(
      numer*r2.denom + r2.numer*denom,
      denom*r2.denom)

  def - (r2: Rational): Rational = this + -r2

  def multiply(r2: Rational): Rational = 
    new Rational(
      numer*r2.numer,
      denom*r2.denom) 

  def divide(r2: Rational): Rational = 
    new Rational(
      numer*r2.denom,
      denom*r2.numer)

  def equal(r2: Rational): Boolean = 
    numer*r2.denom == denom*r2.numer

  override def toString = numer + "/" + denom

}  