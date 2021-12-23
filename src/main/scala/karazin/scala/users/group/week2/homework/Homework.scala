package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    @targetName("addition")
    infix def +(that: Rational): Rational = Rational(this.x*that.y+ that.x*this.y, this.y*that.y)

    @targetName("negation")
    infix def unary_- : Rational = Rational(-this.x, this.y)

    @targetName("substraction")
    infix def -(that: Rational): Rational =  Rational(this.x*that.y+ that.x*this.y, this.y*that.y)

    @targetName("multiplication")
    infix def *(that: Rational): Rational = Rational(this.x*that.x, this.y*that.y)

    @targetName("devision")
    infix def /(that: Rational): Rational = Rational(this.x*that.y, this.y*that.x)

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)

    override def equals(other: Any): Boolean = (this.denom == that.denom) && (this.numer == this.numer)

  end Rational

end Homework
