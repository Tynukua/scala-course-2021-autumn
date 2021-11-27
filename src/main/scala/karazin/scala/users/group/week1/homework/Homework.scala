package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean = if (b) false else true

    def and(left: Boolean, right: Boolean): Boolean = 
      if (not (left)) false else if (not(right)) false else true

    def or(left: Boolean, right: Boolean): Boolean = 
      if (left) true else if (right) true else false

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (l: BigInt, r: BigInt) => 
      List(BigInt(0) to r).map(x=>l).fold(BigInt(0))(_+_)

    val power: (BigInt, BigInt) => BigInt = (r: BigInt, l: BigInt) => 
      List(BigInt(0) to r).map(x=>l).fold(BigInt(1))(multiplication(_,_))

    val fermatNumber: Int => BigInt = 
      (a: Int) => power(BigInt(2), power(BigInt(2), a )) 


  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = ???

  end `Look-and-say Sequence`

end Homework
