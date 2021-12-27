package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
import scala.compiletime.ops.string
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

    def not(b: Boolean): Boolean = if b then false else true

    def and(left: Boolean, right: Boolean): Boolean = 
      if not(left) then
        false
      else if not(right) then
        false
      else
        true
    end and
        

    def or(left: Boolean, right: Boolean): Boolean = 
      if (left) true else if (right) true else false

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (l: BigInt, r: BigInt) => 
      if l != BigInt(0) then
        r + multiplication(l - l.sign, r)
      else 
        l

    val power: (BigInt, BigInt) => BigInt = (r: BigInt, l: BigInt) => 
      if l == BigInt(0) then
        1
      else
        if l > 0 then
          multiplication(r, power(r, l - l.sign))
        else
          power(r, l - l.sign) / r


    val fermatNumber: Int => BigInt = 
      (a: Int) => 
        power(BigInt(2), power(BigInt(2), a)) + 1


  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = (n: Int) => 
      BigInt(lookAndSayInternal(n))
      
    def lookAndSayInternal(n: Int): String = 
      if n == 0 then "1"
      else 
        val prev = lookAndSayInternal(n-1)
        var result = "";
        var curChar = prev charAt 0 ;
        var curCharCount = 0;
        for (i <- 0 to prev.length-1)
          if curChar == prev.charAt(i) then
            curCharCount = curCharCount+1;
          else
            result = result concat curCharCount.toString() concat curChar.toString();
            curCharCount = 1
            curChar = prev charAt i
        // Concating tail seq 
        result = result concat curCharCount.toString() concat curChar.toString();
        result


  end `Look-and-say Sequence`

end Homework
