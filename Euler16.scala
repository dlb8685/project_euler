// https://projecteuler.net/problem=16
/*
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
*/

import scala.math.BigDecimal

object Euler16 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	def powerOfCustom(x: BigInt, of: Int, tot: BigInt = -1): BigInt = of match {
			// recursive function to use BigInt type because native Scala powerOf uses Double and doesn't save all digits, switches to E34 notation.
			// some more detail on why E34 is critical: https://stackoverflow.com/questions/21416525/scalahow-to-make-the-bigdecimal-is-exact-to-integer-part
		case 1 => { if (tot == -1) x else tot }
		case _ => { if (tot == -1) powerOfCustom(x, of - 1, (x * x)) else powerOfCustom(x, of - 1, (tot * x)) }
	}
	
	def sumOf2toXDigits(x: BigInt): Int = {
		x.toString.toArray.filter(x => x != '.').map(x => x.asDigit).sum
		}
	
	def solution1(): Int = {
		val rawNum: BigInt = powerOfCustom(2, 1000)
		sumOf2toXDigits(rawNum)
	}
	
	def main(args: Array[String]) {

		time{ println(solution1()) }
		// Elapsed time: 21.546704ms
	}
}

