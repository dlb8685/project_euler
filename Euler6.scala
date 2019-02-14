// https://projecteuler.net/problem=6
/*
The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*/

object Euler6 {
	
	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
		
	
	def Solution1(): Int = {
		val naturalSumThru100: Int = (101 * 50)  // trivial, even for me. 1 + 100, 2 + 99, ... 50 + 51
		// I'm sure there's a glib way to do sum of x^2 through n with a one-liner, but Solution1 will use brute force.
		val list1to100: List[Int] = List.range(1,101)
		val squaredSumThru100: Int = list1to100.map(x => x*x).sum
		(naturalSumThru100 * naturalSumThru100) - squaredSumThru100
	}
	
	// *after* looking at external thread: https://projecteuler.net/thread=6
	// One-liner formula: 1^2 + 2^2 + ... + n^2 = n * (n+1) * (2n+1) * 1/6
	def Solution2(): Int = {
		val naturalSumThru100: Int = (101 * 50)  // trivial, even for me. 1 + 100, 2 + 99, ... 50 + 51
		val squaredSumThru100: Int = 100 * (101) * (201) * 1/6
		(naturalSumThru100 * naturalSumThru100) - squaredSumThru100
	}
	
	def main(args: Array[String]) {
		time{ println(Solution1()) }
			// Elapsed time: 20.068624ms
			
		time{ println(Solution2()) }
			// Elapsed time: 0.078154ms
	}
}