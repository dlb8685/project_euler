// https://projecteuler.net/problem=15
/*
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
*/

// If I could remember the combinatorics formulas, this is a binomial problem, with number of combinations to get exactly 20 successes out of 40.
// Literally could be a one line formula typed out by hand if I remembered it, but let me think on it. Involves factorials.

// .... But think about it, their toy example is 2 successes out of 4. 6 combinations. Total possible combinations, 2^4
	// 1100, 1010, 1001, 0110, 0101, 0011    (3 X 2 X 1, but where does 3! come from?)
	// (4*3*2*1) / (2*1)(2*1) = (4*3) / (2*1) = 6, I remember now. 5 minutes and the lightbulb went off.  n! / a!b!
// .... 3 out of 5, 10 combinations. Total possible combinations, 2^5
	// 11100, 11010, 11001, 10110, 10101, 10011, 01110, 01101, 01011, 00111
	// (5*4*3*2*1) / (3*2*1)(2*1) = (5*4) / (2*1)
	
object Euler15 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	def solution1(): BigInt = {
		(BigInt(40) * 39 * 38 * 37 * 36 * 35 * 34 * 33 * 32 * 31 * 30 * 29 * 28 * 27 * 26 * 25 * 24 * 23 * 22 * 21) / 
		(BigInt(20) * 19 * 18 * 17 * 16 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1)
	}

	def main(args: Array[String]) {
		time{ println(solution1()) }
		// Elapsed time: 2.220617ms
		// Okay, so I didn't learn a lick of programming from this exercise, but I did refresh my memory on statistics 101.
	
	}
}