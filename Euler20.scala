// https://projecteuler.net/problem=19
/*
n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
*/

object Euler20 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	def factorial(n: Int, runningTot: BigInt = 1): BigInt =  {
		n match {
			case 1 => runningTot
			case _ => factorial(n - 1, n * runningTot)
		}
	}
	
	def solution1(): Int = {
		val factorial100 = factorial(99)	// 100 will just add zeroes to an already huge number, not affect the sum.
			// just for practice, print the largest digit. Use reduceLeft and manual comparison, vs. built in method.
		// println(factorial100.toString.toArray.map(x => x.asDigit).reduceLeft((x,y) => if (x > y) x else y))
			// use foldLeft just to start getting some practice, and stop relying on sum always.
		factorial100.toString.toArray.map(x => x.asDigit).foldLeft(0)(_ + _)
	}


	def main(args: Array[String]) {
	
		time{ println(solution1()) }
		// Elapsed time: 11.504258ms
	}
}


// Note: After this one, going to scout forward for 15-20 harder problems. These are starting to feel much easier.
// Already skipped problem 17 because it was a joke how easy, yet slightly tedious it would be to finish.
// And especially look for opportunities to create generic, partially applied, curried, etc. functions.