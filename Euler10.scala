// https://projecteuler.net/problem=10
/*
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
*/

object Euler10 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}

	def isXPrime(x: Int): Boolean = {
		if (x == 2 | x == 3 | x == 5) {
			true
		}
		else if (x % 2 == 0 | x % 10 == 5 | x % 6 == 3 | x == 4 | x == 6) {
			// no even number (except 2) is a prime
			// no number that ends with 5 (except 5) is a prime
			// all primes after 3 are in form x % 6 == 1 or 5
			false
		}
		// Loop from 7 to sqrt(x) on odd numbers.
		else {		
			val endVal: Int = math.ceil(math.sqrt(x)).toInt
			val scanRange: Range = Range(7, (endVal + 1), 2)
			val divisor: Option[Int] = scanRange.find(n => (x % n == 0) == true)
			if (divisor.getOrElse(-1) == -1) {
				true		// i.e. no divisors for input x in this path
			}
			else {
				false
			}
		}
	}
	
	def solution1(endVal: Int): Long = {
		val scanRange: Range = Range(3, endVal + 1, 2)
		scanRange.filter(x => isXPrime(x)).foldLeft(2L)(_ + _)
	}
	
	def solution2(): Long = {
		// taking from https://pavelfatin.com/scala-for-project-euler/
		// I don't understand at all, but it's supposedly faster. Gives me things to google.
		lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter(i => ps.takeWhile(j => j * j <= i).forall(i % _ > 0))
		ps.view.takeWhile(_ < 2000000).foldLeft(0L)(_ + _)
	}


	def main(args: Array[String]) {
		println(time{ solution1(2000000) })
			// Elapsed time: 1078.158471ms
			// interested to see how this can be solved in fractions of a second
			
		println(time{ solution2() })
			// Elapsed time: 1765.934998ms
			// is actually a little slower. I still want to understand the syntax and what it's doing.
	}


}