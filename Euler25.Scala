// https://projecteuler.net/problem=25
/*
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
...
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
*/

// Note: For any problem after first 20, I will explain why I specifically chose this problem.
// For this one, combination of cache and recursion. This came up somewhat in 14, but in a very ugly way.
// So my goal here is to write this two ways, and I predict solution2 will be 10 times faster than solution1


object Euler25 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}

	/*
	def fibonacciSlow(n: Int): Int = {
			// performance of this method *rapidly* degrades as you set n higher without a cache. 
			// Even running fibonacci(35) takes noticable time. 
			// fibonacci(45) takes several seconds and still isn't close to what we want in terms of digit count.
			// In fact, this would never make it to 1,000 digits unless you wanted to keep your computer locked in a closet for a month.
		if (List(1,2).contains(n)) {
			1
		}
		else {
			fibonacci(n-2) + fibonacci(n-1)
		}
	}
	*/


	def fibonacci(stopVal: BigInt, n: Int = 3, currVal: BigInt = 2, lastVal: BigInt = 1): Int = {
		// function that iterates Fibonacci sequence until result is greater than "stop", and returns index
			// for this problem, assumes "stop" will be a very large number and not something stupid like 0 or 1.
		// Side note: this has nothing to do with caching, so maybe I chose this problem for the wrong reasons.
		if (currVal > stopVal) {
			n
		}
		else {
			fibonacci(stopVal, n + 1, currVal + lastVal, currVal)
		}
	}
	

	def solution1(): Int = {
		val bd: BigDecimal = BigDecimal("1.00E+999")
		val stopVal: BigInt = bd.toBigInt();
		fibonacci(stopVal)
	}
	
	def main(args: Array[String]) {
		time{ println(solution1()) }
		// Elapsed time: 13.655025ms
		// Faster to find an absurdly large fibonacci value using this, than 20th or 30th value with fibonacciSlow
	}

}

