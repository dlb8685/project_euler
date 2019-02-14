// https://projecteuler.net/problem=2

object Euler2 {
	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block    // call-by-name
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}

	def main(args: Array[String]): Unit = {
		println("Solution 1, terms of Fibonacci to 4 million by brute force, then sum. The old-fashioned array and append way.")
		time {
			var l: List[Int] = List(1,2)
			var nextFib: Int = 3   // Scala people flip out if you use mutable variables!
			var evenSum: Int = 2
			while (nextFib < 4000000) {
				l = l ::: List(nextFib)
				nextFib = l(l.length - 2) + l.last
				if (l.last % 2 == 0) {
					evenSum = evenSum + l.last
				}
			}
			println(evenSum)  
		}
			// Elapsed time: 3.313519ms

		
		println("Solution 2, you have to figure out something recursive now.")
		time {
			// This still uses mutable counter, probably would make Mr. Odersky pull his hair out.
				// I think if this was going higher in iterations there would be a stackOverflow for sure....		
			def fibRecursive(n: Int): Int = {
				if (n == 1) {
					1
				}
				else if (n == 2) {
					2
				}
				else {
					fibRecursive(n - 1) + fibRecursive(n - 2)

				}
			}
			
			var latestTot: Int = 0
			var evenSum: Int = 0
			var i: Int = 1
			while(latestTot < 4000000) {
				latestTot = fibRecursive(i)
				if (latestTot % 2 == 0 & latestTot < 4000000) {
					evenSum = evenSum + latestTot
				}
				i = i + 1
			}
			println(evenSum) 
			// This is right, but almost 10x slower than dumbed down Solution 1, and not scalabe as 4 million increases to 40 million, etc.
		}
			// Elapsed time: 42.871485ms
		
		
		println("Solution 3, think about it more, is there some way to have elements like 'last'-'this'-'next'?")
		time {
			var lastInt: Int = 1
			var thisInt: Int = 2
			var nextInt: Int = 3
			var evenSum: Int = 2
			while(nextInt < 4000000) {
				lastInt = thisInt
				thisInt = nextInt
				nextInt = lastInt + thisInt
				if (thisInt % 2 == 0) {
					evenSum = evenSum + thisInt
				}
			}
			println(evenSum)
			// This is almost 100x faster than solution 1, albeit with mutable vars and while loop. Nothing functional or recursive.
		}
			// Elapsed time: 0.085511ms
		
		println("Solution 4, only every third number is even, exploit that to reduce number of loops")
		time {
			var fibInt1: Int = 1
			var fibInt2: Int = 2
			var fibInt3: Int = 3
			var evenSum: Int = 2
			while(fibInt2 < 4000000) {
				fibInt1 = fibInt2 + fibInt3
				fibInt2 = fibInt3 + fibInt1
				fibInt3 = fibInt1 + fibInt2
				if (fibInt2 < 4000000) {
					evenSum = evenSum + fibInt2
				}
			}
			println(evenSum)
			// See https://stackoverflow.com/questions/24483199/more-efficient-solution-project-euler-2-even-fibonacci-numbers?rq=1
			// Doesn't say anything about this being "wrong" for Scala...
			// But pretty much same as Solution 3
		}
			// Elapsed time: 0.081362ms
	}
}