// https://projecteuler.net/problem=3
import scala.collection.mutable.ListBuffer

object Euler3 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	def allPrimesToN(n: Int): ListBuffer[Int] = {
		// Ended up not really using this, but keeping it for record of thought process.
		var allPrimes: ListBuffer[Int] = ListBuffer(2)
	
		// Except 2, only odds can be prime, so just prepend 2 to final answer, save 50% already
		var initRange: List[Int] = List.range(3, n, 2)
		while(initRange.length >= 1) {
			val firstNumLeft = initRange.head
			allPrimes = allPrimes += firstNumLeft
			initRange = initRange.filterNot(initRange => initRange % firstNumLeft == 0)
		}
		
		allPrimes
	}
	
	def nDivisorsOfX(x: BigInt, n: Int, killNum: Int = 999999999): ListBuffer[Int] = {
		var divisors: ListBuffer[Int] = ListBuffer()
		var i = 2
		
		while(divisors.length < n & i < killNum & i < x) {
			if (x % i == 0) {
				divisors += i
			}
			i = i + 1
		}
	
		divisors
	}
	
	def greatestPrimeFactor(x: BigInt, n: Int, killNum: Int = 999999999): BigInt = {
		var divisors: ListBuffer[Int] = ListBuffer()
		var i = 2
		var potentialSolution: BigInt = 0
		var solution: BigInt = 0
		
		while(divisors.length < n & i < killNum & i < x & solution == 0) {
			if (x % i == 0) {
				divisors += i
				// "prime number check"
				potentialSolution = x / i
				val tempCount = nDivisorsOfX(potentialSolution, 1)
				//println(tempCount)
				if (tempCount.length == 0) {
					solution = potentialSolution	
				}
			}
			i = i + 1
		}
	
		solution
	}
	
	


	def main(args: Array[String]): Unit = {		
		val solutionNum: BigInt = 600851475143L
		
		println("Solution 1: my naive solution, finding factors (largest first), then checking if they are primes, in loop fashion.")
		time {println(greatestPrimeFactor(600851475143L, 30))}
			// Elapsed time: 18716.318851ms

		

	}

}