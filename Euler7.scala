// https://projecteuler.net/problem=7
import scala.collection.mutable.ListBuffer

object Euler7 {
	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}

	def allPrimesToNBig(n: BigInt): ListBuffer[BigInt] = {
		// All prime numbers to a given BigInt
		
		var allPrimes: ListBuffer[BigInt] = ListBuffer(2L)
	
		// Except 2, only odds can be prime, so just prepend 2 to final answer, save 50% already
		var initRange: List[BigInt] = List.range(3, n, 2)
		while(initRange.length >= 1) {
			val firstNumLeft = initRange.head
			allPrimes = allPrimes += firstNumLeft
			initRange = initRange.filterNot(initRange => initRange % firstNumLeft == 0)
		}
		
		allPrimes
	}
	
	def allPrimesToN(n: Int): ListBuffer[Int] = {
		// All prime numbers to a given Int
	
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
	
	def allPrimesToN_v1(n: Int): ListBuffer[Int] = {
		// All prime numbers to a given Int
	
		// Ended up not really using this, but keeping it for record of thought process.
		val allPrimes: ListBuffer[Int] = ListBuffer(2)
	
		// Except 2, only odds can be prime, so just prepend 2 to final answer, save 50% already
		var initRange: List[Int] = List.range(3, n, 2)
		while(initRange.length >= 1) {
			val firstNumLeft = initRange.head
			allPrimes.append(firstNumLeft)
			initRange = initRange.filterNot(initRange => initRange % firstNumLeft == 0)
		}
		
		allPrimes
	}

	def isXPrime(x: Int): Boolean = {
		if (List(2,3).contains(x)) {
			true
		}
		else if (x % 2 == 0) {
			false
		}
		// Loop from 3 to sqrt(x) on odd numbers.
		else {		
			val endVal: Int = math.ceil(math.sqrt(x)).toInt
			var i: Int = 3
			var prime: Boolean = true
			while (prime == true & i <= endVal) {
				if (x % i == 0) {
					prime = false
				}
				i = i + 2
			}
			prime
		}
	}

	def xthPrime(x: Int): Int = {
		if (x == 1) {
			2
		}
		
		var counter: Int = 1
		var i: Int = 3
		while (counter < x) {			
			if (isXPrime(i)) {
				counter = counter + 1
			}
			if (counter < x) {
				i = i + 2
			}
		}
		
		i
	}
	
	def isXPrimeV1(x: Int): Boolean = {
		if (List(2,3,5).contains(x)) {
			true
		}
		else if (x % 2 == 0) {
			false		// no even number is a prime
		}
		else if (x % 6 == 3) {
			false		// all primes after 3 are in form x % 6 == 1 or 5
		}
		// Loop from 3 to sqrt(x) on odd numbers.
		else {		
			val endVal: Int = math.ceil(math.sqrt(x)).toInt
			var i: Int = 3
			var prime: Boolean = true
			while (prime == true & i <= endVal) {
				if (x % i == 0) {
					prime = false
				}
				i = i + 2
			}
			prime
		}
	}

	def xthPrimeV1(x: Int): Int = {
		if (x == 1) {
			2
		}
		if (x == 2) {
			3
		}
		
		var counter: Int = 2
		var solution: Int = 3
		var i: Int = 6
		
		// skip by 6, check (i-1) and (i+1)
			// a little help from https://www.xarg.org/puzzle/project-euler/problem-7/ pointing out 6k + 1 property
		while (counter < x) {			
			if (isXPrimeV1(i - 1)) {
				counter = counter + 1
				if (counter == x) {
					solution = i - 1
				}
			}
			if (isXPrimeV1(i + 1) & counter < x) {
				counter = counter + 1
				if (counter == x) {
					solution = i - 1
				}
			}
			if (counter < x) {
				i = i + 6
			}
		}
		
		solution
	}
	
	
	
	def main(args: Array[String]): Unit = {				
		println("Solution 1: Use all primes to N function, manually iterate until length is 10,001. Then take last element")
		time {println(allPrimesToNBig(104750L).last)}
			// Elapsed time: 11649.774778ms
			// But about 5 minutes to manually find a correct N to iterate to.

		println("Solution 2: Use Int fuction since solution comfortably fits in Int data type.")
		time {println(allPrimesToN(104750).last)}
			// Elapsed time: 963.597641ms
		
		println("Solution 3: Use Int function. Optimization on ListBuffer append method.")
		time {println(allPrimesToN_v1(104750).last)}
			// Elapsed time: 976.79323ms
		
		println("Solution 4: Now iterate through a stream of odd integers, optimally check if each one is prime, and stop exactly on 10,001st.")
		time{println(xthPrime(10001))}
			// Elapsed time: 22.830978ms
		
		println("Solution 5: Now iterate by 6, check +- 1 for prime. Slightly faster isXPrime func as well. Stop exactly on 10,001st.")
		time{println(xthPrimeV1(10001))}
			// Elapsed time: 21.675763ms
		
	}

}