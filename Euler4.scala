// https://projecteuler.net/problem=4
/*
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
*/
import scala.collection.mutable.ListBuffer

object Euler4 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	// For the purposes of this problem, assume a 6 digit number, not a more general function
	def isPalindrome1(number: Int): Boolean = {
		var isPalindrome: Boolean = true
		if (number % 10 != math.floor(number / 100000)) {
			isPalindrome = false
		}
		else if (math.floor((number % 100) / 10) != math.floor((number % 100000) / 10000)) {
			isPalindrome = false
		}
		else if (math.floor((number % 1000) / 100) != math.floor((number % 10000) / 1000)) {
			isPalindrome = false
		}
		isPalindrome
	}
	
	def isPalindrome2(number: Int): Boolean = {
		val stringForward = number.toString
		val stringBackward = stringForward.reverse
		if (stringForward == stringBackward) {
			true
		}
		else {
			false
		}
	}
	
	def isPalindrome3(number: Int): Boolean = {
	
		// https://www.scala-lang.org/old/node/11647.html
		def split(n: Int) = {
			if (n == 0) { List(0) } 
			else { (Stream.iterate(n)(_/10)takeWhile(_!=0)map(_%10)toList) reverse }
		}
		
		val numForward = split(number)
		val numBackward = numForward.reverse
		
		if (numForward == numBackward) {
			true
		}
		else {
			false
		}
	}
	
	def isPalindrome4(number: Int): Boolean = {
		// thought of a faster way after comparing 1, 2, 3
		
		if (number % 1000 != ( 
				((number % 100000 / 1000) * 100)
				+ ((number % 100000 / 10000) * 10)
				+ (number % 1000000 / 100000)
				)) {
			false
		}
		else {
			true
		}
	}
	
	def baseAbaseBfromInt(rangeNum: Int): List[(Int, Int)] = {
		var baseAbaseB: ListBuffer[(Int, Int)] = new ListBuffer[(Int, Int)]
		var a: Int = if (rangeNum % 2 == 0) { rangeNum / 2 } else { (rangeNum / 2) + 1 }
		var b: Int = rangeNum / 2
		val aEnd = 99
		while (a <= aEnd) {
			baseAbaseB.append((a,b))
			a += 1
			b -= 1
		} 
		baseAbaseB.toList
	}
	
	def largestPalindrome(): Int = {
		// 1) Start with 198  (99_ + 99_) and work backwards by 1 to 189   (else you get below 900,000)
		val palindromeRange: Range = 198 to 189 by -1
		
		val flatRange: List[(Int, Int)] = palindromeRange.flatMap(x => baseAbaseBfromInt(x)).toList
		// returns List((99,99), (99,98) .... (97,92), (98,91), (99,90))
		
		// Start with (99,99), iterate through until hitting something that returns a palindrome, then go two more times to verify you have the largest palindrome.
		var i: Int = 0
		var j: Int = 3
		var largestPalindrome: Int = -1
		val stopPoint: Int = flatRange.length
		while (j > 0 & i < stopPoint) {
			val (a,b): (Int, Int) = flatRange(i)
			val temp = largestPalindromeSub(a,b)
			if (temp > largestPalindrome) {
				largestPalindrome = temp
			}
			if (largestPalindrome > -1) {
				j -= 1
			}
			i += 1
			
		}
		
		largestPalindrome
	}

	def largestPalindromeSub(baseA: Int, baseB: Int): Int = {
		// accepts 2-digit inputs, assumes they are first 2 numbers of a 3 digit number.
		// Hopefully we hit a palindrome before we go under 900,000, b/c we can vastly cut down on the permutations that we check if so...
			// 7x7, 9x1, and 3x3, iterate through and check if resulting multipliers are palindromes.
		
		val onesCombos = if (baseA == baseB) List((7,7),(9,1),(3,3)) else List((7,7),(1,9),(9,1),(3,3))		
		val palindromeList: List[Int] = onesCombos.map(onesCombo => largestPalindromeSub1(baseA, baseB, onesCombo))
		
		palindromeList.max
	}

	def largestPalindromeSub1(baseA: Int, baseB: Int, onesCombo: (Int, Int)): Int = {
		val (a,b) = onesCombo
		val numA = (baseA * 10) + a
		val numB = (baseB * 10) + b
		val aXb = numA * numB
		if (isPalindrome1(aXb)) {
			aXb
		}
		else {
			-1			// should use Option, but just need to make this work before I learn the intricacies of that.
		}
	}

	

	def palindromeBenchmark(): Unit = {
		// To benchmark your isPalindrome functions, run on every number from 100000 to 999999 and see time difference
		val palindromeRange: Range = Range(start=100000, end=1000000, step=1)
		
		println("isPalindrome1")
		time { 1 to 100 foreach { _ => palindromeRange.map(x => isPalindrome1(x)) } }
			// Elapsed time: 1399.698523ms
			// Winner!

		println("isPalindrome2")
		time { 1 to 100 foreach { _ => palindromeRange.map(x => isPalindrome2(x)) } }
			// Elapsed time: 10208.274812ms

		//println("isPalindrome3")
		//time { 1 to 100 foreach { _ => palindromeRange.map(x => isPalindrome3(x)) } }
			// Elapsed time: So bad I killed the program and commented this out after 90 seconds.
			
		println("isPalindrome4")
		time { 1 to 100 foreach { _ => palindromeRange.map(x => isPalindrome4(x)) } }
			// Elapsed time: 1508.414912ms
	}
	

	def main(args: Array[String]): Unit = {
		
		time{ println(largestPalindrome()) }
			// Elapsed time: 10.186207ms
			// maybe should have had a more mathematically rigorous proof that largest palindrome would be over 900,000 to get full "credit" for such a fast answer.
	}

}