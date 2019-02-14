// https://projecteuler.net/problem=17
/*
If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
*/

// On a personal note, this problem seems easy to solve, yet very tedious. I would skip it except it's the only one of the first 20 left to do.


object Euler17 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}

	def lettersInOnes(onesDigit: Int): Int = onesDigit match {
		case 0 => 0		// "two hundred and forty"
		case 1 => 3 	// "one"
		case 2 => 3		// "two"
		case 3 => 5		// "three"
		case 4 => 4		// "four"
		case 5 => 4		// "five"
		case 6 => 3		// "six"
		case 7 => 5		// "seven"
		case 8 => 5		// "eight"
		case 9 => 4		// "nine"
	}


	def lettersInTens(tensDigit: Int): Int = tensDigit match {
		case 0 => 0		// "one hundred and nine"
		case 2 => 6		// "twenty"
		case 3 => 6		// "thirty"
		case 4 => 5		// "forty"
		case 5 => 5		// "fifty"
		case 6 => 5		// "sixty"
		case 7 => 7		// "seventy"
		case 8 => 6		// "eighty"
		case 9 => 6		// "ninety"
	}

	def lettersInTeens(teensNum: Int): Int = teensNum match {
		case 10 => 3	// "ten"
		case 11 => 6	// "eleven"
		case 12 => 6	// "twelve"
		case 13 => 8	// "thirteen"
		case 14 => 8	// "fourteen"
		case 15 => 7	// "fifteen"
		case 16 => 7	// "sixteen"
		case 17 => 9	// "seventeen"
		case 18 => 8	// "eighteen"
		case 19 => 8	// "nineteen"
	}

	def lettersInHundreds(hundredsNum: Int): Int = hundredsNum match {
		case 0 => 0			// "sixty four"
		case 1 => 13		// "onehundredand"
		case 2 => 13		// "twohundredand"
		case 3 => 15		// "threehundredand"
		case 4 => 14		// "fourhundredand"
		case 5 => 14		// "fivehundredand"
		case 6 => 13		// "sixhundredand"
		case 7 => 15		// "sevenhundredand"
		case 8 => 15		// "eighthundredand"
		case 9 => 14		// "ninehundredand"
	}

	def lettersInNumber(number: Int): Int = {
		if (number < 10) {
			lettersInOnes(number)
		}
		else if (number >= 10 & number < 20) {
			lettersInTeens(number)
		}
		else if (number >= 20 & number < 100) {
			lettersInTens(number / 10) + lettersInOnes(number % 10)
		}
		else if (number % 100 == 0 & number < 1000) {
			// drop "and" from exactly 100, 200, 300, etc.
			lettersInHundreds(number / 100) - 3
		}
		else if (number >= 100 & (number % 100) / 10 != 1 & number < 1000) {
			lettersInHundreds(number / 100) + lettersInTens((number % 100) / 10) + lettersInOnes(number % 10)
		}
		else if (number >= 100 & (number % 100) / 10 == 1 & number < 1000) {
			lettersInHundreds(number / 100) + lettersInTeens(number % 100)
		}
		else if (number == 1000) {
			11		// "onethousand"
		}
		else {
			0
		}
	}
	
	def solution1(): Int = {
		val numRange: Range = Range(1, 1001)
		numRange.map(x => lettersInNumber(x)).foldLeft(0)(_ + _)
	}


	def main(args: Array[String]) {
	
		time { println(solution1()) }
	}
}

// This was indeed my least favorite problem so far, and it's not even close. Just saying...