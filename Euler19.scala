// https://projecteuler.net/problem=19
/*
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
*/

// Trivially, Jan 1 1901 was a Tuesday. Normal years have calendar skip forward by 1 day, and 1900 was not a leap year.
// Just going to write something tail-recursive to bang this out.

import scala.annotation.tailrec

object Euler19 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}

	def daysInMonth(thisMonth: Int, thisYear: Int): Int = {
		if (List(1,3,5,7,8,10,12).contains(thisMonth)) 31
		else if (thisMonth == 2 & (thisYear % 4 != 0 | (thisYear % 100 == 0 & thisYear % 400 != 0))) 28
		else if (thisMonth == 2 & !(thisYear % 4 != 0 | (thisYear % 100 == 0 & thisYear % 400 != 0))) 29
		else 30
	}
	
	def nextMonthAndYear(thisMonth: Int, thisYear: Int): (Int, Int) = {
		val nextMonth: Int = if (thisMonth < 12) thisMonth + 1 else 1
		val nextYear: Int = if (thisMonth < 12) thisYear else thisYear + 1
		(nextMonth, nextYear)
	}

	@tailrec // confirms this is tail-recursive
	def sundayCount(startDOW: Int, thisMonth: Int, thisYear: Int, runningCount: Int = 0, monthsLeft: Int = 1200): Int = {
		// doesn't check final month below, so monthsLeft = 12 means check start month plus 11 more months, etc.
		monthsLeft match {
			case 0 => runningCount
			case _ => {
				val newRunningCount: Int = if (startDOW == 0) runningCount + 1 else runningCount
				val nextStartDOW: Int = (startDOW + daysInMonth(thisMonth, thisYear)) % 7
				val (nextMonth, nextMonthYear) = nextMonthAndYear(thisMonth, thisYear)
				sundayCount(nextStartDOW, nextMonth, nextMonthYear, newRunningCount, monthsLeft - 1)
			}
		}

	}
	
	def solution1(): Int = {
		sundayCount(2, 1, 1901)
	}
	

	def main(args: Array[String]) {
	
		time{ println(solution1()) }
		// Elapsed time: 9.382867ms
	}

}

