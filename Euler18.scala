// https://projecteuler.net/problem=18
/*
By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

				75
				...
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. 
However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)
*/

import scala.annotation.tailrec

object Euler18 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	
	@tailrec	// confirms this is tail-recursive
	def rowMerge(inputArray: Array[Array[Int]]): Array[Array[Int]] = {
		// take bottom two rows, and place largest possible sum for each path in 2nd to bottom, drop bottom.
		// repeat process until you have the final value
		val arrayLength: Int = inputArray.length
		arrayLength match {
			case 1 => inputArray
			case _ => {
				val finalRow = inputArray(arrayLength - 1)
				val penultimateRow = inputArray(arrayLength - 2)
				val mergeRange: Range = Range(0, penultimateRow.length)
					// for your final merge to work properly, you need Array[Array.... even though this is only one row.
				val newFinalRow = Array(mergeRange.map(x => penultimateRow(x) + math.max(finalRow(x), finalRow(x+1))).toArray)
				val newInputArray = inputArray.slice(0, arrayLength - 2) ++ newFinalRow
				rowMerge(newInputArray)
			}
		}
	}
	
	def solution1(): Int = {
		val inputArray: Array[Array[Int]] = getInputArray()
		val finalArray: Array[Array[Int]] = rowMerge(inputArray)
		finalArray(0)(0)
	}
	
	def main(args: Array[String]) {
	
		time{ println(solution1()) }
		// Elapsed time: 8.590423ms
	}
	
	


	def getInputArray(): Array[Array[Int]] = {
		Array(
			Array(75),
			Array(95,64),
			Array(17,47,82),
			Array(18,35,87,10),
			Array(20,4,82,47,65),
			Array(19,1,23,75,3,34),
			Array(88,2,77,73,7,63,67),
			Array(99,65,4,28,6,16,70,92),
			Array(41,41,26,56,83,40,80,70,33),
			Array(41,48,72,33,47,32,37,16,94,29),
			Array(53,71,44,65,25,43,91,52,97,51,14),
			Array(70,11,33,28,77,73,17,78,39,68,17,57),
			Array(91,71,52,38,17,14,91,43,58,50,27,29,48),
			Array(63,66,4,68,89,53,67,30,73,16,69,87,40,31),
			Array(4,62,98,27,23,9,70,98,73,93,38,53,60,4,23)
			)
	}

}