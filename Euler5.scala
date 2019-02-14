//
/*
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
*/
object Euler5 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	def brutalBruteForce(): BigInt = {
		val checkNums: List[Int] = List(19,18,17,16,15,14,13,12,11)
		var i: BigInt = 20
		while (checkNums.map(x => i % x == 0).count(_ == true) < 9) {
			i += 20			
		}
		i
	}

	def solution1(): BigInt = {
		val checkNums: List[Int] = List(18,17,16,15,14,13,12,11)
		var i: BigInt = 380
		while (checkNums.map(x => i % x == 0).count(_ == true) < 8) {
			i += 380
		}			
		i
	}
	
	def solution2(): Int = {
		val checkNums: List[Int] = List(18,17,16,15,14,13,12,11)
		var i: Int = 380
		while (checkNums.map(x => i % x == 0).count(_ == true) < 8) {
			i += 380
		}			
		i
	}
	
	def main(args: Array[String]) {

		time{ println(brutalBruteForce()) }	
			// Elapsed time: 15982.468109ms
			
		time{ println(solution1()) }
			// Elapsed time: 967.602783ms
			
		time{ println(solution2()) }
			// Elapsed time: 155.679166ms
	
	}
}