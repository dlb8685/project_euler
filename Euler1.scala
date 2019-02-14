// https://projecteuler.net/problem=1

object Euler1 {
	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block    // call-by-name
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}

	def main(args: Array[String]): Unit = {
		println("Solution 1, brute force from 1 to 999, sum")
		time {

			val x: Range = new Range(start=1, end=1000, step=1)
			println(x.filter(x=> x % 3 == 0 || x % 5 == 0).foldLeft(0)(_ + _))
		}
			// Elapsed time: ~6.182015ms
		
		println("Solution 2, Brute force 3's, Brute force 5's, subtract 15's")
		time {
			val a: Range = new Range(start=3, end=1000, step=3)
			val b: Range = new Range(start=5, end=1000, step=5)
			val c: Range = new Range(start=15, end=1000, step=15)
			println(a.foldLeft(0)(_ + _) + b.foldLeft(0)(_ + _) - c.foldLeft(0)(_ + _))
		}
			// Elapsed time: ~2.387886ms

		println("Known solution: sum of different segments using 3 + 999, 6 + 996, etc. method")
		time {
			val a: Int = ((1002 * 166) + 501)	// 3 + 999, 6 + 996, ..., 498 + 504 (+ 501), 498 / 3 = 166
			val b: Int = ((1000 * 99) + 500)  // 5 + 995, 10 + 990, ..., 495 + 505 (+ 500), 495 / 5 = 99
			val c: Int = (1005 * 33)  // 15 + 990, 30 + 975, ..., 495 + 510. 495 / 15 = 33 
			println(a + b - c)
		}
			// Elapsed time: ~0.072936ms
	}
}