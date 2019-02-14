// https://projecteuler.net/problem=9
/*
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
*/

object Euler9 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	// is there an elegant way to find pythagorean triplets?
	// 1) If (a,b,c) is a triple, so is (ka, kb, kc)
	// 2) Euclid's formula is the common way to find these (https://en.wikipedia.org/wiki/Pythagorean_triple)
		// a=m^{2}-n^{2},\ \,b=2mn,\ \,c=m^{2}+n^{2}  for m > n > 0
		// which makes the solution nearly algebraic: (m^2 - n^2) + (2mn) + (m^2 + n^2) = 1000, m > n
	// This below just looks ugly, but I think it works. Will think about a more elegant Solution2
	def solution1(targetNum: Int): Int = {
		var i: Int = 3
		var solution: Int = -1
		var m: Int = i - 1
		var n: Int = 1
		while (solution != targetNum) {
			m = i - 1
			n = 1
			var abc = ((m * m)-(n * n)) + (2 * m * n) + ((m * m) + (n * n))
			solution = if (abc == targetNum) abc else -1
			while (m > n & solution != targetNum) {
				m -= 1
				n += 1
				abc = if (m > n) ((m * m)-(n * n)) + (2 * m * n) + ((m * m) + (n * n)) else -1
				solution = if (abc == targetNum) abc else -1
			}
			i += 1
		}
		((m * m)-(n * n)) * (2 * m * n) * ((m * m) + (n * n))
	}
	
	// really need to determine Scala code that can create an Iterator((2,1), (3,1), (4,1), (3,2), (5,1), (4,2), ...)
	// For any given tuples, it's trivial to run through a function that returns a, b, c, and sum a+b+c
	def findABC(inputTuple: Tuple2[Int, Int]): Tuple4[Int, Int, Int, Int] = {
		val (m,n) = inputTuple
		val a: Int = (m * m) - (n * n)
		val b: Int = 2 * m * n
		val c: Int = (m * m) + (n * n)
		val abc: Int = a + b + c
		(a,b,c,abc)
	}

	def findABCForM(m: Int): Seq[Tuple4[Int, Int, Int, Int]] = {
		val nRange: Range = Range(1, m)
		nRange.map(n => findABC((m, n)))
	}

	
	def solution2(targetNum: Int): Int = {
		// a is always less than m^2, and still needs to be less than 1/2 the targetNum
		val mRangeMax = math.floor(math.sqrt(targetNum / 2)).toInt
		val solutionRange: Range = Range(2, mRangeMax)
		//  Iterate "m" through 2,3,4,5... iterate through every "n" to "m", calc A,B,C, stop when they add to targetNum.
		val (a,b,c,d) = solutionRange.flatMap(x => findABCForM(x)).find(_._4 == targetNum).get
		a * b * c
	}
	
	
	
	def main(args: Array[String]) {
	
		// println(	time{solution1(12)} )  		// 12 -> (3,4,5) should return 60
		// println(	time{solution1(30)} )  		// 30 -> (5,12,13) should return 780
		// println(	time{solution1(126)} )  	// 126 -> (28,45,53) should return 66780
		
		println( time{solution1(1000)} )
			// Elapsed time: 0.021122ms
			// Alright, so it looks a little (a lot) imperative, but runs pretty fast.

		println( time{solution2(1000)} )
			// Elapsed time: 6.257328ms
			// Much easier to read, functional, and ~300x slower than imperative.
			// Either there's a small performance trade-off in cases like this, or I need to get better at functional.
	}


}

// https://stackoverflow.com/questions/33514027/scala-equivalent-for-javas-stream-findfirst
// https://stackoverflow.com/questions/6799648/in-scala-what-does-view-do
