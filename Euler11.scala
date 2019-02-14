// https://projecteuler.net/problem=11
/*
In the 20×20 grid below, four numbers along a diagonal line have been marked in red.

08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
...
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48

The product of these numbers is 26 × 63 × 78 × 14 = 1788696.

What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
*/
// safe to use Int on all product functions. 99^4 = 96059601

object Euler11 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}
	
	def horizontalProduct(inputVector: Vector[Vector[Int]], x: Int, y: Int): Int = {
		val y1: Int = y
		val y2: Int = y + 1
		val y3: Int = y + 2
		val y4: Int = y + 3
		if (y > 16) 0 else inputVector(x)(y1) * inputVector(x)(y2) * inputVector(x)(y3) * inputVector(x)(y4)
	}
	
	def verticalProduct(inputVector: Vector[Vector[Int]], x: Int, y: Int): Int = {
		val x1: Int = x
		val x2: Int = x + 1
		val x3: Int = x + 2
		val x4: Int = x + 3
		if (x > 16) 0 else inputVector(x1)(y) * inputVector(x2)(y) * inputVector(x3)(y) * inputVector(x4)(y)
	}
	
	def diagDownRightProduct(inputVector: Vector[Vector[Int]], x: Int, y: Int): Int = {
		val x1: Int = x
		val x2: Int = x + 1
		val x3: Int = x + 2
		val x4: Int = x + 3
		val y1: Int = y
		val y2: Int = y + 1
		val y3: Int = y + 2
		val y4: Int = y + 3
		if (x > 16 | y > 16) 0 else inputVector(x1)(y1) * inputVector(x2)(y2) * inputVector(x3)(y3) * inputVector(x4)(y4)
	}
	
	def diagDownLeftProduct(inputVector: Vector[Vector[Int]], x: Int, y: Int): Int = {
		val x1: Int = x
		val x2: Int = x - 1
		val x3: Int = x - 2
		val x4: Int = x - 3
		val y1: Int = y
		val y2: Int = y + 1
		val y3: Int = y + 2
		val y4: Int = y + 3
		if (x < 4 | y > 16) 0 else inputVector(x1)(y1) * inputVector(x2)(y2) * inputVector(x3)(y3) * inputVector(x4)(y4)
	}
	
	def fourProducts(inputVector: Vector[Vector[Int]], x: Int, y: Int): Vector[Int] = {
		val h: Int = horizontalProduct(inputVector, x, y)
		val v: Int = verticalProduct(inputVector, x, y)
		val dr: Int = diagDownRightProduct(inputVector, x, y)
		val dl: Int = diagDownLeftProduct(inputVector, x, y)
		Vector(h, v, dr, dl)
	}

	def solution1(): Int = {
		val inputValuesString: String = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
		val inputVector1D: Vector[Int] = inputValuesString.split(" ").map(x => x.toInt).toVector
		val inputVector: Vector[Vector[Int]] = inputVector1D.sliding(20,20).toVector
		val coords: List[(Int, Int)] = (0 to 399).map(x => (x / 20, x % 20)).toList
		coords.flatMap({case (x,y) => fourProducts(inputVector, x, y)}).max
	}

	def main(args: Array[String]) {

		time{ println(solution1()) }
			// Elapsed time: 41.444905ms
			// Good news: 100% functional solution
			// Could optimize by reusing calculations on the x and y axis and being smarter about what will be 0 in advance.
	}

}

// https://alvinalexander.com/scala/how-to-split-sequences-subsets-groupby-partition-scala-cookbook
	// useful for "sliding" method, to break 400 element Vector into 20 x 20 object