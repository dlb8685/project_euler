// https://projecteuler.net/problem=14
/*
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*/

/*
First, write a recursive function that works, then figure out how to cache in Scala because certain numbers will come up constantly.
*/

object Euler14 {

	// timer function at the top of every solution: http://biercoff.com/easily-measuring-code-execution-time-in-scala/
	def time[R](block: => R): R = {
		val t0 = System.nanoTime()
		val result = block
		val t1 = System.nanoTime()
		println("Elapsed time: " + ((t1 - t0) / 1000000.0) + "ms")
		result
	}


	def collatzSeq(currentNum: Int, moveCount: Int = 0): Int = {
		// generally set moveCount to 0 when invoking, and the rest is recursion.
		if (currentNum == 1) {
			moveCount
		}
		else {
			val nextNum: Int = if (currentNum % 2 == 0) currentNum / 2 else (currentNum * 3) + 1
			collatzSeq(nextNum, moveCount + 1)
		}
	}
	
	def collatzSeqWPrint(currentNum: Int, moveCount: Int = 0): Int = {
		// generally set moveCount to 0 when invoking, and the rest is recursion.
		if (currentNum == 1) {
			moveCount
		}
		else {
			val nextNum: Int = if (currentNum % 2 == 0) currentNum / 2 else (currentNum * 3) + 1
			println(nextNum)
			collatzSeqWPrint(nextNum, moveCount + 1)
		}
	}
	
	
	def collatzSeqMemoized: Int => Int = {
		// Big thanks to this link: https://medium.com/musings-on-functional-programming/scala-optimizing-expensive-functions-with-memoization-c05b781ae826

		// this verison is not tail recursive, but it is cache-able. So it shouldn't stack up a gazallion times b/c almost always hitting something cached within 1-3 iterations.
		def collatzSeq(currentNum: Int): Int = {
				// generally set moveCount to 0 when invoking, and the rest is recursion.
				if (currentNum == 1) {
					1
				}
				else {
					val nextNum: Int = if (currentNum % 2 == 0) currentNum / 2 else (currentNum * 3) + 1
					1 + collatzSeq(nextNum)
				}
			}

		val cache = collection.mutable.Map.empty[Int, Int]

		currentNum => cache.getOrElse(currentNum, {
			cache update(currentNum, collatzSeq(currentNum)) 
			cache(currentNum)
			})
	}
	
	
	def solution1(): Int = {
		// just iterates through, no caching or time-saving measures. Establish a benchmark vs. cached solution in terms of performance.
		val seqToMillion: Range = Range(1, 1000000)
		// see maxBy (https://stackoverflow.com/questions/15769366/how-to-find-max-in-a-list-of-tuples)
		seqToMillion.map(x => (x,collatzSeq(x))).maxBy(_._2)._1
		
	}
	
	def solution2(): Int = {
		// use memoized version of collatzSeq, since some numbers (like 2,4,etc.) repeat on probably every one of the million iterations.
		val seqToMillion: Range = Range(1, 113385)
			// initiates cached version of collatz function
		val collatzSeq = collatzSeqMemoized
		seqToMillion.foreach(x => (x,collatzSeq(x)))	// initializes cached values on everything
		seqToMillion.map(x => (x,collatzSeq(x))).maxBy(_._2)._2
	}
	
	def solution3(): Int = {
		// Trying to figure out a way to gaurantee that collatzSeq(85039) runs and is cached, and cache is used for collatzSeq(113385), for instance.
		// Last resort here is to imperatively iterate through, to avoid any weirdness from the map function.
		// At the very least, I'm learning something here...

		val collatzSeq = collatzSeqMemoized
		var largestChain: Int = 1
		var largestChainLength: Int = 1
		var i: Int = 1
		while (i < 1000000) {
			var collatzI: Int = collatzSeq(i)
			if (collatzI > largestChainLength) {
				largestChainLength = collatzI
				largestChain = i
				}
			i += 1
		}
		
		largestChain
	}
	
	def solution4(): Int = {
	
		def collatzSeqCache(currentNum: Int, cache: collection.mutable.Map[Int, Int]): Int = {
				// generally set moveCount to 0 when invoking, and the rest is recursion.
				if (currentNum == 1) {
					1
				}
				else {
					// Can actually go far enough without hitting the cache to stack overflow.
					// Loop through anything higher than currentNum with counter, and then once nextNum is *lower*, try cache
					var count: Int = 1
					var nextNum: Long = if (currentNum % 2 == 0) currentNum / 2 else (currentNum * 3) + 1
					while (nextNum > currentNum) {
						nextNum = if (nextNum % 2 == 0) nextNum / 2 else (nextNum * 3) + 1
						count += 1
					}
					var finalNum: Int = nextNum.toInt	// Int is safe. nextNum should now be < currentNum which can never be more than 1 million.
					count + cache.getOrElse(finalNum, collatzSeqCache(finalNum, cache))   
				}
			}
		
		// keep a running cache, add after you call collatz, pass into every invocation
		val cache = collection.mutable.Map.empty[Int, Int]
		
		
		var largestChain: Int = 1
		var largestChainLength: Int = 1
		var i: Int = 1
		while (i < 1000000) {
			var collatzI: Int = collatzSeqCache(i, cache)
			cache update(i, collatzI)
			if (collatzI > largestChainLength) {
				largestChainLength = collatzI
				largestChain = i
				}
			i += 1
		}
		
		//println(cache)
		largestChain
	}
	
	// maybe try to understand this tomorrow: http://www.programmingopiethehokie.com/2014/04/exploring-memoization.html
	
	

	def main(args: Array[String]) {
	
		//time{ println(solution1()) }
		// This runs for well over 2-3 minutes, I just killed it.
		
		//time{ println(solution2()) }
		// stack overflow at 113385
		
		// time{ println(solution3()) }
		// stack overflow....
		
		time{ println(solution4()) }
		// Elapsed time: 1184.336899ms
	}

}