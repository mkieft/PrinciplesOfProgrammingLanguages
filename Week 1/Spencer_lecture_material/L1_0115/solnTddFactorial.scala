/* solnTddFactorial.scala
	I left the TDD intermediate function implementations in with comments.
	Disclaimer: I didn't actually test my intermediate solutions

	all this does is call the test function
	The test function doesn't return anything
	If you pass all the tests, then you'll get 100.
	If you fail a test, you'll see an error message. 
 */


// // initial solution. just a function definition. with a simple return value
// // x! = x * (x-1)!
// def factorial(x:Int):Int = {
// 	0
// }


// // next solution. Solves all the base cases
// // 0! == 1
// // 1! == 1
// // x! = x * (x-1)!
// def factorial(x:Int):Int = {
// 	1
// }


// // next solution solves the inductive cases
// // 2! = 2
// // 3! = 6
// // 4! = 24
// // x! = x * (x-1)!
// def factorial(x:Int):Int = {
// 	if (x == 0) {
// 		1
// 	}
// 	else if (x == 1) {
// 		1
// 	}
// 	else {
// 		x * factorial(x - 1)
// 	}
// }


// // Reimplementation. Cleaning up the code. because I feel like it
// // x! = x * (x-1)!
// def factorial(x:Int):Int = {
// 	if (x == 0 || x == 1) {
// 		1
// 	}
// 	else {
// 		x * factorial(x - 1)
// 	}
// }


// // An implementaiton that doesn't allow negatives
// // x! = x * (x-1)!
// def factorial(x:Int):Int = {
// 	require(x >= 0)
// 	if (x == 0 || x == 1) {
// 		1
// 	}
// 	else {
// 		x * factorial(x - 1)
// 	}
// }


// // reimplemented. because I feel like it.
// // x! = x * (x-1)!
// def factorial(x:Int):Int = {
// 	require(x >= 0)
// 	def fact(x:Int): Int = {
// 		if (x == 0 || x == 1) {
// 			1
// 		}
// 		else {
// 			x * fact(x - 1)
// 		}
// 	}
//  fact(x)
// }


// Final implementaiton: reimplemented because I prefer pattern matching...
// x! = x * (x-1)!
def factorial(x:Int):Int = {
	require(x >= 0)
	def fact(x:Int): Int = x match {
		case 0 | 1 => 1
		case _ => x * fact(x - 1)
	}
	fact(x)
}

/*
	Tail recursion is a topic for another day
 */
// // with tail recursion optimization...
// // x! = x * (x-1)!
// def factorial(x:Int):Int = {
// 	require(x >= 0)
// 	def fact(x:Int, sc:(Int) => Int): Int = x match {
// 		case 0 | 1 => sc(1)
// 		case _ => fact(x - 1, factXMinusOne => x * sc(factXMinusOne))
// 	}
// 	fact(x, r => r)
// }

// // with tail recursion optimization... but written in a not functional way
// // x! = x * (x-1)!
// def factorial(x:Int):Int = {
// 	require(x >= 0)
// 	def fact(x:Int, sc:(Int) => Int): Int = x match {
// 		case 0 | 1 => sc(1)
// 		case _ => {
// 			def nextSc(factXMinusOne:Int):Int = {
// 				x * sc(factXMinusOne)
// 			}
// 			fact(x - 1, nextSc)
// 		}
// 	}

// 	def scInit(residual:Int) = residual
// 	fact(x, scInit)
// }


def test():Unit = {
	// I broke this into multiple functions, because why not...
	testBaseCases()
	testInductiveCases()
	// testNegativeCases()
	println("//||   _--_   _--_")
	println("  ||  | () | | () |")
	println("  ||   -__-   -__-")

}


def testNegativeCases():Unit = {
	// DISCLAIMER: this is a pretty hacky test, but it works in a useful way
	try {
		factorial(-1)
		throw new Error("Failed on -1")
	} catch {
		case error:java.lang.IllegalArgumentException => assert(1 == 1)
		case e:Throwable => throw e
	}
}


def testInductiveCases():Unit = {
	// 1 * 2 => 2
	assert(2 == factorial(2))
	// 2 * 3 => 6
	assert(6 == factorial(3))
	// 6 * 4 => 24
	assert(24 == factorial(4))
	// TODO: write another test
}

def testBaseCases():Unit = {
	val fact0 = factorial(0)
	// For testing, I prefer to use assert statements over printing
	// but you do you
	// println(fact0)
	assert(fact0 == 1)

	val fact1 = factorial(1)
	// order in about '==' shouldn't matter
	assert(fact1 == 1)
	// bellow is faster in C, but idk about Scala
	assert(1 == fact1)
}


test()
