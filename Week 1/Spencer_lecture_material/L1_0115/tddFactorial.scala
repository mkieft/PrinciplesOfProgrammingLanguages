/* tddFactorial.scala
	complete the TODOs
	then check out the solns that I've posted
	all this does is call the test function
	The test function doesn't return anything
	If you pass all the tests, then you'll get 100.
	If you fail a test, you'll see an error message. 
 */


// x! = x * (x - 1)!
def factorial(x:Int):Int = {
 // TODO: implement for base case
 // TODO: uncomment testInductiveCases()
 // TODO: implement for test inductive cases
 // TODO: write some negative number test cases
 // TODO: implement to match you negative number test cases
 0
}


def test():Unit = {
	// I broke this into multiple functions, because why not...
	testBaseCases()
	// TO
	// testInductiveCases()
	// TODO: test negative numbers

	println("//||   _--_   _--_")
	println("  ||  | () | | () |")
	println("  ||   -__-   -__-")

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
