// Exercise 2.1 - Write a recursive Fibonacci
// Notes: Termination call should be when we hit 0, i.e. no more numbers to calculate for a given value
def fib(n: Int): Int = {
	@annotation.tailrec
	def next(n: Int, s1: Int, s2:Int): Int = { 
		if (n == 0) s1
		else next(n-1,s2,s1+s2)
	}
	next(n,0,1)
}


// Exercise 2.2 - Implement isSorted, which checks whether an Array[A] is sorted according to a supplied function
// Notes: Lots of early termination clauses

def isSorted[A](as:Array[A], ordered: (A,A) => Boolean): Boolean = {
	@annotation.tailrec
	def next(x: Int, previous: A): Boolean = {
		if (x == as.length) true
		else if ( ordered(as(x), previous) ) next(x+1, ordered(x))
		else false
	}
	if(as.length <= 0) true
	else next(1,as(0))
}

// Exercise 2.3 - Convert a function f of two arguments into a function of one argument that partially applies f

def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
	(a: A) => (b: B) => f(a,b)
}

// Exercise 2.4 - Implement uncurry, which reverses the transformation of curry (from 2.3)

def uncurry[A,B,C](f:A => B => C): (A,B) => C = {
	((a: A), (b: B)) => f(a)(b)
}

// Exercise 2.5 - Implement the higher-order function that composes two functions

def compose[A,B,C](f: B => C, g: A => B): A => C = {
	a: A => f(g(a))
}