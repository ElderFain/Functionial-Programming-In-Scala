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
