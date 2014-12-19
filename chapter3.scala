sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1 - What will be the result of the following match expression?
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x // Fail at , Cons(4,_)
    case Nil => 42  // Fail, no Nil type
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // Success, x= 1, y = 2, return 3
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  // Exercise 3.2 - Implement the function 'tail' for removing the first element of a List
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil // If the list is nil, we could return an empty list, throw an error, return Nil (not a List). 
      // Could we somehow not allow Tail to work on a function with an empty list? Change the signature?
      case Cons(_,t) => t
    }
  }

  // Exercise 3.3 - Implement the function setHead for replacing the first element of a List with a different value
  def setHead[A](l: List[A], a: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h,t) => Cons(a,t)
    }
  }

  // Exercise 3.4 - Implement Drop, which removes the first n elements from a list
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil,
      case Cons(_,t) => drop(t,n-1)
    }
  }

  // Exercise 3.5 - Implement dropWhile, which removes elements from the list as long as they match a predicate
  // I did not get this at all, I was thinking very...procedural 
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h,t) if f(h) => dropWhile(t,f)
      case _ => l
    }
  }

  // Exercise 3.6 - Implement a function, `init` that returns a `List` consisting of all but the last element of a `List`
  def init[A](l: List[A]): List[A] = {
    
  }
}


