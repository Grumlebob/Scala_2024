// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List: 

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException() 
    case Cons(h, _) => h                                                                                                                                                                                                                                  
  
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2)) 

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))
    
  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)
/*
Exercise 1[E]. What is the value of the following match expression?1 Answer without running the
code

Answer: 3

import adpro.adt.List.*
List(1, 2, 3, 4, 5) match
case Cons(x, Cons(2, Cons(4, _))) => x
case Nil => 42
case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
case Cons(h, t) => h
case _ => 101
Which function from adpro.adt is called in this example?*/

  // Exercise 2
  def tail[A] (l: List[A]): List[A] = 
    l match
      case Cons(head, tail) => tail
      case Nil => throw NoSuchElementException()

  // Exercise 3
  /*Exercise 3 [E]. Generalize tail to drop, a function that removes the first n elements from a list.
The running time should be proportional to n—no need to make a copy of the entire list. Throw
NoSuchElementException if the list is too short.3 For non-positive n the list is unchanged.*/
  
  def drop[A] (l: List[A], n: Int): List[A] = 

      (l, n) match
        case (l, n) if n <= 0 => l
        case (Nil, _) => throw NoSuchElementException()
        case (l, 0) => l
        case (Cons(_, t), n) => drop(t, n - 1)

  // Exercise 4
/*
Exercise 4[M]. Implement dropWhile, which removes elements starting the head of the list l, as long
as they satisfy a predicate p. Do not use exceptions: if all elements satisfy p then return the empty list.
Remark. dropWhile is useful when we process a list and search only for interesting elements. We
can then characterize what interests us as not p and ignore elements until we meet what we want.
Often, however, it is more elegant to just filter the elements satisfying p.*/
  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = 
    l match
      //If head satisfy, we keep dropping
      case Cons(h, t) if p(h) => dropWhile(t, p)
      //if head doesn't satisfy, we will go into this match case and return the list
      case _ => l


  // Exercise 5
  /*
  Exercise 5 [H]. Implement a function init that returns a list consisting of all but the last element
of the original list. Given List(1, 2, 3, 4), the function returns List(1, 2, 3). Throw
NoSuchElementException if the list is empty.
def init[A](l: List[A]): List[A]
Does this function take constant time, like tail? Does it take constant space? 5*/
  def init[A] (l: List[A]): List[A] = 

    l match
      //If list is empty, we can't drop anything and therefore throw exception
      case Nil => throw NoSuchElementException()
      //with one element, we return Nil
      case Cons(_, Nil) => Nil
      //If more than one element, we keep the head and call init recursively on the tail
      case Cons(h, t) => Cons(h, init(t))

    

  // Exercise 6
/*Exercise 6 [M]. Compute the length of a list using foldRight.6 Remember that foldRight has
been presented briefly in the lecture slides, in the text book; you can find it in the top of the file
Exercises.scala. Also, the next exercise has an example demonstrating the essence of foldRight.
def length[A](l: List[A]): Int*/
  def length[A] (l: List[A]): Int = 
    //Args:
    //l: List[A] - The list we want to find the length of
    //0, is our acc! counter starts at 0
    // (ele,acc) => acc + 1. We don't care about the elements, we just want to incease acc counter by 1
    val startacc = 0
    foldRight(l, startacc, (ele, acc) => acc + 1)

  // Exercise 7
/*
Exercise 7[M]. The function foldRight presented in the book is not tail-recursive and will result
in a StackOverflowError for large lists. Convince yourself that this is the case, and then write
another general list-recursion function, foldLeft, that is tail-recursive:
def foldLeft[A,B](l: List[A], z: B) (f: (B, A) =>B): B
For comparison consider that:
foldLeft (List(1, 2, 3, 4),0) (_ + _) computes (((0 + 1) + 2) + 3) + 4 while
foldRight(List(1, 2, 3, 4),0) (_ + _) computes 1 + (2 + (3 + (4 + 0))).
In this case the result is obviously the same, but not always so.7*/
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = 

    @annotation.tailrec
    def loop (l: List[A], acc: B): B = 
      l match
        //When we reached the end, just return acc. The acc makes it tail recursive!
        case Nil => acc
        //Pass tail. Each iteration call out function with the acc and the head.
        case Cons(h, t) => loop(t, f(acc, h))
    loop(l, z)

  

  // Exercise 8
/*Exercise 8[M]. Write product (computing a product of a list of integers) and a function to compute
the length of a list using foldLeft.8*/
  def product (as: List[Int]): Int = 
    val startAcc = 1
    //If acc is 0, then whole result will be 0. So we set it to 1, as 1 is neutral element for multiplication
    // (_ * _) is the same as (a,b) => a * b
    foldLeft(as, startAcc, _ * _)

  def length1[A] (as: List[A]): Int =
    val startAcc = 0
    foldLeft(as, startAcc, (acc, _) => acc + 1)

  // Exercise 9
  /*Exercise 9 [H]. Write a function that returns the reverse of a list (given List (1,2,3), it returns
List (3,2,1)). Use one of the fold functions.9*/
  def reverse[A] (l: List[A]): List[A] = 
    //foldright
    val startAcc = Nil
    //Vi propper head foran "resten af listen"
    foldLeft(l, startAcc, (acc, ele) => Cons(ele, acc))
 
  // Exercise 10
/*Exercise 10[M]. Write foldRight using foldLeft and reverse. The left fold performs the dual
operation to the right one, so if you reverse the list you should be able to simulate one with the other.
This version of foldRight is useful because it is tail-recursive, which means it works even for large
lists without overflowing the stack. On the other hand, it is slower by a constant factor.*/
  def foldRight1[A, B] (l: List[A], acc: B, f: (A, B) => B): B = 
    //We reverse the list, and then foldLeft it
    foldLeft(reverse(l), acc, (b, a) => f(a, b))

  // Exercise 11
/*Exercise 11 [H]. Write foldLeft in terms of foldRight. Do not use reverse here (reverse is a
special case of foldLeft so a solution based on reverse is cheating).
Hint: This may well be the most difficult exercise in the entire course. Synthesize a function that
computes the run of foldLeft, and then invoke this function. To implement foldLeft[A, B] you
will be calling foldRight with the following type parameters:
foldRight[A, B =>B] (... , ..., ...)
This will compute a new function, which then needs to be called.
Note: From now on, the use of explicit recursion is bad-smell for us. Only use explicit recursion
when dealing with a non-standard iteration. Otherwise, you should use a suitable HOF. Similarly, a
you should only use fold if any of the other simpler HOFs cannot.*/
  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = 
    //TODO - IKKE MIN LØSNING.
    foldRight(l, (b: B) => b, (a, g) => b => g(f(b, a)))(z)

 
  // Exercise 12
/*Exercise 12[M]. Write a function that concatenates a list of lists into a single list. Its runtime should
be linear in the total length of all lists. Use append that concatenates two lists (find it in the book
and in the source file)*/
  def concat[A] (l: List[List[A]]): List[A] = 
    val startAcc = Nil
    foldLeft(l, startAcc, append)
  
  // Exercise 13
/*Exercise 13[M]. Implement filter that removes from a list the elements that do not satisfy p.12
def filter[A] (l: List[A]) (p: A =>Boolean): List[A]*/
  def filter[A] (l: List[A], p: A => Boolean): List[A] =
    foldRight(l, Nil, (ele, acc) => if p(ele) then Cons(ele, acc) else acc)


  // Exercise 14
/*Exercise 14[M]. Write a function flatMap that works like map except that f, the function mapped, returns
a list instead of a single value, and the result is automatically flattened to a list like with concat:
def flatMap[A,B] (l: List[A]) (f: A =>List[B]): List[B]
For instance, flatMap(List(1, 2, 3)) (i =>List(i, i)) results in List(1, 1, 2, 2, 3, 3).
Together with map, (flatMap) will be key in the rest of the course. Understand this well.*/
  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] = 
    val nonFlattened = map(l, f)
    concat(nonFlattened)
    

  // Exercise 15
  /*Exercise 15[M]. Use flatMap to re-implement filter.14*/
  def filter1[A] (l: List[A], p: A => Boolean): List[A] = 
    flatMap(l, ele => if p(ele) then List(ele) else Nil)

  // Exercise 16
  /*Exercise 16[M]. Write a recursive function that accepts two lists of integers and constructs a new list
by adding elements at the same positions. If the lists are not of the same length, the function drops
trailing elements of either list. For example, the lists List(1,2,3) and List(4,5,6,7) become
List(5,7,9)*/
  def addPairwise (l: List[Int], r: List[Int]): List[Int] = 
    @annotation.tailrec
    def loop (l: List[Int], r: List[Int], acc: List[Int]): List[Int] = 
      (l, r) match
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(h1 + h2, acc))
    reverse(loop(l, r, Nil))

  // Exercise 17
/*Exercise 17[M]. Generalize the function you just wrote so that it is not specific to integers or addition.
It should work with arbitrary binary operations. Name the new function zipWith*/
  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = 
    @annotation.tailrec
    def loop (l: List[A], r: List[B], acc: List[C]): List[C] = 
      (l, r) match
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))
    reverse(loop(l, r, Nil))

  // Exercise 18
/*Exercise 18 [H]. Implement a function hasSubsequence for checking whether a List contains
another List as a subsequence. For instance, List(1,2,3,4) would have List(1,2) , List(2,3),
and List(4) as subsequences, among others. You may have some difficulty finding a concise purely
functional implementation that is also efficient. That is okay. Implement the function that comes
most naturally, but is not necessarily efficient. Note: Any two values x and y can be compared for
equality in Scala using the expression x ==y. Here is the suggested type:
def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean
Recall that an empty sequence is a subsequence of any other sequence*/
  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = 

    def loop(left: List[A], right: List[A]): Boolean = 
      (left, right) match
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) => 
          if h1 == h2 then loop(t1, t2)
          else loop(t1, right)
    loop(sup, sub)
