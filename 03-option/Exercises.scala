// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

// Exercise 1

trait OrderedPoint 
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  /*
  The method should return +1 if the left argument is larger, -1 if the right argument is larger, and zero
if both sides are equal.*/
  override def compare(that: java.awt.Point): Int = 
    //left = this point
    //right = that point

    //First we check x-coordinates, then y-coordinates
    //Left higher: +1
    if this.getX() > that.getX() then 1
    //Right higher: -1
    else if this.getX() < that.getX() then -1
    else
      //left higher: +1
      if this.getY() > that.getY() then 1
      //right higher: -1
      else if this.getY() < that.getY() then -1
      //Equal: 0
      else 0 


// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2 Exercise 2[E]. Write a recursive function size that counts nodes (leaves and branches) in a tree.

  def size[A](t: Tree[A]): Int = 
    t match
      //et leaf er 1 node
      case Leaf(_) => 1
      //En branch er også 1 node, og så har den to undernoder som tæller sig selv rekursivt
      case Branch(l, r) => 1+ size(l) + size(r) 

  // Exercise 3
  /*
  Exercise 3[E]. Write a recursive function maximum that returns the maximum element in a Tree[Int].
Note: In Scala, you can use x.max(y) (or x max y) to compute the maximum of two integers
x and y.3*/

  def maximum(t: Tree[Int]): Int = 
    t match
      case Leaf(v) => v
      //Givet af opgaven. Vi bruger HOF max, som er en metode på Int
      case Branch(l, r) => maximum(l).max(maximum(r))

  // Exercise 4Exercise 4 [E]. Write a function map, analogous to the function of the same name on List, that
  //modifies each element in a tree with a given function transforming the elements.

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = 
    t match
      //Vi kalder kun vores funktion på leafs
      case Leaf(v) => Leaf(f(v))
      //Har vi ikke et leaf, men en branch, passere vi blot vores funktion videre til vores undernoder
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))

  // Exercise 5
  /*
  Exercise 5[H]. Generalize size, maximum, and map, writing a new function fold that abstracts over
their similarities. Reimplement them in terms of this more general function*/

  def fold[A,B](t: Tree[A])(combineBranches: (B, B) => B)(transformLeaf: A => B): B = 
    //g is the function that transforms the leaf
    //f is the function that combines the results of the left and right branches
    t match
      //Enten har vi et leaf, hvor vi kalder vores transformLeaf funktion
      case Leaf(v) => transformLeaf(v)
      //Eller også har vi en branch, hvor vi blot passerer vores funktioner videre til vores undernoder
      case Branch(l, r) => 
        combineBranches(fold(l)(combineBranches)(transformLeaf), fold(r)(combineBranches)(transformLeaf))


  def size1[A](t: Tree[A]): Int = 
    //Hver branch er en node, hvor vi tæller +1 
    val combineBranches: (Int, Int) => Int = 
      (left, right) => left + right + 1
    //Hver leaf er også en node, hvor vi tæller +1
    val transformLeaf: (leaf:A) => Int = 
      leaf => 1

    fold(t)(combineBranches)(transformLeaf)

  def maximum1(t: Tree[Int]): Int =
    //Ligesom maximum, vi bruger HOF max, for at tjekke left og right nodes 
    val combineBranches: (Int, Int) => Int = 
      (left, right) => left.max(right)
    //Et leaf er bare selve værdien den holder.
    val transformLeaf: (value: Int) => Int = 
      value => value

    fold(t)(combineBranches)(transformLeaf)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = 
    //Branches sender vi videre til vores undernoder
    val combineBranches: (left: Tree[B], right: Tree[B]) => Tree[B] = 
      (left, right) => Branch(left, right)

    //Vi kalder kun vores f, på leafs
    val transformLeaf: A => Tree[B] = 
      value => Leaf(f(value))

    fold(t)(combineBranches)(transformLeaf)




enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6
  /*
  Generelt er disse to cheatsheets gode til at forstå opgaven:
https://gist.github.com/marconilanna/66c65c548ed879d875364c0f0cd8778a
https://blog.tmorris.net/posts/scalaoption-cheat-sheet/
  */

  def map[B](f: A => B): Option[B] = 
    this match
      case None => None
      //Det er sådan map kan ende med at have Some(Some(value))
      case Some(a) => Some(f(a))

  def flatMap[B](f: A => Option[B]): Option[B] =  
    this match
      case None => None
      //Men derimod flatMap, ender vi altid kun med en enkelt Some, såsom Some(value)
      case Some(a) => f(a)

  def getOrElse[B >: A] (default: => B): B =
    //Er der en værdi, giver vi blot værdien, ellers giver vi default
    this match
      case None => default
      case Some(a) => a
    

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    //Er der en værdi, giver vi den, ellers giver vi ob 
    this match
      case None => ob
      case Some(a) => Some(a)
    

  def filter(p: A => Boolean): Option[A] = 
    //Best teacher tip: filter is where from c# - Otherwise it sounds like we are filtering stuff away
    //så er vores predicate p, sandt, så giver vi værdien, ellers None
    this match
      case None => None
      case Some(a) => if p(a) then Some(a) else None

  // Scroll down for Exercise 7, in the bottom of the file, outside Option
  def forAll(p: A => Boolean): Boolean = 
    this match
      case None => true
      case Some(a) => p(a)
    

object Option:

  // Exercise 9
  /*
  Write a generic function map2 that combines two Option values using a binary function:
    If either Option value is None, then the return value is None, too. 
    Do not use pattern matching.
    Use map/flatMap or for-yield comprehensions.
Besides being a lifter of binary functions to the Option universe, the map2 function can also be seen
as a sequencer: it combines (sequences) the result of a fallible execution producing ao with the result
of a fallible execution producing ab. The combination is done using f if both results are successful
    */

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    //HOF version
    ao.flatMap(a => bo.map(b => f(a,b)))

  def map3[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    //For comprehension version
    for 
      a <- ao //same as flatmap Ao. And a is each element in Ao
      b <- bo //same as flatmap Bo. And b is each element in Bo
    yield f(a,b)  //same as map (a,b) => f(a,b)


  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    //Vi starter med en tom liste, som vi vil bygge op
    val initAcc = Some(List.empty[A])
    //vi bygger listen op med en funktion som concatter elementer til acclisten
    val folder = (ao: Option[A], acc: Option[List[A]]) => map2(ao, acc)(_::_)
    //Vi bruger foldRight, da vi vil bygge listen op fra højre mod venstre
    aos.foldRight(initAcc)(folder)

  // Exercise 11
  /*
  Exercise 11[H]. Implement a function traverse:
def traverse[A, B](a: List[A])(f: A =>Option[B]): Option[List[B]]
The function behaves like map executed sequentially on the list a, where the mapped function f can
fail. If at least one application fails, then the entire computation of the mapping (the traversal) fails.
The traverse function does not increase the power of what we can do, but it can be implemented
more efficiently than using map and sequence in combination. This is because map, being a
polymorphic structure-preserving function oblivious to the types of transformed values, will not see
by itself that it should stop on the first None. Think which lower-level combinator you can use, if
map is too simple here.*/

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    //Vi starter med en tom liste, som vi vil bygge op
    val initAcc = Some(List.empty[B])
    //Vi bygger listen op med en funktion som concatter f(elemente) til acclisten
    val folder = (a: A, acc: Option[List[B]]) => map2(f(a), acc)(_::_)
    as.foldRight(initAcc)(folder)
    
end Option

 

// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h:: t => Some(h)

// Exercise 7 version 1

def headGrade(lst: List[(String,Int)]): Option[Int] =
  //HOF: Map version
  //Vi tager Some(head), som er en tuple af (navn,grade), og tager det andet element i tuplen med ._2
  headOption(lst).map(pairvalue => pairvalue._2)

// Exercise 7 version 2
def headGrade1(lst: List[(String,Int)]): Option[Int] =
  //For comprehension version
  for
    //Vi tager Some(head), som er en tuple af (navn,grade) 
    pairValue <- headOption(lst)
    //yield er ligesom map, så vi tager andet element i tuplen med ._2
  yield pairValue._2

// Implemented in the text book
def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8 version 1
def variance(xs: Seq[Double]): Option[Double] =
  //Map version
  //Vi tager (x -gns(xs))^2 for hver x i xs
  val varianceofEachElement = xs.map(x => math.pow(x - xs.sum / xs.length, 2))
  //og returnere gns. af varianceofEachElement
  mean(varianceofEachElement)

// Exercise 8 version 2
def variance1(xs: Seq[Double]): Option[Double] =
  //For comprehension version
  for 
    meanValue <- mean(xs)
    varianceofEachElement = xs.map(x => math.pow(x - meanValue, 2))
    varianceMean <- mean(varianceofEachElement)
  yield varianceMean
