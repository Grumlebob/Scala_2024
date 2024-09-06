// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.intro

import scala.annotation.tailrec

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1
  def square(n: Int): Int =
    n * n

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))

end MyModule

/** Exercise 2. [E] In functional languages it is common to experiment with code in an interactive way
in a REPL (read-evaluate-print-loop). Start Scala’s repl using scala-cli repl .. This starts scala
with your project loaded and the classpath configured. Experiment with calling adpro.intro.MyModule.abs
and square interactively. Store results in new values (using val).
* */

// Exercise 3
def fib(n: Int): Int =

  @annotation.tailrec
  def loop(acc: Int, current: Int, loopCounter: Int): Int =
    if loopCounter <= 1 then acc // Base case: F1 = 0
    else loop(current, acc + current, loopCounter - 1)
  loop(0, 1, n)

@main def runFib: Unit =
  println(fib(10))


// Exercise 4
def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
  
  def loop(prevElement: A, array: Array[A]): Boolean =
    if array.isEmpty then true
    else if !ordered(prevElement, array(0)) then false
    else loop(array(0), array.drop(1))
  
  //Start med head og tail som array. Hvis der er 0 eller 1 element er den altid sorteret.
  if as.isEmpty then true
  else
    if as.length == 1 then true
    else loop(as(0), as.drop(1)) //This is same as head::tail from f#


// Exercise 5
/*Assignment description:
a function that converts a binary function f (function
taking two arguments) into a function that takes one argument and after getting the argument returns
a function awaiting the other argument. Once both arguments are given, the transformed function
should behave the same as the original f:
* */
def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  def curriedA (a: A): B => C = //A method that takes an A and returns a B => C
    def curriedB (b: B): C = f(a, b) //A method that takes a B and returns a C
    curriedB //we call the method that takes a B and returns a C
  curriedA //we call the method that takes an A and returns a B => C

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  curry(isSorted)

// Exercise 6
def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  def uncurried (a: A, b: B): C = f(a)(b)
  uncurried

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  uncurry(isSortedCurried)


// Exercise 7
def compose[A, B, C](f: B => C, g: A => B): A => C =
  def composed (a: A): C = f(g(a))
  //Vi returnere en enkelt funktion som tager en A og returnere en C. Vi kalder f(g(a)) for at lave vores A til en C
  composed
