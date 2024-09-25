// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    val (randomInt, nextRng) = rng.nextInt
    val nonNegativeInt = if (randomInt < 0) -(randomInt + 1) else randomInt
    (nonNegativeInt, nextRng)
  


  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    val (nonNegInt, nextRng) = nonNegativeInt(rng)  // Renamed the variable to nonNegInt
    val randomDouble = nonNegInt.toDouble / (Int.MaxValue.toDouble + 1)
    (randomDouble, nextRng)
  

  


  // Exercise 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (nonNegInt, rng2) = nonNegativeInt(rng)
    val (randomDouble, rng3) = double(rng2)
    ((nonNegInt, randomDouble), rng3)
  

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (randomDouble, rng2) = double(rng)
    val (nonNegInt, rng3) = nonNegativeInt(rng2) 
    ((randomDouble, nonNegInt), rng3)
  }

  


  // Exercise 4

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 

    // Define the initial accumulator
    val initialAccumulator: (List[Int], RNG) = (List(), rng)

    // Define the folder function
    def folder(currentCount: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      val (randomInt, nextRng) = acc._2.nextInt
      (randomInt :: acc._1, nextRng)
    }

    // Perform the foldRight-like recursion (count is positive)
    if (count <= 0) initialAccumulator
    else folder(count - 1, ints(count - 1)(rng))
  

  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] = 
    map(nonNegativeInt)(nonNegativeInt => nonNegativeInt.toDouble / (Int.MaxValue.toDouble + 1))


  // Exercise 6

  def map2[A, B, C](randomA: Rand[A], randomB: Rand[B])(combine: (A, B) => C): Rand[C] = 
    rng => {
      val (valueA, rng2) = randomA(rng)
      val (valueB, rng3) = randomB(rng2)
      (combine(valueA, valueB), rng3)
    }


  // Exercise 7
  def sequence[A](randomGenerators: List[Rand[A]]): Rand[List[A]] = 
    
    // Define the initial accumulator
    val initialAccumulator: Rand[List[A]] = unit(List[A]())
    
    // Define the folder function
    def folder(randomGenerator: Rand[A], combinedRand: Rand[List[A]]): Rand[List[A]] = {
      map2(randomGenerator, combinedRand)(_ :: _)
    }

    // Perform the foldRight operation using the folder function
    randomGenerators.foldRight(initialAccumulator)(folder)


  def ints2(size: Int): Rand[List[Int]] = rng => {

    // Define the folder function that explicitly generates random integers
    def folder(count: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      if (count <= 0) acc
      else {
        val (randomInt, nextRng) = acc._2.nextInt
        folder(count - 1, (randomInt :: acc._1, nextRng))
      }
    }

    // Use the folder to generate the list of integers
    folder(size, (List(), rng))
  }


  // Exercise 8

  def flatMap[A, B](randomValue: Rand[A])(nextRandomGenerator: A => Rand[B]): Rand[B] =
    rng => {
      val (value, rng2) = randomValue(rng)
      nextRandomGenerator(value)(rng2)
    }

  def nonNegativeLessThan(bound: Int): Rand[Int] = 
    flatMap(nonNegativeInt) { nonNegativeInt =>
      val mod = nonNegativeInt % bound
      if (nonNegativeInt + (bound - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(bound)
    }


end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below
  
  def map[B](transform: A => B): State[S, B] = 
    flatMap(value => State.unit(transform(value)))

  def map2[B, C](stateB: State[S, B])(combine: (A, B) => C): State[S, C] = 
    flatMap(valueA => stateB.map(valueB => combine(valueA, valueB)))

  def flatMap[B](nextStateFunction: A => State[S, B]): State[S, B] = 
    State(state => {
      val (value, nextState) = run(state)
      nextStateFunction(value).run(nextState)
    })



object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)
 
  def sequence[S, A](stateFunctions: List[State[S, A]]): State[S, List[A]] = 

    // Define the initial accumulator
    val initialAccumulator: State[S, List[A]] = State.unit[S, List[A]](List())

    // Define the folder function
    def folder(stateFunction: State[S, A], combinedState: State[S, List[A]]): State[S, List[A]] = {
      stateFunction.map2(combinedState)(_ :: _)
    }

    // Perform the foldRight operation using the folder function
    stateFunctions.foldRight(initialAccumulator)(folder)



  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](stateFunction: State[S, A])(initialState: S): LazyList[A] = {

    // Define the folder function for recursively generating the lazy list
    def folder(currentState: S): LazyList[A] = {
      val (value, nextState) = stateFunction.run(currentState)
      LazyList.cons(value, folder(nextState))
    }

    // Start generating the LazyList from the initial state
    folder(initialState)
  }

  


  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] = 

    // Properly construct a `State` that uses `RNG` and produces `Int`
    val stateFunction: State[RNG, Int] = State(_.nextInt)

    // Use the `stateToLazyList` function to generate the LazyList of random integers
    stateToLazyList(stateFunction)(rng)
  



  lazy val tenStrictInts: List[Int] = 
    lazyInts(RNG.SimpleRNG(42)).take(10).toList


end State
