// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*

@main def runAllExercises(): Unit =
  def printSeparator(exerciseNumber: String): Unit =
    println(s"\n===== Exercise $exerciseNumber =====")

  // Initialize an RNG
  val rng = RNG.SimpleRNG(42)

  // Exercise 1: Testing nonNegativeInt
  printSeparator("1: nonNegativeInt")
  val (nonNegInt1, rng1) = RNG.nonNegativeInt(rng)
  println(s"nonNegativeInt: $nonNegInt1")
  
  // Exercise 2: Testing double
  printSeparator("2: double")
  val (doubleValue, rng2) = RNG.double(rng1)
  println(s"double: $doubleValue")

  // Exercise 3: Testing intDouble and doubleInt
  printSeparator("3: intDouble and doubleInt")
  val ((intValue1, doubleValue2), rng3) = RNG.intDouble(rng2)
  println(s"intDouble: (Int: $intValue1, Double: $doubleValue2)")
  
  val ((doubleValue3, intValue2), rng4) = RNG.doubleInt(rng3)
  println(s"doubleInt: (Double: $doubleValue3, Int: $intValue2)")

  // Exercise 4: Testing ints
  printSeparator("4: ints")
  val (intList, rng5) = RNG.ints(5)(rng4)
  println(s"ints (5 random ints): $intList")

  // Exercise 5: Testing double2
  printSeparator("5: double2")
  val (double2Value, rng6) = RNG.double2(rng5)
  println(s"double2: $double2Value")

  // Exercise 6: Testing map2 with two random generators
  printSeparator("6: map2")
  val randIntDouble = RNG.map2(RNG.nonNegativeInt, RNG.double)((i, d) => (i, d))
  val ((mappedInt, mappedDouble), rng7) = randIntDouble(rng6)
  println(s"map2 (Int, Double): (Int: $mappedInt, Double: $mappedDouble)")

  // Exercise 7: Testing sequence
  printSeparator("7: sequence")
  val randList = List.fill(5)(RNG.int)
  val randSeq = RNG.sequence(randList)
  val (seqResult, rng8) = randSeq(rng7)
  println(s"sequence (List of 5 random ints): $seqResult")

  // Exercise 8: Testing nonNegativeLessThan
  printSeparator("8: nonNegativeLessThan(10)")
  val (nonNegLessThanValue, rng9) = RNG.nonNegativeLessThan(10)(rng8)
  println(s"nonNegativeLessThan: $nonNegLessThanValue")

  // Exercise 9: State - Testing sequence of State actions
  printSeparator("9: State.sequence")
  val stateActions = List(State.get[RNG], State.get[RNG], State.get[RNG])
  val combinedStateAction = State.sequence(stateActions)
  val (stateResult, _) = combinedStateAction.run(rng9)
  println(s"State.sequence: List of RNGs returned")

  // Exercise 10: Testing stateToLazyList
  printSeparator("10: stateToLazyList")
  val stateFunction = State[RNG, Int](_.nextInt)
  val lazyList = State.stateToLazyList(stateFunction)(rng)
  println(s"stateToLazyList (First 5 random numbers from LazyList): ${lazyList.take(5).toList}")

  // Exercise 11: Testing lazyInts
  printSeparator("11: lazyInts")
  val lazyInts = State.lazyInts(rng)
  println(s"lazyInts (First 5 random ints): ${lazyInts.take(5).toList}")

  // Exercise 12: Testing tenStrictInts
  printSeparator("12: tenStrictInts")
  println(s"tenStrictInts: ${State.tenStrictInts}")



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


  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    rng.nextInt match
      case (i, nextRng) if i < 0 =>
        //+1 to handle edge case where i = Int.MinValue 
        val nextElement = -(i + 1)
        (nextElement, nextRng)
      //if positive, just use the value
      case (i, nextRng) => (i, nextRng)

  


  // Exercise 2
  def double(rng: RNG): (Double, RNG) = 
    nonNegativeInt(rng) match
      case (nonNegInt, nextRng) => 
        val nextElement = nonNegInt.toDouble / (Int.MaxValue.toDouble + 1)
        (nextElement, nextRng)

  


  // Exercise 3
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (nonNegInt, rng2) = nonNegativeInt(rng)
    val (randomDouble, rng3) = double(rng2)
    ((nonNegInt, randomDouble), rng3)
  

  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val (randomDouble, rng2) = double(rng)
    val (nonNegInt, rng3) = nonNegativeInt(rng2) 
    ((randomDouble, nonNegInt), rng3)
  

  

  // Exercise 4

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    //If count is 0, return empty list and rng
    if count <= 0 then 
      (List(), rng)
    else
      //else we get the nextElement and new state
      val (previousList, newRng) = ints(count - 1)(rng)
      //we generate a new integer
      val (randomInt, nextRng) = newRng.nextInt
      //We append new integer to our list, and pass new rng state.
      (randomInt :: previousList, nextRng)

  
    
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
  // kinda like zip.
  def map2[A, B, C](randomA: Rand[A], randomB: Rand[B])(combine: (A, B) => C): Rand[C] = 
    rng => {
      val (valueA, rng2) = randomA(rng)
      //Use rng state from randomA
      val (valueB, rng3) = randomB(rng2)
      val nextElement = combine(valueA, valueB)
      (nextElement, rng3)
    }


  // Exercise 7
  def sequence[A](randomGenerators: List[Rand[A]]): Rand[List[A]] = 
    
    //initial state
    val initialAccumulator: Rand[List[A]] = unit(List[A]())
    
    //Folder
    def folder(randomGenerator: Rand[A], combinedRand: Rand[List[A]]): Rand[List[A]] = 
      map2(randomGenerator, combinedRand)(_ :: _)
    
    randomGenerators.foldRight(initialAccumulator)(folder)


  def ints2(size: Int): Rand[List[Int]] = 
    rng => 
      def loop(count: Int, acc: (List[Int], RNG)): (List[Int], RNG) =
        if count <= 0 then acc
        else
          val (randomInt, nextRng) = acc._2.nextInt
          loop(count - 1, (randomInt :: acc._1, nextRng))

      loop(size, (List(), rng))
  


  // Exercise 8

  def flatMap[A, B](randomValue: Rand[A])(nextRandomGenerator: A => Rand[B]): Rand[B] =
    //Rand[B] is a function that takes a RNG and returns a tuple of B and RNG
    //so we start with a lambda with input rng and return a tuple of B and RNG
    //We return this lambda.
    rng => 
      val (value, rng2) = randomValue(rng)
      val nextRandFunction = nextRandomGenerator(value)
      nextRandFunction(rng2)
    


  def nonNegativeLessThan(bound: Int): Rand[Int] = 
    flatMap(nonNegativeInt) { nonNegInt =>
      val mod = nonNegInt % bound
      if nonNegInt + (bound - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(bound)
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
    val stateFunction: State[RNG, Int] = State(_.nextInt)
    stateToLazyList(stateFunction)(rng)
  

  lazy val tenStrictInts: List[Int] = 
    lazyInts(RNG.SimpleRNG(42)).take(10).toList


end State
