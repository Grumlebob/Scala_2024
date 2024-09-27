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


      /*
      Exercise 1. Write a function that uses RNG.nextInt to generate a random integer between 0 and
Int.maxValue. Make sure to handle the corner case when nextInt returns Int.MinValue, which
does not have a non-negative counterpart.1
def nonNegativeInt(rng: RNG): (Int, RNG)*/
  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    rng.nextInt match
      case (i, nextRng) if i < 0 =>
        //+1 to handle edge case where i = Int.MinValue 
        val nextElement = -(i + 1)
        (nextElement, nextRng)
      //if positive, just use the value
      case (i, nextRng) => (i, nextRng)


  /*
  Exercise 2. Write a function to generate a Double between 0 and 1, not including 1. You can
use Int.MaxValue to obtain the maximum positive integer value, and you can use x.toDouble to
convert an x of type Int to a Double.2
def double(rng: RNG): (Double, RNG)*/
  def double(rng: RNG): (Double, RNG) = 
    nonNegativeInt(rng) match
      case (nonNegInt, nextRng) => 
        val nextElement = nonNegInt.toDouble / (Int.MaxValue.toDouble + 1)
        (nextElement, nextRng)
  

  /*
  Exercise 3. Write functions to generate (Int, Double)-pairs and (Double, Int)-pairs, where the
integers are non-negative. You should be able to reuse the functions written above. Add explicit
return type annotations to both functions in your solution.3
1 def intDouble(rng: RNG)
2 def doubleInt(rng: RNG)*/
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (nonNegInt, rng2) = nonNegativeInt(rng)
    val (randomDouble, rng3) = double(rng2)
    ((nonNegInt, randomDouble), rng3)
  

  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val (randomDouble, rng2) = double(rng)
    val (nonNegInt, rng3) = nonNegativeInt(rng2) 
    ((randomDouble, nonNegInt), rng3)
  

  // Exercise 4
  /*
  Exercise 4. Write a function to generate a list of random integers. Add explicit return type annotation
to this function.4
def ints(size: Int)(rng: RNG)
  */

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    //If count is 0, return empty list and rng
    if count <= 0 then 
      (List(), rng)
    else
      //else we get the nextElement and new state
      val (previousList, newRng) = ints(count - 1)(rng)
      //we generate a new integer and new state
      val (randomInt, nextRng) = newRng.nextInt
      //We append new integer to our list, and pass new rng state.
      (randomInt :: previousList, nextRng)

  
    
  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  /*
  Input: A value of type A
  Output: A function that takes a RNG and returns a tuple of A and RNG
  */
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  /*
  Input: 
    s: A function that takes a RNG and returns a tuple of A and RNG
    f: A function that takes A and returns B
  Output:
    A function that takes a RNG and returns a tuple of B and RNG
  */

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = 
    //Map takes a Rand generator and a function takes takes A and returns B
    //NonNegativeInt returns a Rand[Int]
    // i => is our function that changes A to B
    map(nonNegativeInt) { i => i - i % 2 }

  /*
  Exercise 5. Use map to reimplement double for Rand. See Exercise 2 above.
Observe how, for Option, we used the higher order API to avoid using pattern matching, and how
here we use it to avoid being explicit about the state (and also to avoid decomposing, a.k.a. pattern
matching, the results of random generators into value and new state).5
*/

  lazy val double2: Rand[Double] = 
    map(nonNegativeInt)(nonNegativeInt => nonNegativeInt.toDouble / (Int.MaxValue.toDouble + 1))


  /*
  Exercise 6. 
  kinda like zip.
  Implement map2 with the signature shown below. This function takes two generators,
ra and rb, and creates a generator producing values created by calling a function f to combine the
values produced by ra and rb.6*/
  def map2[A, B, C](randomA: Rand[A], randomB: Rand[B])(combine: (A, B) => C): Rand[C] = 
    rng => {
      //First we generate from A
      val (valueA, rng2) = randomA(rng)
      //Use rng state from randomA, to generate from B
      val (valueB, rng3) = randomB(rng2)
      //We combine the result
      val nextElement = combine(valueA, valueB)
      //We return the combined result, and the latest rng state
      (nextElement, rng3)
    }


  /*
  Exercise 7. Implement sequence for combining a List of Rand transitions into a single transition.
Recall that we have already implemented sequence for Option—reuse the experience.
def sequence[A](ras: List[Rand[A]]): Rand[List[A]]
Use sequence to reimplement the ints function you wrote before. For the latter, you can use the
standard library function List.fill(n)(x)7 to make a list with x repeated n times.
*/
  def sequence[A](randomGenerators: List[Rand[A]]): Rand[List[A]] = 
    
    //initial state
    val initialAccumulator: Rand[List[A]] = unit(List[A]())
    
    //Folder
    def folder(randomGenerator: Rand[A], combinedRand: Rand[List[A]]): Rand[List[A]] = 
      //We combine the result of the random generator with the accumulator
      map2(randomGenerator, combinedRand)(_ :: _)
    
    randomGenerators.foldRight(initialAccumulator)(folder)


  def ints2(size: Int): Rand[List[Int]] =
    //we return a function that takes a RNG and returns a tuple of List[Int] and RNG (Rand is shorthard for S => (A, S)) 
    rng => 
      def loop(count: Int, acc: (List[Int], RNG)): (List[Int], RNG) =
        if count <= 0 then acc
        else
          //acc._2 is the rng state (shorthard for accessing element 2 in the tuple)
          val (randomInt, nextRng) = acc._2.nextInt
          //We append the randomInt to the list (acc._1), and pass the new rng state
          loop(count - 1, (randomInt :: acc._1, nextRng))

      loop(size, (List(), rng))
  


  /*
  Exercise 8. Implement flatMap, and then use it to implement nonNegativeLessThan.9
def flatMap[A,B](f: Rand[A])(g: A =>Rand[B]): Rand[B]
Note: for Option, we used map to compose a partial computation with a total computation. In here
we used map to compose a random generator with a deterministic function. Similarly for flatMap.
Function, Option.flatMap was used to compose two partial computations. On Rand the flatMap
function is used to compose two random generators.*/

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
      //Ensures we are always below the bound, by using modulo
      val mod = nonNegInt % bound
      if nonNegInt + (bound - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(bound)
    }



end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  /*
  Exercise 9. Read Section 6.5 before solving this exercise. Observe that everything we have done
so far can just as well be done for other states than RNG. Generalize the functions unit, map, map2,
flatMap, and sequence. Note that this exercise is split into two parts of the .scala file (search for
"Exercise 9" twice).
*/

  def flatMap[B](nextStateFunction: A => State[S, B]): State[S, B] =

    //We return a State(S => (B, S)) 
    State(state => {
      //We run "step" the current state, to produce a value and a new state
      val (value, nextState) = run(state)
      //The value is then used by our function, which itself returns a new Function S => (B, S)
      nextStateFunction(value).run(nextState)
    })
  
  def map[B](transform: A => B): State[S, B] =
    //We use flatMap to transform the value
    // We use transform of each element to convert it from A to B
    //Each value B uses Unit to convert the value to a State of S => (B, S)
    //We return a State(S => (B, S))
    flatMap(value => State.unit(transform(value)))

  def map2[B, C](stateB: State[S, B])(combine: (A, B) => C): State[S, C] = 
    //We use flatMap to combine the two states
    //ValueA is the value from the current state
    //ValueB is the value from the stateB
    //We use combine to combine the two values
    flatMap(valueA => stateB.map(valueB => combine(valueA, valueB)))




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

    //Init acc is a State that returns a List of A and a new state that
    //Unit is a function that takes a value and returns a State of S => (List[A], S),
    //the value given for unit is the empty list.
    val initialAccumulator: State[S, List[A]] = State.unit[S, List[A]](List())

    //The folder takes a stateFunction and a combinedState
    //it uses map2 to combine the stateFunction with the combinedState
    //and the lambda function is used to combine the two values
    def folder(stateFunction: State[S, A], combinedState: State[S, List[A]]): State[S, List[A]] = {
      stateFunction.map2(combinedState)(_ :: _)
    }

    //So we have a list of States, and we want to combine them into a single state with a list of the elements
    //We use foldRight to combine the states from right to left
    stateFunctions.foldRight(initialAccumulator)(folder)



  import adpro.lazyList.LazyList

  /*
  Exercise 10. We now connect the State and LazyLists. Recall from basics of computer science that
automata and traces are intimately related: each automaton generates a language of traces. In our implementation
automata are implemented using State and LazyLists can be used to represent traces.
Implement a function stateToLazyList that given a state object and an initial state, produces a
lazyList of values generated by this State object (this automaton).*/
  
  def stateToLazyList[S, A](stateFunction: State[S, A])(initialState: S): LazyList[A] = {

    //Folder recursively generates the LazyList
    def folder(currentState: S): LazyList[A] = {
      //For each state, we use run "step" to produce a value and a new state
      val (value, nextState) = stateFunction.run(currentState)
      //We concat the value, and call the function with the next state.
      //That way we keep generating values infinitely, and produce a new state.
      LazyList.cons(value, folder(nextState))
    }

    // Start generating the LazyList from the initial state
    // and simply return the result.
    folder(initialState)
  }

  


  /*
  Exercise 11. Use stateToLazyList to generate a lazy stream of random integer numbers. Finally,
obtain a finite list of 10 random values from this stream.
Notice, how concise is the expression to obtain 10 random values from a generator using streams.
This is because all our abstractions compose very well.
*/
  
  def lazyInts(rng: RNG): LazyList[Int] =
    //Statefunction takes a RNG and returns a tuple of Int and RNG
    //State(_.nextInt) is a function that takes a RNG and returns a tuple of Int and RNG
    val stateFunction: State[RNG, Int] = State(_.nextInt)
    stateToLazyList(stateFunction)(rng)
  

  lazy val tenStrictInts: List[Int] =
    //We take the first 10 elements from the LazyList
    //We use the RNG.SimpleRNG(42) as the initial state
    //tolist forces it to do calculations. 
    lazyInts(RNG.SimpleRNG(42)).take(10).toList


end State
