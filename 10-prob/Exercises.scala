// Advanced Programming, Andrzej Wasowski
// Probabilistic Programming (AKA Probability is also a monad)
package adpro.prob

import pigaro.*

// Used by samplers
given rng: spire.random.rng.SecureJava 
  = spire.random.rng.SecureJava.apply

// All the exercise text is included below in comments (no PDF file
// this week).

// Probabilistic programs are hard to test, so the tests we have are a
// bit weak. Also note, that more exercises you solve the slower the
// test suite becomes, due to many samplings it runs. You may want to
// disable tests temporarily to speed up work.

val M = 42

// For many exercises, you are (quietely) expected to run the queries
// in the REPL in order to inspect probability values (or to print
// them to the standard output).
//
// Hand in the completed file (this file, Exercises.scala). No zip
// files, no pdfs, etc.
//
// The main inspiration for this exercise comes from the material of a
// probabilistic programming course by prof. Joost-Pieter Katoen at
// RWTH Aachen, Germany.

// Before starting to solve the exercises below, please study the file
// Basic.sc, side-by-side with this week's slides. Once you
// understood the code in Basic.sc, come back here.

// Peter and Paula play a game.  An urn contains some black balls and
// a single red ball. They take turns taking one random ball out of
// the urn. The first one to pick the red ball wins.
//
// We model the players using, for instance, an enum:

enum Player:
  case Peter, Paula 

import Player.*

// And we add a function to determine the next player (a simple alternation):

def next(player: Player): Player = player match
  case Peter => Paula
  case Paula => Peter

// The number of balls, including exactly 1 red ball, found in the run at the
// begining of the game

val BallsNo: Int = 8

// Exercise 1.
//
// Write a function pick that given the number 'n' of black balls in
// the urn returns a probability distribution that generates Red with
// the probability of picking the red ball. There is always one red
// inside the urn, when a move is made.  Use the following constructor
// 'Pigaro.bernoulli' to implement 'pick'.

// bernoulli[U] (probability: Double, success: U, failure: U): Dist[U]

enum Ball: 
  case Red, Black
import Ball.*

def pick(n: Int): Dist[Ball] = {
  val probabilityOfRed = 1.0 / (n + 1)
  Pigaro.bernoulli(probabilityOfRed, Red, Black)
}

//  Exercise 2. 
//
//  Write a function 'move' that given the starting player and the
//  number of black balls 'n' present in the urn returns the
//  probability distribution defining which player wins. 
//
// Hint: Andrzej's solution used 'pick' and 'Dirac'. 
// 
// Dirac[A](a: A): Dist[A]
//
// This constructor returns a distribution where the value 'a' has
// probability '1'.  Dirac is basically a constant probability
// distribution - only one outcome is legal. This is the monadit
// const/unit/pure.


def move(player: Player, n: Int): Dist[Player] = {
  //Base Case: If there are no black balls left (n == 0), the function returns Dirac(player), indicating the current player wins.
  if (n == 0) Dirac(player)
  else {
    pick(n).flatMap {
      //If Red, it returns a distribution where the current player wins.
      case Red => Dirac(player)
      //If Black, it recursively calls move with the next player and one less black ball.
      case Black => move(next(player), n - 1)
    }
  }
}

// Exercise 3.
//
// Peter is polite and offers a choice to Paula, if she wants to start or
// rather had that he started.
//
// Use the function 'move' to estimate the chance of Paula winning
// when she starts, and when Peter starts, if the urn contains 'BallsNo' balls
// in total (including one red).   
//
// To calculate probability, first use d.sample(N) for some object of
// type Dist[...] to obtain a sample of N elements (type IData), and
// then use IData's method .pr to calculate the probability.  The
// latter takes a value from the distribution range. It returns the
// estimate of probability that the distribution takes this value.
//
// IData[A].pr(value: A): Double
val N = 10000  // Sample size for probability estimation

// Probability that Paula wins given Paula starts (the total no of balls: BallsNo)
def probPaulaStarts: Double = {
  val paulaStartsDist = move(Paula, BallsNo - 1)
  val sampleData = paulaStartsDist.sample(N)
  sampleData.pr(Paula)
}

// Probability that Paula wins given Peter starts (the total no of balls: BallsNo)
def probPeterStarts: Double = {
  val peterStartsDist = move(Peter, BallsNo - 1)
  val sampleData = peterStartsDist.sample(N)
  //Condition: sampleData.pr(Paula) calculates the probability of Paula winning based on who started.
  sampleData.pr(Paula)
}

//  Which strategy is beter for Paula? What if BallsNo == 9? 
// If BallsNo == 8, Paula has a slight advantage if she starts, because
// the odds slightly favor the player who starts when the total number of
// balls is even. If BallsNo == 9, then Peter would have a slight advantage,
// as the odds slightly favor the starting player when the total number of
// balls is odd.


// Exercise 4.
//
// A quick pen-and-pencil question: Can you estimate the size of
// the Bayesian network generated by 'move (p, 10)' for some player constant p?
//
// Observe, that this model would be very annoying and laborious to build
// manually on paper, but with statistical interpretation in a programming
// framework we can build models for 200 balls easily.  This is probably the
// main strength of probabilistic programming.
//
// You do not need to write the answer to this question for grading.
// Use it yourself to appreciate the power of the probabilistic programming
// tool).

// Exercise 5.

// We know that Paula has won the game.  What is the probability that she has
// started the game?  Use MAP (maximum posterior probability), assuming that
// it was initially equally likely that Peter and Paula are starting.
//
// This exercise is split in a number of smaller steps.  You should try to get
// an overview of the entire constructed model.
//
// We first create a uniform prior for the first mover:

lazy val firstMover: Dist[Player] = Pigaro.uniform("firstMover")(Peter, Paula)

// Now create a nullary function 'gameResult' that picks the first mover
// randomly using 'firstMover' and then returns the probability distribution
// for a game played with BallsNo balls in the urn. We want to keep
// both the first mover and the winner in the model, so that we can
// reason about who has started under the condition of who has won.
// Thus the type Dist[(Player, Player)] seems appropriate.
//
// The _flatMap function, or its domain-specific synonym probDep, may
// prove useful. 

def gameResult: Dist[(Player, Player)] = {
  firstMover.flatMap { first =>
    move(first, BallsNo - 1).map { winner => (first, winner) }
  }
}

// What is the probability that Paula wins with this uniform prior? Does it
// agree with your intuition? Write the answer in a comment:

// Answer: The probability that Paula wins with a uniform prior should be
// approximately equal to the probability we calculated in Exercise 3, based on
// whether Paula or Peter started, and averaging them since both start equally.

// Now we are going to make the observation that Paula wins. 

lazy val gameWonByPaula: Dist[(Player, Player)] = 
  gameResult.matching { case (_,Paula) => }

// Calculate the probability that Paula started given that she won.
// You will need to sample and use IData's .pr or .prMatching
// methods.

lazy val probPaulaStarted: Double = {
  val sampleData = gameWonByPaula.sample(N)
  sampleData.prMatching { case (Paula, _) => true }
}

// Does this probability depend on the number of balls in the urn in the
// urn being even or odd? What if it is even? What if it is odd?
//
// Answer: Yes, the probability depends on whether the number of balls is even or odd.
// If the number is even, the likelihood of either player winning is balanced. If the
// number is odd, the starting player has a slight advantage, so if Paula won, it would
// imply a slightly higher chance she started in cases with an odd number of balls.


// Exercise 6.
//
// We know that winning player wins approximately 1/2 games when she
// starts, and we know now that if there is an even number of balls
// in the urn then the probability is precisely equal for both players, while
// if the number of balls is odd the probability of the first player winning
// is slightly higher.
//
// In this exercise, we assume that the number of balls is unknown, but it is
// taken from range 1 to 6 with uniform probability (uniform prior) and we
// will observe that Player1 has won.  We will ask what is the probability
// that the urn held an odd number of balls in the beginning of the game.  We
// expect this probability to be slightly higher than 50%, as player 1 winning
// makes as believe slightly that an odd number of balls are in the urn.

// Let UpperBound will be the maximum number of balls in the urn that we
// consider.

val UpperBound = 6

// Construct a uniform prior on the number of black balls in the urn
// from zero to UpperBound - 1.

// Use the Pigaro.uniform[A] constructor (a variadic function that takes all the
// equally like values of A as its variable size argument list):

// Pigaro.uniform[A](name: String)(a : A*) :Element[A]

lazy val blackBallsNo: Dist[Int] = Pigaro.uniform("blackBallsNo")((0 until UpperBound)*)


// Now convert the prior distribution on the initial number of black balls in
// the urn, into a distribution over the winning player.  Since the game is
// entirely symmetric, we can assume that Paula is starting (the result for
// Peter will be the same). Hint: flatMap or probDep

// There is no test for this step of the computation.

def outcome: Dist[(Int, Player)] = {
  blackBallsNo.flatMap { n =>
    move(Paula, n).map(winner => (n + 1, winner))  // `n + 1` accounts for the red ball
  }
}

// The following asserts that Paula has won.

lazy val paulaWon: Dist[(Int, Player)] = 
  outcome.matching { case (_, Paula) => }

// Now define the posterior probabilities for all size of the urn from 1 to
// UpperBound. You can do this using IData.pr.
// We need a slightly different version of the former that takes a predicate,
// not a concrete value (or the pattern match version):

// IData[T].pr (p: T => Boolean)
// IData[T].prMatching  { case ... => }

lazy val posteriorOdd: Double = {
  val sampleData = paulaWon.sample(N)
  sampleData.pr { case (numBalls, _) => numBalls % 2 != 0 }
}

// Is the posteriorOdd greater than 1/2? Why?
//
// Answer: Yes, the posteriorOdd is expected to be slightly greater than 1/2 because,
// given that Paula won, it is more likely that the starting player had a slight advantage.
// This slight advantage exists when there is an odd number of balls, hence making it
// slightly more probable that the urn had an odd number of balls.


// Reflect whether the above estimation would take you more time analytically
// or with a probabilistic programming library?
/* 
Using a probabilistic programming library significantly reduces the time and complexity needed for this estimation compared to solving it analytically. With the library:

Complexity Reduction: The library abstracts the intricacies of probability distributions, allowing us to model complex, sequential probabilistic events (like drawing balls and switching players) without manually calculating probabilities at each step.

Sampling and Inference: Instead of calculating exact probabilities by hand (which becomes challenging with larger numbers of balls or more complex conditions), the library handles sampling, Monte Carlo estimation, and posterior updates with minimal code, enabling efficient exploration of possible outcomes.

Scalability: Analytically, adding more conditions or increasing the ball count would increase the manual computation exponentially, while with the library, we simply adjust parameters and resample.

In summary, a probabilistic programming library makes these estimations faster, more accurate, and easier to adjust for different scenarios, especially as complexity grows.
*/

