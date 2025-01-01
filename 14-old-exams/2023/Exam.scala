//> using lib "org.scalacheck::scalacheck:1.17.0"
//> using lib "org.scalactic::scalactic:3.2.17"
//> using lib "org.typelevel::spire:0.18.0"

/* Final Exam: Advanced Programming, by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2023: 05 January 2024
 *
 * The exam consists of 9 questions to be solved within 4 hours.
 * Solve the tasks in the file 'Exam.scala' (this file).
 *
 * You can use all functions provided in the included files,  as well
 * as functions we implemented in the course (if the source is missing
 * in this folder, you can add it to this file, so that things compile)
 *
 * You can access any static written aids, also online, but you are
 * not allowed to communicate with anybody or with anything (bots).
 * Using GitHub copilot ChatGPT and similar large language models
 * during the exam is not allowed. By submitting you legally declare to
 * have solved the problems alone, without communicating with anybody.
 *
 * Do not modify this file in other ways than answering the questions
 * (adding imports is allowed). Do not reorder the answers, and do not
 * remove question numbers or comments from the file. 
 *
 * Submit this file and only this file to LearnIT. Do not convert to
 * any other format than .scala. Do not submit the entire zip archive.
 * The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * The answers will be graded manually. We focus on the correctness of
 * ideas, the use of concepts, clarity, and style. We will use
 * undisclosed automatic tests during grading, but not to compute the
 * final grade, but to help us debug your code.
 *
 * We do require that the file compiles.  The directory has a project
 * setup so compilation with scala-cli shall work out-of-the-box.
 * If you cannot make a fragment compile, put your solution in a
 * comment, next to the three question marks. We will grade the
 * solutions in comments as well.
 *
 * Files that do not compile will automatically fail the exam. 
 *
 * The size of the exam has been adjusted from previous years so that
 * you have time to work with the compiler. We do not recommend to run
 * and test the code if you are pressed for time. It is a good idea to
 * run and test, if you have time. 
 *
 * Good luck! */

package adpro

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.TripleEquals.*

import adpro.laziness.LazyList
import adpro.state.*

object Streaming: 

  /* QUESTION 1 ######################################################
   * Study the following recursive function. Then implement the same
   * semantics using a fold.
   */

  /**
   * Recursive Version Explanation:
   * This function counts the number of odd integers in a LazyList.
   * It uses tail recursion to process each element.
   */
  
  def fViaRec(l: LazyList[Int]): Int = 
    def doRec(l: LazyList[Int], z: Int): Int = 
      l match 
        case LazyList.Cons(hd, tl) => // Match LazyList head and tail
          if hd() % 2 == 1 // Check if the head is odd
          then doRec(tl(), z + 1) // Increment count and recurse
          else doRec(tl(), z) // Recurse without incrementing
        case LazyList.Empty => z // Return accumulated count when list is empty

    doRec(l, 0) // Start recursion with initial count 0

  /**
   * Fold Version Explanation:
   * Achieves the same result using foldLeft.
   * Each element increments the accumulator if it is odd.
   */
  def fViaFold(l: LazyList[Int]): Int = 
    val initAcc = 0 // Define the initial accumulator value
    def Folder(acc: Int, a: Int): Int = // Define the folder function
      if a % 2 == 1 then acc + 1 // Increment accumulator if `a` is odd
      else acc // Keep accumulator unchanged if `a` is even
    l.foldLeft(initAcc)(Folder) // Apply foldLeft with initial value and folder function

  def fViaFoldRight(l: LazyList[Int]): Int = 
    val initAcc = 0 // Define the initial accumulator value
    def Folder(a: Int, acc: => Int): Int = // Define the folder function with lazy accumulator
      if a % 2 == 1 then acc + 1 // Increment accumulator if `a` is odd
      else acc // Keep accumulator unchanged if `a` is even
    l.foldRight(initAcc)(Folder) // Apply foldRight with initial value and folder function

end Streaming



object Parsing:

  import adpro.parsing.*
  import adpro.parsing.Sliceable.*

  /* QUESTION 2 ######################################################
   * The following parser parses CSV (comma separated values) input
   * containing of integer numbers (and  no other types). The type of
   * values produced by a successful parse is List[List[Int]].
   *
   * Example input:
   * 1,2,3 , 5,4
   * 42,42
   *
   * Example output: 
   * List(List(1,2,3,5,4), List(42,42))
   *
   * Use this to write a parser `longestLine` of type Parser[Int] that
   * returns the maximum number of columns in the longest line in the
   * input, measured by the number of integers in the line. The parser
   * should produce Right(5) for the above example.
   */

  val WS: Parser[String] = regex("""[ \t]+""".r) // Match whitespace
  val NL: Parser[String] = string("\n") // Match newline character
  val INT: Parser[Int] = regex("""(\+|-)?[0-9]+""".r).map(_.toInt) // Match integers and map to Int

  val commaSeparatedInts: Parser[List[Int]] =
    { WS.? |* INT *| WS.? ** (string(",") |* WS.? |* INT *| WS.?).* }
      .map { (h,t) => h::t }

  lazy val parser: Parser[List[List[Int]]] =
    { commaSeparatedInts ** { ( NL |* commaSeparatedInts ) }.* }
      .map { (h,t) => h::t }

        /**
   * longestLine Parser Explanation:
   * Parses multiple lines of integers and finds the length of the longest line.
   * Example Input:
   * List(List(1,2,3,5,4), List(42,42))
   *
   * Step 1: parser.map(lli => ...)
   * - Input: List(List(1,2,3,5,4), List(42,42))
   * - lli = List(List(1,2,3,5,4), List(42,42))
   *
   * Step 2: lli.map(_.length)
   * - First list length: 5
   * - Second list length: 2
   * - Result: List(5,2)
   *
   * Step 3: .max
   * - Find maximum in List(5,2)
   * - Result: 5
   *
   * Final Output: 5
   */
  lazy val longestLine: Parser[Int] =
    parser.map(lli => // Map parsed lines to their lengths
      lli.map(_.length).max) // Find maximum length among lines


  /* QUESTION 3 ######################################################
   * Implement a parser of type Parser[Boolean] that parses a CSV
   * input and returns true if all lines have the same number of
   * elements.  It should return Right(False) for the above example.
   *
   * NB. This question does not require that you answered QUESTION 2.
   */

  /**
   * allLinesTheSame Parser Explanation:
   * Checks if all lines have the same number of integers.
   * Example Input:
   * List(List(1,2,3,5,4), List(42,42))
   *
   * Step 1: parser.map(lli => ...)
   * - Input: List(List(1,2,3,5,4), List(42,42))
   * - lli = List(List(1,2,3,5,4), List(42,42))
   *
   * Step 2: lli.map(_.length)
   * - First list length: 5
   * - Second list length: 2
   * - Result: List(5,2)
   *
   * Step 3: lengths.distinct
   * - Remove duplicate lengths
   * - Result: List(5,2)
   *
   * Step 4: lengths.distinct.length == 1
   * - Check if there is only one unique length
   * - Result: false
   *
   * Final Output: false
   */
  val allLinesTheSame: Parser[Boolean] = 
    parser.map(lli => // Map parsed lines
      lli.map(_.length)) // Get lengths of each line
      .map { lengths => lengths.distinct.length == 1 } // Check if all lengths are equal

  /**
   * _allLinesTheSame Parser Explanation:
   * Alternate implementation using foldLeft.
   * Example Input:
   * List(List(1,2,3,5,4), List(42,42))
   *
   * Step 1: parser.map(lli => ...)
   * - Input: List(List(1,2,3,5,4), List(42,42))
   * - lli = List(List(1,2,3,5,4), List(42,42))
   *
   * Step 2: lli.map(_.length)
   * - Result: List(5,2)
   *
   * Step 3: .foldLeft((-1, true))((acc, a) => ...)
   * - Initial accumulator: (-1, true)
   * - First iteration: (5, true)
   * - Second iteration: (2, false)
   *
   * Step 4: .map(t => t._2)
   * - Extract second value from accumulator
   * - Result: false
   *
   * Final Output: false
   */
  val _allLinesTheSame: Parser[Boolean] = 
    parser.map(lli => lli.map(_.length))
      .map(li => li.foldLeft((-1, true))((acc, a) =>
        if acc._1 > 0 then (a, acc._2 && (acc._1 == a)) 
        else (a, true)))
      .map(t => t._2)

end Parsing



object Game:

  import pigaro.*

  /* QUESTION 4 ######################################################
   * Consider the old Chinese game of rock, paper, and scissors.  Two
   * players independently and simultaneously select a move.  The
   * possible moves are Rock, Paper, and Scissors.  The winning is
   * established using the following rules:
   *
   * Rock wins against Scissors
   * Paper wins against Rock
   * Scissors win against Paper
   * Otherwise there is a draw.
   *
   * The function `winner` encodes the rules of the game, i.e. which
   * player wins in each pair of moves. `None` represents a draw.
   * Continue reading below.
   */

  enum Move:
    case Rock, Paper, Scissors

  enum Player: 
    case P1, P2

  import Move.*, Player.*

  type Result = Option[Player]

  def winner(player1: Move, player2: Move): Result = (player1, player2) match
    case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Some(P1)
    case (Scissors, Rock) | (Rock, Paper) | (Paper, Scissors) => Some(P2)
    case _ => None

  /* A strategy is a distribution over the next move, so Dist[Move].
   * Create two strategies, one for Alice and one for Bob:
   * a) Alice picks Rock, Paper, and Scissors with uniform probability.
   * b) Bob Picks Rock and Paper with probability 0.5 each, and never
   * picks Scissors.
   *
   * Link to relevant documentation 
   * https://www.itu.dk/people/wasowski/ula/scala-doc/index.html */

  type Strategy = Dist[Move]

  lazy val Alice: Strategy =
    Pigaro.uniform("alice winner")(Rock, Paper, Scissors)

  lazy val Bob: Strategy =
    Pigaro.uniform("bob winner")(Rock, Paper)


  /* QUESTION 5 ######################################################
   * Implement a function that computes a result of a game given two
   * probabilistic strategies `player1` and `player2`. The function
   * shall give a probability of winning for each player, and a
   * probability of a draw as a Dist[Result].   You can use the
   * function 'winner' from above. 
   *
   * Answering QUESTION 4 is not required to answer this one.
   */


  /**
   * Game function explanation:
   * Computes the probability distribution of game outcomes given two strategies.
   * Example:
   * - player1 chooses moves with probabilities (Rock: 0.33, Paper: 0.33, Scissors: 0.33)
   * - player2 chooses moves with probabilities (Rock: 0.5, Paper: 0.5)
   */
  def game(player1: Strategy, player2: Strategy): Dist[Result] =
    player1.map2(player2)(winner) // Combine strategies using `winner` function

  /**
   * Alternate implementation using map:
   * Step 1: Pair the strategies using tuple (player1 -> player2)
   * Step 2: Apply `.map` to extract move pairs
   * Step 3: Use `winner` to determine the outcome
   * Step 4: Return a probability distribution of results
   * Example Step-by-Step:
   * - Input: (Rock, Paper)
   * - Winner: Player 2
   */
  def _game(player1: Strategy, player2: Strategy): Dist[Result] =
    (player1 -> player2) // Step 1: Pair player strategies
      .map { case (p1, p2) => // Step 2: Extract move pair
        winner(p1, p2) // Step 3: Apply winner logic
      } // Step 4: Map results into a distribution


  /* QUESTION 6 ######################################################
   * Obtain a sample of 10000 outcomes and  estimate the  probability
   * that Alice is winning when playing against  Bob.  You do not have
   * to report the number, just show the code that computes it.
   *
   * The correct answer will compile even if you leave QUESTIONS 4-5 
   * unanswered.
   */
  
  given rng: spire.random.rng.SecureJava 
    = spire.random.rng.SecureJava.apply

  lazy val aliceFraction: Double = 
    game(Alice, Bob).sample(10000)
    .prMatching{ 
      case Alice => }
  
  /**
   * Explanation of aliceFraction:
   * Computes the probability that Alice wins in 10,000 simulated games.
   *
   * Step 1: game(Alice, Bob) generates a probability distribution over results.
   * Step 2: sample(10000) runs 10,000 simulations based on the distribution.
   * Step 3: prMatching extracts the probability of Alice winning.
   */
  lazy val _aliceFraction: Double =
    game(Alice, Bob) // Step 1: Generate game result distribution
      .sample(10000) // Step 2: Sample 10,000 outcomes
      .prMatching { // Step 3: Calculate probability of Alice winning
        case Some(Player.P1) => true
      }

  /**
   * Explanation of _aliceFraction:
   * Alternate implementation to compute Alice's winning probability.
   *
   * Step 1: game(Alice, Bob) generates a probability distribution over results.
   * Step 2: sample(10000) runs 10,000 simulations.
   * Step 3: pr calculates the probability of a specific outcome.
   */
  lazy val __aliceFraction: Double =
    game(Alice, Bob) // Step 1: Generate game result distribution
      .sample(10000) // Step 2: Sample 10,000 outcomes
      .pr(Some(Player.P1)) // Step 3: Calculate probability of Alice winning
    
end Game



object RL: 

  /* QUESTION 7 ######################################################
   * The type Q below represents Q-Tables in reinforcement learning,
   * so mapping states to actions to expected rewards. The function
   * update below produces a new q-table which has been modified for
   * the given state and action, using a new value of rewards and
   * estimate.  The details of how this update happens and how it is 
   * used in learning are not relevant for us here though.
   *
   * Our goal is to test the update function, and we will be testing
   * it on the State=Int, Action=Int case. We only test an update for
   * a state and action for which the Q-table q is defined.
   *
   * The type of the specialized update is written below as
   * `IntUpdate` for convenience.  Continue reading below.
   * */

  type Q[State, Action] = Map[State, Map[Action, Double]]

  val α = 0.5 // Learning rate
  val γ = 1.0 // Discount factor

/**
   * Updates the Q-table for a specific state and action.
   */
  def update[State, Action](q: Q[State, Action], state: State, action: Action)
    (reward: Double, estimate: Double): Q[State, Action] =
    val qsa = q(state)(action) // Retrieve current Q-value for state-action pair
    val rewardComponent = reward + γ * estimate // Calculate reward component
    val adjustedValue = α * rewardComponent // Scale reward component by learning rate
    val newValue = (1.0 - α) * qsa + adjustedValue // Blend old and new values
    val updatedActionMap = q(state) + (action -> newValue) // Update action map with new value
    val updatedQTable = q + (state -> updatedActionMap) // Replace old state map with updated one
    updatedQTable // Return updated Q-table


  type IntUpdate = 
    (Q[Int, Int], Int, Int) => (Double, Double) => Q[Int, Int]

  /* We need test data. We will test on q-tables initialized with zero
   * reward values for all actions and states.  The function qZero
   * below fills a Q-table of size nStates by nActions with zeroes.
   * States are counted from 0 to nStates-1 inclusively, and actions
   * are counted from 0 to nActions-1 inclusively.
   *
   * Continue reading below.
   */

  /**
   * Initializes a Q-table with zero values for all states and actions.
   */
  def qZero(nStates: Int, nActions: Int): Q[Int, Int] =
    val zeroValue = 0.0 // Define initial zero value for all actions
    val actionMap = (0 until nActions).map(a => a -> zeroValue).toMap // Create zeroed action map
    val stateActionMap = (0 until nStates).map(s => s -> actionMap).toMap // Create zeroed state-action map
    stateActionMap // Return initialized Q-table

  /* We will also test on randomly initialized qTables, which are
   * created using the Scalacheck generator below.
   * Continue reading below.
   */

  val genRewards = Gen.choose(-100, 100).map(_.toDouble)
  def qGen(nStates: Int, nActions: Int): Gen[Q[Int, Int]] = for 
    av <- Gen.listOfN[Double](nActions, genRewards)
    av1 = av.zipWithIndex
            .map(_.swap)
            .toMap
    sav = List.fill(nStates)(av1)
              .zipWithIndex
              .map(_.swap)
              .toMap
  yield sav


  abstract class NullUpdatesSpec(update: IntUpdate, name: String) 
    extends org.scalacheck.Properties(name): 

    /* Write a **scenario** test that updates a zero-initialized 2 x 3
     * q-table (States in 0,1; Actions in 0,1,2) on the position
     * (0)(0) with arguments (reward=0.0, estimate=0.0). The test
     * should check that the obtained q-table is still the same as the
     * input table (such operation does nothing interesting).
     */

    property("00 Null update on null table 2x3") = 
      update(qZero(2, 3), 0, 0)(0.0, 0.0) == qZero(2, 3)



    /* QUESTION 8 ####################################################
     * Write a **property** test that runs on randomly initialized
     * Q-Tables of size 2 x 3. The update should be performed on any
     * position (for any state, and any action within the range
     * defined in the Q-table), not only at (0)(0) as above. 
     *
     * The `reward` argument should get the value stored in the input
     * Q-table on the updated position. The estimate argument should
     * still be zero.
     *
     * Check that the resulting table under these conditions is the
     * same as the input table.
     */

    property("01 Null update on null table 2x3") = 
      forAllNoShrink(qGen(2,3)) { q => 
        val m = q(0).size
        val n = q.size
        forAll(Gen.choose(0, n-1), Gen.choose(0, m-1)) { (s, a) =>
          val qU = update(q,s,a)(q(s)(a), 0.0)
          qU ?= q
        }
      }

  end NullUpdatesSpec

end RL
