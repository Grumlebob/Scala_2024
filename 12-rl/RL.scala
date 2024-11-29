
// QLearning.scala
// Implementation of Q-Learning Algorithm for Reinforcement Learning
// Includes an example application to the Cliff Walking problem with adjustable grid selection

import scala.util.Random
import scala.collection.immutable.Map

// Q-Learning Algorithm
class QLearning[S, A](
    alpha: Double,       // Learning rate: determines how quickly the agent learns
    epsilon: Double,     // Exploration rate: probability of taking a random action
    gamma: Double = 1.0  // Discount rate: weight of future rewards (default 1.0 for undiscounted problems)
) {
  // Q-Table representation as a map of states to a map of actions and their Q-values
  private var qTable: Map[S, Map[A, Double]] = Map()

  // Initialize Q-Table with all Q-values set to 0.0
  def initialize(states: Iterable[S], actions: Iterable[A]): Unit = {
    qTable = states.map(s => s -> actions.map(a => a -> 0.0).toMap).toMap
  }

  // Epsilon-greedy action selection: balances exploration and exploitation
  def selectAction(state: S): A = {
    val actions = qTable.getOrElse(state, Map())
    if (Random.nextDouble() < epsilon) {
      Random.shuffle(actions.keys.toList).head // Take a random action
    } else {
      actions.maxBy(_._2)._1 // Take the action with the highest Q-value
    }
  }

  // Update the Q-value for a state-action pair using the Q-Learning formula
  def update(state: S, action: A, reward: Double, nextState: S): Unit = {
    val maxNextQ = qTable.getOrElse(nextState, Map()).values.maxOption.getOrElse(0.0)
    val currentQ = qTable.getOrElse(state, Map()).getOrElse(action, 0.0)
    val updatedQ = currentQ + alpha * (reward + gamma * maxNextQ - currentQ)
    //println(s"Updating Q($state, $action): Current Q-value: $currentQ, Updated Q-value: $updatedQ")
    qTable = qTable.updated(state, qTable(state).updated(action, updatedQ))
  }

  // Train the agent for a single episode, starting from an initial state and ending in a terminal state
  def trainEpisode(
      initialState: S,
      terminalState: S,
      transition: (S, A) => (S, Double) // Transition function: defines state dynamics and rewards
  ): Unit = {
    //println("Training phase: Iterating through one episode")
    var state = initialState
    while (state != terminalState) {
      val action = selectAction(state) // Choose an action using epsilon-greedy policy
      val (nextState, reward) = transition(state, action) // Get the next state and reward
      //println(s"Transitioning: State=$state, Action=$action, NextState=$nextState, Reward=$reward")
      update(state, action, reward, nextState) // Update Q-value using Temporal Difference
      state = nextState // Move to the next state
    }
  }

  // Extract the learned policy as a map from states to the best action in each state
  def getPolicy: Map[S, A] = {
    qTable.map { case (state, actions) =>
      state -> actions.maxBy(_._2)._1
    }
  }

  // Print the Q-Table
  def printQTable(): Unit = {
    println("Q-Table: Each state shows Q-values for all actions")
    qTable.foreach { case (state, actions) =>
      println(s"State: $state, Actions: $actions")
    }
  }
}

// Example: Cliff Walking Problem
object CliffWalking {
  type State = (Int, Int) // State is represented as a (row, column) coordinate on the grid
  type Action = String    // Actions are represented as strings: "Up", "Down", "Left", "Right"

  // Define the dimensions of the grid
  val rows = 4
  val cols = 12

  // Define the possible actions
  val actions: List[Action] = List("Up", "Down", "Left", "Right")

  // Define the step function: calculates the next state and reward based on the current state and action
  def step(state: State, action: Action): (State, Double) = {
    val (row, col) = state
    val nextState = action match {
      case "Up"    => (math.max(row - 1, 0), col)           // Move up, bounded by grid edges
      case "Down"  => (math.min(row + 1, rows - 1), col)    // Move down, bounded by grid edges
      case "Left"  => (row, math.max(col - 1, 0))           // Move left, bounded by grid edges
      case "Right" => (row, math.min(col + 1, cols - 1))    // Move right, bounded by grid edges
    }

    if (nextState == (rows - 1, cols - 1)) (nextState, 0.0) // Goal state: reward 0
    else if (row == rows - 1 && col > 0 && col < cols - 1) ((0, 0), -100.0) // Cliff: large negative reward
    else (nextState, -1.0) // Default reward for other transitions
  }

  // Print the environment grid (standard or valid random)
  def printEnvironment(useStandardGrid: Boolean): Unit = {
    println("Environment Grid:")
    val randomCliffs = Random.shuffle((1 until cols by 2).toList).take(math.min(rows - 2, cols / 2)) // Ensure path exists
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        if (useStandardGrid) {
          if (r == 3 && c == 0) print("S	") // Start position
          else if (r == 3 && c == 11) print("G	") // Goal position
          else if (r == 3 && c > 0 && c < 11) print("C	") // Cliff positions
          else print(".	") // Regular positions
        } else {
          if (r == 3 && c == 0) print("S	") // Start position
          else if (r == 3 && c == 11) print("G	") // Goal position
          else if (r == 3 && randomCliffs.contains(c)) print("C	") // Valid random cliff
          else print(".	") // Regular positions
        }
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    // Adjustable Parameters
    val alpha = 0.1       // Learning rate
    val epsilon = 0.1     // Exploration rate
    val gamma = 1.0       // Discount factor
    val episodes = 400    // Number of training episodes
    val useStandardGrid = false // Set to false to use a valid random grid

    printEnvironment(useStandardGrid) // Show the environment grid

    val qLearning = new QLearning[State, Action](alpha, epsilon, gamma)
    val states = for (r <- 0 until rows; c <- 0 until cols) yield (r, c) // All possible states on the grid

    qLearning.initialize(states, actions) // Initialize Q-table with all Q-values set to 0.0

    // Train the agent
    println(s"Training the agent: α=$alpha, ε=$epsilon, γ=$gamma, N=$episodes")
    for (_ <- 1 to episodes) {
      qLearning.trainEpisode((3, 0), (3, 11), step) // Use the step function for state transitions
    }

    // Print the Q-table
    qLearning.printQTable()

    // Extract and print the learned policy
    println(s"Optimal Policy (a=$alpha, e=$epsilon, y=$gamma, N=$episodes):")
    val policy = qLearning.getPolicy
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        val action = policy.getOrElse((r, c), ".") // Default action for undefined states
        print(s"$action	")
      }
      println()
    }
  }
}
