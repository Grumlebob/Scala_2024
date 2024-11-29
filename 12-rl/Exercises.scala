package adpro.lazyList

@main def runAllExercises(): Unit =
  def printSeparator(exerciseNumber: String): Unit =
    println(s"\n===== Exercise $exerciseNumber =====")

  // Exercise 1: Create a LazyList of natural numbers starting from 1
  printSeparator("1")
  println("First 10 natural numbers using from:")
