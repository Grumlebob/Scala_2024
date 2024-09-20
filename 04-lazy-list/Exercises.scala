package adpro.lazyList

@main def runAllExercises(): Unit =
  def printSeparator(exerciseNumber: String): Unit =
    println(s"\n===== Exercise $exerciseNumber =====")

  // Exercise 1: Create a LazyList of natural numbers starting from 1
  printSeparator("1")
  val naturals = LazyList.from(1)
  println("First 10 natural numbers using from:")
  naturals.take(10).toList.foreach(println)

  // Exercise 2: Convert LazyList to List
  printSeparator("2")
  val sampleList = LazyList(1, 2, 3, 4, 5)
  println(s"LazyList to List: ${sampleList.toList}")

  // Exercise 3: Take and Drop
  printSeparator("3")
  val taken = naturals.take(10)
  val dropped = naturals.drop(5).take(5)
  println(s"Taking the first 10 elements: ${taken.toList}")
  println(s"Dropping the first 5 elements and taking the next 5: ${dropped.toList}")

  // Exercise 4: takeWhile
  printSeparator("4")
  val takeWhileLessThan10 = naturals.takeWhile(_ < 10)
  println(s"Elements less than 10: ${takeWhileLessThan10.toList}")

  // Exercise 5: forAll and exists
  printSeparator("5")
  println(s"All elements less than 0: ${naturals.forAll(_ < 0)}") // Should be false
  println(s"Exists element equal to 5: ${naturals.exists(_ == 5)}") // Should be true

  // Exercise 6: takeWhile1 using foldRight
  printSeparator("6")
  val takeWhileFoldRight = naturals.takeWhile1(_ < 10)
  println(s"Elements less than 10 using takeWhile1: ${takeWhileFoldRight.toList}")

  // Exercise 7: headOption using foldRight
  printSeparator("7")
  println(s"Head of LazyList (should be Some(1)): ${naturals.headOption1}")

  // Exercise 8: map, filter, append, and flatMap using foldRight
  printSeparator("8")
  println(s"Mapping *2 over first 10 naturals: ${naturals.map(_ * 2).take(10).toList}")
  println(s"Filtering even numbers: ${naturals.filter(_ % 2 == 0).take(10).toList}")
  println(s"Appending naturals to itself and taking 20: ${naturals.take(10).append(naturals).take(20).toList}")
  println(s"FlatMapping to(x) over naturals: ${naturals.flatMap(LazyList.to).take(20).toList}")

  // Exercise 10: Fibonacci using fold
  printSeparator("10")
  println("First 10 Fibonacci numbers using fold:")
  LazyList.fibs.take(10).toList.foreach(println)

  // Exercise 11: Fibonacci using unfold
  printSeparator("11")
  println("First 10 Fibonacci numbers using unfold:")
  LazyList.fibsUnfold.take(10).toList.foreach(println)

  //Playground for fib
  val k = 10
  val left = LazyList.fibs.asInstanceOf[LazyList[Int]].drop(k)
  val right = LazyList.fibsUnfold.asInstanceOf[LazyList[Int]].drop(k)
  println(s"Comparing fibs and fibsUnfold from $k: ${left.take(80).toList == right.take(80).toList}")
  

  // Exercise 13: mapUnfold, takeUnfold, takeWhileUnfold, zipWith
  printSeparator("13")
  println(s"Mapping *2 over first 10 naturals using unfold: ${naturals.mapUnfold(_ * 2).take(10).toList}")
  println(s"Taking 5 elements using unfold: ${naturals.takeUnfold(5).toList}")
  println(s"Taking while elements < 5 using unfold: ${naturals.takeWhileUnfold(_ < 5).toList}")
  println("Zipping naturals with itself using addition:")
  val zipped = naturals.zipWith[Int, Int](_ + _)(naturals).take(10).toList
  zipped.foreach(println)

  println("\n Wuuu-hu all exercises printed!")


enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h,t) => Some(h())

  def tail: LazyList[A] = this match
    case Empty => Empty
    case Cons(h,t) => t()

  /* Note 1. f can return without forcing the tail
   *
   * Note 2. this is not tail recursive (stack-safe) 
   *
   * Note 3. We added the type C to the signature. This allows to start with a
   * seed that is a subtype of what the folded operator returns.
   * This helps the type checker to infer types when the seed is a subtype, for 
   * instance, when we construct a list:
   *
   * o.foldRight (Nil) ((a,z) => a:: z)
   *
   * The above works with this generalized trick. Without the C generalization
   * the compiler infers B to be List[Nothing] (the type of Nil) and reports
   * a conflict with the operator.  Then we need to help it like that:
   *
   * o.foldRight[List[Int]] (Nil) ((a,z) => a:: z)
   *
   * With the C type trick, this is not neccessary. As it hints the type
   * checker to search for generalizations of B.
   *
   * I kept the foldLeft type below in a classic design, so that you can
   * appreciate the difference. Of course, the same trick could've been
   * applied to foldLeft.
   */
  def foldRight[B, C >: B](z: => B)(f: (A, => C) => C): C = this match
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))

  /* Note 1. Eager; cannot be used to work with infinite lazy lists. So
   * foldRight is more useful with lazy lists (somewhat opposite to strict lists)
   * Note 2. Even if f does not force z, foldLeft will continue to recurse.
   */
  def foldLeft[B](z: => B)(f :(A, => B) => B): B = this match
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)

  def find(p: A => Boolean) = 
    this.filter(p).headOption

  // Exercise 2
  def toList: List[A] =
    this match
      case Empty => List.empty[A]
      //So head is appended to tail. The tail is passed recursively to .toList, until it is empty and we append List.Empty
      case Cons(h, t) => h()::t().toList

  // Exercise 3
  //take(n) for returning the first n elements of a LazyList
  def take(n: Int): LazyList[A] = 
    this match
      //While we are still taking, concat head with tail
      case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
      //if n is 0, then concat with empty list
      case _ => Empty

  //drop(n) for skipping the first n elements of a LazyList
  def drop(n: Int): LazyList[A] = 
    this match
      //We keep discarding head, untill n = 0, then we return the rest of the list ('this')
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this

  /*
    Question:
      For fluency, try the following test case in the REPL (should terminate with no memory exceptions
      and very fast). Why does it terminate without exception? Answer this question as a comment in the
      Scala file under the same exercise number.
      naturals.take(1000000000).drop(41).take(10).toList

  	Answer:
      When you call take(1000000000), it does not eagerly generate a billion elements. 
      Instead, it creates a promise to generate up to a billion elements when needed lazily.
      
      When you call drop(41), it does not generate the first 41 elements.
      Instead, it creates a promise to skip the first 41 elements when needed lazily.
      
      Only the elements needed to produce the final result (drop(41) and then take(10)) are evaluated.
      This is a small operation! Compared to the billion elements which aren't needed.

      toList is when we actually force the evaluation of the elements.
    

*/


  
  // Exercise 4
  /*
  Write the function takeWhile(p) that returns the longest prefix of a LazyList in
  which all elements match the predicate p. Use pattern matching.
Question:
  naturals.takeWhile { _ < 1000000000 }.drop(100).take(50).toList
  The above should terminate very fast, with no exceptions thrown. Why?

Answer:
  As the answer above, the takeWhile function does not eagerly generate elements.
  Instead, it creates a promise to generate elements lazily when needed.
  in this case we only drop 100 elements and then take the 50 next ones, meaning
  we evaulate 150 elements in total. 

*/

  def takeWhile(p: A => Boolean): LazyList[A] = 
    this match
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Empty


  // Exercise 5
  /*
  Implement forAll(p) that checks that all elements in this LazyList satisfy a
given predicate. Terminate the traversal as soon as it encounters a non-matching value. Use recursion and pattern matching.

Questions:
  (1)
  We use the following test case for forAll: naturals.forAll { _ < 0 }
  If we used this one, it would be crashing: naturals.forAll { _ >=0 }. Why?
  (2)
  Recall that exists has already been implemented before (in the book). Both forAll and exists
  are a bit strange for infinite lazy lists; you should not use them unless you know the result; but once
  you know the result there is no need to use them. They are fine to use on finite lazy lists. Why?

Answer:
  (1):
    Every natural number is above 0. So the predicate _ >= 0 will never be false.
  (2):
    For infinite lazy lists, forAll and exists are not optimal because they will keep evaluating the list
    until they reach the end. 
    For finite lazy lists, forAll and exists are fine to use because they will terminate once the result is known.
*/
  def forAll(p: A => Boolean): Boolean = 
    this match 
      // If the list is non-empty, check the head and recurse on the tail
      case Cons(h, t) => 
        // If the predicate is true for the head, check the rest of the list
        p(h()) && t().forAll(p)
      // If the list is empty or we've reached the end, return true
      case _ => true
  
  // Note 1. lazy; tail is never forced if satisfying element found this is
  // because || is non-strict
  // Note 2. this is also tail recursive (because of the special semantics
  // of ||)
  def exists(p: A => Boolean): Boolean = 
    //use pattern matching
    this match
      // If the predicate is true for the head, return true, or if the predicate is true for (any element) the tail, return true
      case Cons(h, t) => p(h()) || t().exists(p)
      //If we reach the end of the list, return false
      case _ => false
  
  
  // Exercise 6[H]. Use foldRight to implement takeWhile
  def takeWhile1(p: A => Boolean): LazyList[A] =
    val startAcc = empty[A]  
    
    def folder(ele: A, acc: => LazyList[A]): LazyList[A] =
      if p(ele) then cons(ele, acc)
      else empty

    foldRight(startAcc)(folder)


  // Exercise 7[H]: Implement headOption using foldRight.
  def headOption1: Option[A] = 
    val startAcc = None

    def folder(ele: A, acc: => Option[A]): Option[A] = Some(ele)

    foldRight(startAcc)(folder)


  // Exercise 8
  def map[B](transform: A => B): LazyList[B] = 
    val startAcc = empty[B]
    
    def folder(element: A, tail: => LazyList[B]): LazyList[B] = 
      cons(transform(element), tail)
    
    foldRight(startAcc)(folder)

  def filter(predicate: A => Boolean): LazyList[A] = 
    val startAcc = empty[A]
    
    def folder(element: A, tail: => LazyList[A]): LazyList[A] = 
      if predicate(element) then cons(element, tail)
      else tail
    
    foldRight(startAcc)(folder)

  /* 
   * The contsraint 'B >: A' requires that B is a
   * supertype of A. The signature of append allows to concatenate a list of
   * supertype elements, and creates a list of supertype elements.  We could have
   * writte just the following:
   *
   * def append(that: => LazyList[A]): LazyList[A]
   *
   * but this would not allow adding a list of doubles to a list of integers
   * (creating a list of numbers).  Compare this with the definition of
   * getOrElse last week, and the type of foldRight this week.
   * 
   */
  def append[B >: A](that: => LazyList[B]): LazyList[B] = 
    //We simply append 'this' to that. So we set 'that' as our starting acc.
    val startAcc = that
    
    def folder(element: B, acc: => LazyList[B]): LazyList[B] = 
      cons(element, acc)
    
    foldRight(startAcc)(folder)


  def flatMap[B](transform: A => LazyList[B]): LazyList[B] = 
    // Start with an empty LazyList as the initial accumulator
    val startAcc = empty[B]

    //This is the final accumulator so to say.
    def innerFolder(innerElement: B, innerAcc: => LazyList[B]): LazyList[B] = 
      cons(innerElement, innerAcc)

    def folder(element: A, tail: => LazyList[B]): LazyList[B] = 

      //Because every map call returns a list, 
      //we need to flatten this list, so we don't end up with a List[List[B]]
      val transformedList = transform(element)
      //We foldRight over the transformed list, and append the result to the tail
      //this is basically the flatten operation
      transformedList.foldRight(tail)(innerFolder)

    // Use foldRight to combine elements
    foldRight(startAcc)(folder)


  /*
  Exercise 9[M]. The book presents the following implementation of find:
def find(p: A => Boolean): Option[A]= this.filter(p).headOption
Explain why this implementation is suitable (efficient) for lazy lists and would not be optimal for
lists. 
The exam may contain open questions to be answered in English.)*/
  /*
  Answer 1, why is this Implementation Suitable (Efficient) for LazyList?:
    It only processes elements up to the first match and stops there.
    So if list is 10 in lenght. But we find our element at the third match. The 7 rest WON'T be processed.	

  Answer 2, why is this Implementation NOT Optimal for Strict (Eager) Lists?
    It processes all elements of the list, even if the first element matches the predicate.
    So if list is 10 in lenght. But we find our element at the third match. The 7 rest WILL still be processed.

  */


  // Exercise 13
  def mapUnfold[B](f: A => B): LazyList[B] =
    // unfold to create a new LazyList by applying function 'f' to each element of the original LazyList
    LazyList.unfold(this) { 
      //Lambda function passed to unfold above
      currentList =>
        currentList match {
          // If the current list is non-empty, apply 'f' to the head and and pass tail as the new state.
          case Cons(h, t) => 

            Some((f(h()), t()))
          // If the current list is empty, stop unfolding
          case Empty => None
        }
    }

  def takeUnfold(n: Int): LazyList[A] = 
    LazyList.unfold((this, n)) {
      case (currentList, remaining) =>
        currentList match {
          
          // If The list has elements, and we're taking the last element (remaining == 1)
          case Cons(h, t) if remaining == 1 =>
            
            // Include the current element and return an empty list for the next state,
            // so that upon next unfold, it will hit the 'case _ => None' and stop unfolding
            Some((h(), (empty, 0)))

          //If we are still grabbing elements
          case Cons(h, t) if remaining > 1 =>
            Some((h(), (t(), remaining - 1)))

          // Case 3:List empty. 
          case _ => None
        }
    }
  
  def takeWhileUnfold(p: A => Boolean): LazyList[A] =
    
    LazyList.unfold(this) { 
      currentList =>
        currentList match 
          //If The list has elements, and the head satisfies the predicate
          case Cons(h, t) if p(h()) =>
            //We include the head, and pass the tail as the currentList.
            Some((h(), t()))

          //If p(h) doesn't satisfy, or if list is empty. We stop unfolding
          case _ => None
    }


  def zipWith[B >: A, C](combine: (=> B, => B) => C)(otherList: LazyList[B]): LazyList[C] =
    // Use unfold to combine elements of both lazy lists
    LazyList.unfold((this, otherList)) { case (firstList, secondList) =>
      (firstList, secondList) match {
        //If both lists have elements.
        case (Cons(head1, tail1), Cons(head2, tail2)) =>
          // Apply the combine operation to the heads of the lists
          // Use the tails of both lists for the next step, as firstList and secondList
          Some((combine(head1(), head2()), (tail1(), tail2())))

        //If any of the lists are empty, we discard the rest of the elements
        case _ => None
      }
    }



end LazyList // enum ADT


// The companion object for lazy lists ('static methods')

object LazyList:

  def empty[A]: LazyList[A] = Empty

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty 
    then empty
    else cons(as.head, apply(as.tail*))

  // Exercise 1

  //From: The function from should create a lazy list producing all
  //numbers larger than n, starting from n, increasing.
  def from(n: Int): LazyList[Int] =
    def loop (n: Int): LazyList[Int] = 
      cons(n, loop(n+1))
    loop(n)

  //To: The function to should create a lazy list producing
  //all numbers smaller than n, starting from n, decreasing.
  def to(n: Int): LazyList[Int] =
    def loop (n: Int): LazyList[Int] = 
      cons(n, loop(n-1))
    loop(n)

  //Use from to create a value naturals: LazyList[Int] representing all natural numbers in order.
  lazy val naturals: LazyList[Int] =
    from(1)


  /* Exercise 10
  Compute a lazy list of Fibonacci numbers fibs: 0, 1, 1, 2, 3, 5, 8, and so on
  */
  lazy val fibs: LazyList[BigInt] = 
    
    def fib(current: BigInt, next: => BigInt): LazyList[BigInt] =
      cons(current, fib(next, current + next))
    
    //first number is set as current, and next is set as next
    fib(BigInt(0), BigInt(1))
  

  /* Exercise 11
  Exercise 11[H]. Write a more general lazy-list building function called unfold. It takes an initial
state, and a function for producing both the next state and the next value in the generated lazy list.
def unfold[A, S] (z: S) (f: S => Option[(A, S)]): LazyList[A]
If you solve it without using pattern matching, then you obtain a particularly concise solution, that
combines aspects of this and last week’s material.
You can test this function in REPL by unfolding the lazy list of natural numbers and checking whether
its finite prefix is equal to the corresponding prefix of naturals.
*/

// Exercise 11
  def unfold[A, S](currentState: S)(getNextState: S => Option[(A, S)]): LazyList[A] =
    
    //we have some function for producing the next state (S)
    //and the next value in the generated lazy list (A)
    getNextState(currentState) match
      //if getNextState was successfull, we will have some next element and next State
      case Some((nextElement, nextState)) => 
        //If we have a next element, we cons it to the rest of the list
        //We set nextState as the current state, and pass the function to the next state
        cons(nextElement, unfold(nextState)(getNextState))
      
      //if we do not have a next element
      case None => empty
    


  lazy val fibsUnfold: LazyList[BigInt] =
    // Initial state for the Fibonacci sequence: (0, 1)
    val initialState: (BigInt, BigInt) = (BigInt(0), BigInt(1))
    
    def generateNextState(state: (BigInt, BigInt)): Option[(BigInt, (BigInt, BigInt))] =
      state match
        case (currentValue, nextValue) =>
          // Return the current value and the next state (nextValue, currentValue + nextValue)
          //The next state is the currentvalue+nextValue
          Some((currentValue, (nextValue, currentValue + nextValue)))
    
    // Use unfold to generate the Fibonacci sequence
    unfold(initialState)(generateNextState)


end LazyList // companion object
