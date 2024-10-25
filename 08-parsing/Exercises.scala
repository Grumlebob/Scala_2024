// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.parsing

import scala.util.matching.Regex

import org.scalacheck.*
import org.scalacheck.Prop.*

// ************************************************************************
// PART I
//
// An abstract, representation-independent, design of an algebra of parsing
// combinators.  This code can be compiled (checked for type errors), but to
// test it a concrete implementation of Parser, and ParseError is needed.
// ************************************************************************
 
trait Parsers[ParseError, Parser[+_]]:
  self =>

  def string(s: String): Parser[String]
  def char(c: Char): Parser[Char] =
    string(c.toString).map { _.charAt(0) }

  /** A default `succeed` implementation in terms of `string` and `map`. We
    * leave `succeed` abstract, since `map` is defined below in terms of
    * `flatMap` and `succeed`, which would be a circular definition! But we
    * include the definition here in case implementations wish to use it (say
    * if they provide a custom implementation of `map`, breaking the cycle)
    */
  def defaultSucceed[A](a: A): Parser[A] =
    string("").map { _ => a }

  def succeed[A](a: A): Parser[A]

  def regex(r: Regex): Parser[String]

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]
    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = p.or (p2)
    def slice: Parser[String] 
    def flatMap[B](f: A => Parser[B]): Parser[B]
  end extension


  object Laws:

    // Storing the laws in the trait -- the will be instantiated when we have
    // concrete implementation.  Still without a concrete implementation they
    // can be type checked, when we compile.  This tells us that the
    // construction of the laws is type-correct (the first step for them
    // passing).
    
    val runChar = forAll { (c: Char) => char(c).run(c.toString) == Right(c) }

    val runString = forAll { (s: String) => string(s).run(s) == Right(s) }

    val abracadabra: Prop = Prop.protect {  
      val p =  string("abra") | string("cadabra")
      { p.run("abra") == Right("abra") } :| "on abra" 
        && { p.run("cadabra") == Right("cadabra") } :| "on cadabra" }

    val listOfOn: Prop = Prop.protect {
      val p = (string("ab") | string("cad")).listOfN(3)
      { p.run("ababcad") == Right(List("ab", "ab", "cad")) }      :| "#1"
        && { p.run("cadabab") == Right(List("cad", "ab", "ab")) } :| "#2"
        && { p.run("ababab")  == Right(List("ab", "ab", "ab")) }  :| "#3" }
        
    // map laws

    // Not planning to run this (would need equality on parsers), 
    // but can write for type checking
  
    def mapStructurePreserving[A](p: Parser[A]): Boolean =
      p.map { a => a } == p

    // this test is possible to run once we implement things

    def mapStructurePreserving2succeed: Prop =
      forAll { (s: String, n: Int) =>
        val p = self.succeed(n)
        val p1 = self.map(p)(a => a)
        p.run(s) == p1.run(s)
      }

    // map2 laws

    def map2withTwoSuccesses: Prop =
      val p = self.succeed(42).map2(self.succeed(-42)) { _+_ }
      forAll { (input: String) => p.run(input) == Right(0) }

    // succeed laws

    def succeed = forAll { (a: Int, s: String) =>
        self.succeed(a).run(s) == Right(a) }
    
    // many laws

    def manyString =
      forAll { (s: String, n: Int) =>
        (s.size > 0 && !s.contains('x')) ==> {
          val k = (n % 100).abs
          val t = s.substring (0, Math.min (10, s.size))
          val p = string(t)
          val input = t * k + "xx" + t
          val output = List.fill(k)(t)
          val result = many(p).run(input) 
          val expect =  Right(output)
          Prop(result == expect) :| s"got $result expected $expect"
      } }

    // many1 laws

    def many1Success = forAll { (s: String, n: Int) =>
      (s.size > 0 && !s.contains ('x')) ==> {
        val k = (n % 100).abs + 1
        val t = s.substring(0, Math.min(10, s.size))
        val input = t * k + "x" + t
        val output = List.fill(k)(t)
        string(t).many1.run(input) == Right(output)
      } }

    def many1Fail = forAll { (s: String, t: String) =>
      val q = t.substring(0, Math.min(10, t.size))
      !s.startsWith (q) ==> string(t).many1.run(s).isLeft
    }

    // listOfN laws

    def listOfN1 =
      (string("ab")|string("cad")).listOfN(3).run("ababcad") 
        == Right(List("ab", "ab", "cad"))
   
    def listOfN2 =
      (string("ab")|string("cad")).listOfN(3).run("cadabab") 
        == Right(List("cad", "ab", "ab"))
   
    def listOfN3 =
      (string("ab")|string("cad")).listOfN(3).run("ababab")
        == Right(List("ab", "ab", "ab"))
   
    def listOfN4 =
      (string("ab")|string("cad")).listOfN(2).run("cadabab")
        == Right(List("cad", "ab"))
   
    def listOfN5 =
      (string("ab")|string("cad")).listOfN(1).run("ababab")
        == Right(List("ab"))
   
    def listOfNFail =
       (string("ab")|string("cad")).listOfN(3).run("ababxcad").isLeft

    // or laws

    def orLeft =
      string("abra").or(string("cadabra")).run("abra ") == Right("abra")

    def orRight =
      string("abra").or(string("cadabra")).run("cadabra") == Right("cadabra")

    def orFail = string("abra").or(string("cadabra")).run("blablab").isLeft 

    // string laws
    def stringEmpty = 
      forAll { (s: String) => string("").run(s) == Right("") }

    def stringNegative =
      forAll { (s: String, t: String) =>
        !t.startsWith (s) ==> self.run (string (s)) (t).isLeft }

  end Laws

  // Exercise 1 Implement product and map2 using flatMap.
  extension [A](p1: Parser[A]) 
    def map2[B, C](p2: => Parser[B])(combine: (A, B) => C): Parser[C] =
      p1.flatMap { resultA =>
        p2.map { resultB =>
          val combinedResult = combine(resultA, resultB)
          combinedResult
        }
      }

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      p1.flatMap { resultA =>
        p2.map { resultB =>
          (resultA, resultB)
        }
      }


    // Write here: 
    //
    // (1) The type parameter A is not explicitly declared in the signatures of map2 and product because it is implicitly inferred from the first parser argument.
    //
    // (2) The second argument p2: => Parser[B] is passed by-name to allow lazy evaluation of the second parser. By making p2 a by-name parameter, it ensures that the second parser is only evaluated if the first parser (p1, which map2 is called on) succeeds.

    def **[B](p2: => Parser[B]): Parser[(A, B)] = 
      p1.product(p2)
    def |*[B](p2: => Parser[B]): Parser[B] =
      p1.flatMap { s => p2 }
    def *| (p2: => Parser[Any]): Parser[A] =
      (p1 ** p2).map { (a, _) => a }
    def ? : Parser[Option[A]] =
      { p1.map { Some(_) } } | succeed(None)
    def * : Parser[List[A]] = 
      p1.many
  end extension

  /* Exercise 2
  Use succeed and map2 to implement the combinator many. This combinator continues
to parse using p as long as it succeeds and puts the results in a list. The last (the rightmost) parsed
element is the head of the list after parsing.
  */

  extension [A](p: Parser[A]) 
    def many: Parser[List[A]] =
      p.flatMap { head =>
        p.many.map(tail => head :: tail)
      }.or(succeed(Nil))


  // Exercise 3 Express map using flatMap
  extension [A](parser: Parser[A])
    def map[B](transform: A => B): Parser[B] =
      parser.flatMap { result =>
        val transformedResult = transform(result)
        succeed(transformedResult)
      }


  // Exercise 4. Use many and map to implement a parser manyA that recognizes zero or more consecutive
  //’a’ characters and returns the number of matched characters

  // A better name would be: howManyA
  def manyA: Parser[Int] =
    val parserA = char('a')
    val parsedAList = parserA.many
    parsedAList.map(_.size)


  // Exercise 5. Implement many1, a parser that matches its argument 1 or more times
  extension [A](parser: Parser[A]) 
    def many1: Parser[List[A]] =
      val atLeastOneParser = parser.map2(parser.many)(_ :: _)
      atLeastOneParser
      
  /*
  In the context of the parser combinators, many1 is an extension method because it operates on a specific parser instance and enhances its behavior without modifying the underlying parser class directly. In functional programming, it is common to use extension methods to add functionality to a type without altering the original type’s definition.*/

  // Exercise 6. Using map2 and succeed, implement the combinator listOfN:
  extension [A](parser: Parser[A]) 
    def listOfN(countOfElements: Int): Parser[List[A]] =
      if countOfElements <= 0 then succeed(Nil)
      else
        val firstElementParser = parser
        val restElementsParser = parser.listOfN(countOfElements - 1)
        firstElementParser.map2(restElementsParser)(_ :: _)


  // Exercise 7
  /*
  Using flatMap write the parser that parses a single digit, and then as many occurrences
of the character ’a’ as was the value of the digit. Your parser should be named digitTimesA and
return the value of the digit parsed (thus one less the number of characters consumed).*/
  def digitTimesA: Parser[Int] =
    val digitParser = regex("[0-9]".r)
    digitParser.flatMap { digit =>
      val parsedDigit = digit.toInt
      char('a').listOfN(parsedDigit).map(_.size) // Expect exactly `parsedDigit` 'a's
    }


  // For Exercise 8 read the code below until you find it.

end Parsers

// **********************************************************************
// PART II
//
// A concrete implementation for our parsers, with optimized slicing and
// committing (the late variant in the chapter).  It provides all the 
// basic operator implementations for Parsers and detailed representation
// of the parser data structures.
// **********************************************************************

case class ParseError(stack: List[(Location, String)] = Nil):

  def push(loc: Location, msg: String): ParseError =
    this.copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map { (_, s) }.toList)

  def latest: Option[(Location,String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest.map { _._1 }

  /** Display collapsed error stack - any adjacent stack elements with the
    * same location are combined on one line. For the bottommost error, we
    * display the full line, with a caret pointing to the column of the error.
    * Example:
    * 1.1 file 'companies.json'; array
    * 5.1 object
    * 5.2 key-value
    * 5.10 ':'
    * { "MSFT" ; 24,
    *          ^
    */
  override def toString =
    if stack.isEmpty then "no error message"
    else
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
        collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map((loc, msg) => s"${formatLoc(loc)} $msg").mkString("\n") + context

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      view.
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = s"${l.line}.${l.col}"

end ParseError

case class Location(input: String, offset: Int = 0):

  /** The line number in the current input at the position offset */
  lazy val line: Int = 
    input.slice(0, offset + 1).count { _ == '\n' } + 1

  /** The column number in the current input at the position offset */
  lazy val col: Int = 
    input.slice(0, offset + 1).lastIndexOf('\n') match
      case -1 => offset + 1
      case lineStart => offset - lineStart

  /** Promote a location and a message to a ParseError */
  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  /** Shift this location by n characters forward */
  def advanceBy(n: Int) = copy(offset = offset + n)

  /** The remaining input starting at this location */
  def remaining: String = input.substring(offset)

  /** A slice of the input starting at this location and having n chars */
  def slice(n: Int): String = input.substring(offset, offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if input.length > 1
    then
      val itr = input.linesIterator.drop(line - 1)
      if (itr.hasNext) itr.next() else ""
    else ""

  def columnCaret = (" " * (col - 1)) + "^"

end Location

/** `isSliced` indicates if the current parser is surround by a `slice`
  * combinator. This lets us avoid building up values that will end up getting
  * thrown away. Mostly convenience functions used below. */
case class ParseState(loc: Location, isSliced: Boolean = false):
  def advanceBy(numChars: Int): ParseState =
    copy(loc = loc.advanceBy(numChars))
  def input: String = loc.input.substring(loc.offset)
  def unslice = copy(isSliced = false)
  def reslice(s: ParseState) = copy(isSliced = s.isSliced)
  def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
end ParseState

type Parser[+A] = ParseState => Result[A]

/** The result of a parse--a `Parser[A]` returns a `Result[A]`.
  *
  * There are three cases:
  *   - Success(a, n): a is the value, n is # of consumed characters
  *   - Slice(n): a successful slice; n is the # of consumed characters
  *   - Failure(n, isCommitted): a failing parse
  *
  * As usual, we define some helper functions on `Result`.
  * 
  * `Result` is an example of a Generalized Algebraic Data Type (GADT),
  * which means that not all the data constructors of `Result` have
  * the same type. In particular, `Slice` _refines_ the `A` type
  * parameter to be `String`. If we pattern match on a `Result`
  * and obtain a `Slice`, we expect to be able to assume that `A` was
  * in fact `String` and use this type information elsewhere.
  */
enum Result[+A]:
  case Success(get: A, length: Int)
  case Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
  case Slice(length: Int) extends Result[String]

  /** A helper to extract the value from the result */
  def extract(input: String): Either[ParseError, A] = this match
    case Slice(length) => Right(input.substring(0, length))
    case Success(get, _) => Right(get)
    case Failure(get, _) => Left(get)

  /** Convert a parse result to a slice (failures remain failures). */
  def slice: Result[String] = this match
    case s @ Slice(_) => s
    case Success(_, length) => Slice(length)
    case f @ Failure(_, _) => f

  /* Used by `attempt`. */
  def uncommit: Result[A] = this match
    case Failure(e, true) => Failure(e, false)
    case _ => this

  /* Used by `flatMap` */
  def addCommit(isCommitted: Boolean): Result[A] = this match
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _ => this

  /* Used by `scope`, `label`. */
  def mapError(f: ParseError => ParseError): Result[A] = this match
    case Failure(e, c) => Failure(f(e), c)
    case _ => this

  /** Adjust the number of characters consumed while producing the result. `n`
    * is the number of chars consumed by this particular parser that produced
    * the result (the total number of consumed. could be higher if other
    * parsers have already consumed some part of the input). */
  def advanceSuccess(n: Int): Result[A] = this match
    case Slice(length) => Slice(length + n)
    case Success(get, length) => Success(get, length + n)
    case Failure(_, _) => this

end Result

object Sliceable 
  extends Parsers[ParseError, Parser]:
  
  import Result.{Slice, Success, Failure}

  // Exercise 8
  //As a warmup, answer (for yourself)
  //the following question: why do we have to make the Parser type covariant here?
  /*
  Making Parser covariant allows for greater flexibility in composing and reusing parsers by permitting parsers of specific types (e.g., Parser[String], Parser[Int]) to be used in more general contexts (e.g., Parser[Any])
  */

  /** Consume no characters and succeed with the given value */
  def succeed[A](value: A): Parser[A] =
    (state: ParseState) => Success(value, 0)


  // For Exercise 9 continue reading below
  /*
  Implement the combinator or that takes two parsers as arguments and tries them sequentially.
The second parser is only tried, if the first one failed.*/

  def string(w: String): Parser[String] = (s: ParseState) =>
    val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
    if i == -1 then // they matched
      if s.isSliced then Slice(w.length)
      else Success(w, w.length)
    else Failure(s.loc.advanceBy(i).toError(s"'$w'"), i != 0)

  /** Returns -1 if s.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s, returns s.length. Note: locally imperative. */
  def firstNonmatchingIndex(s: String, s2: String, offset: Int): Int =
    var i = 0
    while (i + offset < s.length && i < s2.length)
      if s.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s.length - offset >= s2.length then -1
    else s.length - offset

  def fail(msg: String): Parser[Nothing] =
    (s: ParseState) => Failure(s.loc.toError(msg), true)


  extension [A](p: Parser[A]) 

    def run(s: String): Either[ParseError, A] =
      p(ParseState(Location(s), false)).extract(s)

    /* This implementation is rather delicate. Since we need an `A`
     * to generate the second parser, we need to run the first parser
     * 'unsliced', even if the `flatMap` is wrapped in a `slice` call.
     * Once we have the `A` and have generated the second parser to
     * run, we can 'reslice' the second parser.
     *
     * Note that this implementation is less efficient than it could
     * be in the case where the choice of the second parser does not
     * depend on the first (as in `map2`). In that case, we could
     * continue to run the first parser sliced.
     */
    def flatMap[B](f: A => Parser[B]): Parser[B] = (s: ParseState) => 
      p(s.unslice) match
      case Success(a, n) =>
        f(a)(s.advanceBy(n).reslice(s))
          .addCommit(n != 0)
          .advanceSuccess(n)
      case Slice(n) =>
        f(s.slice(n))(s.advanceBy(n).reslice(s)).advanceSuccess(n)
      case f @ Failure(_, _) => f

    def slice: Parser[String] = 
      (s: ParseState) => p(s.copy(isSliced = true)).slice

  end extension

  // Exercise 9
  /*
  Implement the combinator or that takes two parsers as arguments and tries them sequentially.
  The second parser is only tried, if the first one failed.
*/
 
  extension [A](parser1: Parser[A]) 
    def or(parser2: => Parser[A]): Parser[A] =
      (state: ParseState) =>
        val firstParserResult = parser1(state)
        firstParserResult match
          case failure @ Failure(_, false) => parser2(state)
          case otherResult => otherResult


  // Exercise 10
  /*
  In Scala, a string s can be promoted to a Regex object (which has methods for matching)
using the method call s.r, for instance, "[a-zA-Z_][a-zA-Z0-9_]*".r.
Implement a new primitive, regex, which promotes a regular expression to a parser:*/

  def regex(pattern: Regex): Parser[String] = (state: ParseState) =>
    val inputRemaining = state.loc.remaining
    val matchedPrefix = pattern.findPrefixOf(inputRemaining)

    matchedPrefix match
      case Some(matched) =>
        val lengthConsumed = matched.length
        if state.isSliced then
          Slice(lengthConsumed)  // Return only the length if sliced
        else
          Success(matched, lengthConsumed)  // Return matched string and length if not sliced
      case None =>
        val parseError = state.loc.toError(s"Expected pattern: $pattern")
        Failure(parseError, isCommitted = false)


   
end Sliceable

// *********************************************************************
// PART III
//
// An example implementation of JSON parser (it uses the abstract parser
// interface, but it is tested with the concrete implementation above)
// *********************************************************************

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])
  
class JSONParser[ParseError, Parser[+_]](P: Parsers[ParseError,Parser]):

  import P.*
  import JSON.*

  // Exercise 11
  /*
  Implement:
• QUOTED – a Parser[String] that matches a quoted string literal, and returns the value of the
string (without the syntactic quotes)
• DOUBLE – a Parser[Double] that matches a double number literal, and returns its numeric value
• ws – a Parser[Unit] that matches a non-empty sequence of white space characters
*/

  lazy val QUOTED: Parser[String] =
    val quotePattern = "\"[^\"]*\"".r
    regex(quotePattern).map { matched =>
      val contentWithoutQuotes = matched.drop(1).dropRight(1)
      contentWithoutQuotes
    }


  lazy val DOUBLE: Parser[Double] =
    val doublePattern = "[-+]?([0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?|[0-9]+)".r
    regex(doublePattern).map { matched =>
      val parsedDouble = matched.toDouble
      parsedDouble
    }

  lazy val ws: Parser[Unit] =
    val whitespacePattern = "\\s+".r  // Matches one or more whitespace characters
    regex(whitespacePattern).map(_ => ())



  // Exercise 12 
  /*
• jnull – matches the literal null and returns JNull
• jbool – matches literals true and false and returns JBool
• jstring – wraps the result of QUOTED in a JString value
• jnumber – wraps the result of DOUBLE in a JNumber value

*/
  
  lazy val jnull: Parser[JSON] =
    string("null").map(_ => JNull)


  lazy val jbool: Parser[JSON] =
    val trueParser = string("true").map(_ => JBool(true))
    val falseParser = string("false").map(_ => JBool(false))
    trueParser or falseParser


  lazy val jstring: Parser[JString] =
    QUOTED.map { content =>
      JString(content)
    }


  lazy val jnumber: Parser[JNumber] =
    DOUBLE.map { number =>
      JNumber(number)
    }


  // Exercise 13
  /*
  • jarray – parses an array literal: a comma-separated list of JSON values, surrounded by a pair of
square brackets
• field – parses a JSON object field: a quoted field name, followed by a colon token, followed by a
JSON value. It produces a field name–value pair, that will later be used to construct an object
• jobject – parses a JSON object: a comma-separated list of fields, surrounded by a pair of braces.
• json that parses an arbitrary JSON value is already implemented in the template file
*/

  // A parser that matches and skips optional whitespace
  lazy val optWs: Parser[Unit] = regex("\\s*".r).map(_ => ())

  private lazy val commaSeparatedVals: Parser[List[JSON]] = 
    (for {
      first <- json
      _     <- optWs
      rest  <- (for {
                  _    <- string(",")
                  next <- commaSeparatedVals
                } yield next).or(succeed(Nil))
    } yield first :: rest).or(succeed(Nil))


  // Parser for JSON arrays (handles optional commas between values, no trailing commas)
  lazy val jarray: Parser[JArray] =
    for {
      _      <- string("[")
      _      <- optWs // Optional whitespace after opening bracket
      values <- commaSeparatedVals.or(succeed(Nil)) // Comma-separated values or empty array
      _      <- optWs.flatMap(_ => string("]")) // Ensure the closing bracket is handled
    } yield JArray(values.toVector)

  lazy val field: Parser[(String, JSON)] = 
    for {
      key   <- QUOTED
      _     <- optWs
      _     <- string(":")
      _     <- optWs
      value <- json
    } yield (key, value)


  private lazy val commaSeparatedFields: Parser[List[(String, JSON)]] = 
    for {
      first <- field
      _     <- optWs
      rest  <- (for {
                  _    <- string(",")
                  _    <- optWs
                  more <- commaSeparatedFields
                } yield more).or(succeed(Nil))
    } yield first :: rest


  // Parser for JSON objects (handles optional commas between fields, no trailing commas)
  lazy val jobject: Parser[JObject] =
    for {
      _      <- string("{")
      _      <- optWs // Optional whitespace after opening brace
      fields <- commaSeparatedFields.or(succeed(Nil)) // Comma-separated fields or empty object
      _      <- optWs.flatMap(_ => string("}")) // Ensure the closing brace is handled
    } yield JObject(fields.toMap)


  // Top-level JSON parser
  lazy val json: Parser[JSON] = 
    for {
      _ <- optWs
      j <- string("null").map(_ => JNull) or jbool or jnumber or jstring or jarray or jobject
      _ <- optWs
    } yield j



    
  // Exercise 14 (no code)

  //see main method runJsonTest() below

  // Exercise 15
  //
  // Write here:
  //
  // (1). They don’t depend on any concrete implementation of the parsers but express expected behavior that all implementations must satisfy. Therefore, the laws can be type-checked without needing a concrete implementation.
  //
  // (2). The main advantage is reusability: all concrete implementations of Parsers can be tested against the same set of laws, ensuring consistency and correctness. It also provides type-safety and encourages early validation of the expected behavior, making it easier to implement and verify parsers consistently.
end JSONParser


//To test exercise 13/14
@main def runJsonTest(): Unit = 
  import Sliceable.*

  val jsonTxt = """
  {
    "Company name" : "Microsoft Corporation",
    "Ticker"  : "MSFT",
    "Active"  : true,
    "Price"   : 30.66,
    "Shares outstanding" : 8.38e9,
    "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
  }
  """

  // Instantiate the parser
  val jsonParser = new JSONParser(Sliceable)

  // Run the parser on the JSON string
  val result = jsonParser.json.run(jsonTxt)

  // Print the result of the parsing
  println(s"Parsing result: $result")

