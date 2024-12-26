/* Final Exam: Advanced Programming, by Andrzej Wąsowski IT University
 * of Copenhagen, Autumn 2024: 06 January 2025
 *
 * The exam consists of 12 questions to be solved within 4 hours.
 * Solve the tasks in the file 'Exam.scala' (this file).
 *
 * You can use all functions provided in the included files,  as well as
 * functions that we implemented in the course. If the source is missing in
 * this folder, you can add it to this file (so that things compile on our
 * side). You can use the standard library functions as well. Staying closer
 * to the course API is likely to yield nicer solutions.
 *
 * You can access any static written materials, printed and online, but you
 * are not allowed to communicate with anybody or with anything (bots).
 * Using GitHub copilot, ChatGPT and similar language models during the exam
 * is not allowed. By submitting you legally declare to have solved the
 * problems alone, without communicating with anybody, and not using
 * language models.
 *
 * Do not modify this file in other ways than answering the questions or
 * adding imports and source of needed functions. Do not reorder the
 * answers, and do not remove question numbers or comments from the file.
 *
 * Submit this file and only this file to LearnIT. Do not convert to
 * any other format than .scala. Do not submit the entire zip archive.
 * The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 character columns to make grading easier.
 *
 * The answers will be graded manually. We focus on the correctness of
 * ideas, the use of concepts, clarity, and style. We will use undisclosed
 * automatic tests during grading, but not to compute the final grade, but
 * to help us debug your code.
 *
 * We do require that your hand-in compiles.  The directory has a project
 * setup so compilation with scala-cli shall work out-of-the-box. If you
 * cannot make a fragment compile, put your solution in a comment, next to
 * the three question marks. We will grade the solutions in comments as
 * well.
 *
 * We will check whether the file compiles by running
 *
 *    scala-cli compile .
 *
 * Hand-ins that do not compile will automatically fail the exam.
 *
 * There is a skeletong test file in the bundle, that you can use to test
 * your solutions.  It does not contain any useful tests. It is just there
 * to get you started with testing faster.
 *
 * We do not recommend writing and running tests if you are pressed for
 * time. It is a good idea to run and test, if you have time.  The tests
 * should not be handed in.  We only grade the answers to questions below.
 *
 * Good luck!
 **/

package adpro

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.TripleEquals.*

import adpro.laziness.LazyList
import adpro.state.*

val NOTHING = 42

// vim:tw=76:cc=70
