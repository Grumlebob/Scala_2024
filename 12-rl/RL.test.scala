// Advanced Programming, A. Wąsowski, IT University of Copenhagen 

package adpro.rl

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*

import adpro.stateTR.StateTR
import fpinscala.answers.state.RNG

val seed = RNG.Simple(42)

object RLSpec extends org.scalacheck.Properties("rl"): 

  property("bestAction: has always better value than others") = 
    forAll { (qad: Map[Int, Double]) => 
      qad.nonEmpty ==>
        qad.forall { (_, r) => r <= qad(bestAction(qad)) } }

  property("bestReward: has always better value than others") = 
    forAll { (qad: Map[Int, Double]) => 
      qad.nonEmpty ==>
        qad.forall { (_, r) => r <= bestReward(qad) } }
 
  property("bernoulli: check if converges well") = 
    forAll { (seed: Long) =>
      val rng = RNG.Simple(seed)
      val N = 10000
      val l = LazyList.fill(N)(bernoulli(0.5, true, false))
      val experiments = StateTR.sequence(l).run(rng).result._1
      val c = experiments.count(identity).toDouble / N.toDouble
      { c >= 0.48 && c <= 0.52 } :| f"The proportion of success was $c" }

  property("randomOf: check if converges well") = 
    forAll { (seed: Long) =>
      val rng = RNG.Simple(seed)
      val N = 10000
      val as = List(1, 2, 3, 4)
      val l = LazyList.fill(N)(randomOf(as))
      val experiments = StateTR.sequence(l).run(rng).result._1
      val cs = as.map { a => experiments.count { _ == a }.toDouble }
                 .map { c => c.toDouble / N.toDouble }
      cs.forall { c => c >= 0.23 && c <= 0.27 } }

