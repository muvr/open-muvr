package com.eigengo.lift.exercise.classifiers.model

import akka.actor.ActorLogging
import com.eigengo.lift.exercise.classifiers.QueryModel

trait StandardEvaluation {
  this: ActorLogging =>

  import QueryModel._

  // TODO: introduce memoisation into `evaluate` functions?

  def evaluateAtState(path: Proposition, state: Set[GroundFact]): Boolean = path match {
    case True =>
      true

    case False =>
      false

    case Assert(Neg(fact)) =>
      !state.contains(fact)

    case Assert(fact: GroundFact) =>
      state.contains(fact)

    case Conjunction(fact1, fact2, remaining @ _*) =>
      val results = (fact1 +: fact2 +: remaining).map(p => evaluateAtState(p, state))
      results.forall(_ == true)

    case Disjunction(fact1, fact2, remaining @ _*) =>
      val results = (fact1 +: fact2 +: remaining).map(p => evaluateAtState(p, state))
      results.contains(true)
  }

  def evaluateQuery(query: Query)(state: Set[GroundFact], lastState: Boolean): QueryValue = query match {
    case Formula(fact) =>
      val result = evaluateAtState(fact, state)
      log.debug(s"st = $state\n  st |== $query\n  ~~> ${ if (result) "## TRUE ##" else "## FALSE ##"}")
      StableValue(result = result)

    case TT =>
      log.debug(s"st = $state\n  st |== TT\n  ~~> ## TRUE ##")
      StableValue(result = true)

    case FF =>
      log.debug(s"st = $state\n  st |== FF\n  ~~> ## FALSE ##")
      StableValue(result = false)

    case And(query1, query2, remaining @ _*) =>
      log.debug(s"st = $state\n  st |== $query${(query1 +: query2 +: remaining).map(q => s"\n  ~~> && st |== $q").mkString("")}")
      val results = (query1 +: query2 +: remaining).map(q => evaluateQuery(q)(state, lastState))
      results.foldRight[QueryValue](StableValue(result = true)) { case (p, q) => meet(p, q) }

    case Or(query1, query2, remaining @ _*) =>
      log.debug(s"st = $state\n  st |== $query${(query1 +: query2 +: remaining).map(q => s"\n  ~~> || st |== $q").mkString("")}")
      val results = (query1 +: query2 +: remaining).map(q => evaluateQuery(q)(state, lastState))
      results.foldRight[QueryValue](StableValue(result = false)) { case (p, q) => join(p, q) }

    case Exists(AssertFact(fact), query1) if !lastState && evaluateAtState(fact, state) =>
      // for some `AssertFact(fact)` step (whilst not in last trace step)
      log.debug(s"st = $state\n  st |== $query\n  ~~> && st |=/= End \t## TRUE ##\n  ~~> && st |== $fact \t## TRUE ##\n  ~~> && st |== '$query1'")
      UnstableValue(query1)

    case Exists(AssertFact(_), _) =>
      // No `AssertFact(_)` steps possible
      log.debug(s"st = $state\n  st |== $query\n  ~~> ## FALSE ##")
      StableValue(result = false)

    case Exists(Test(query1), query2) =>
      log.debug(s"st = $state\n  st |== $query\n  ~~> && st |== $query1\n  ~~> && st |== $query2")
      meet(evaluateQuery(query1)(state, lastState), evaluateQuery(query2)(state, lastState))

    case Exists(Choice(path1, path2, remainingPaths @ _*), query1) =>
      log.debug(s"st = $state\n  st |== $query${(path1 +: path2 +: remainingPaths).map(p => s"\n  ~~> || st |== Exists($p, $query1)").mkString("")}")
      evaluateQuery(Or(Exists(path1, query1), Exists(path2, query1), remainingPaths.map(p => Exists(p, query1)): _*))(state, lastState)

    case Exists(Sequence(path1, path2, remainingPaths @ _*), query1) =>
      log.debug(s"st = $state\n  st |== $query\n  ~~> st |== ${Exists(path1, Exists(path2, remainingPaths.foldRight(query1) { case (p, q) => Exists(p, q) }))}")
      evaluateQuery(Exists(path1, Exists(path2, remainingPaths.foldRight(query1) { case (p, q) => Exists(p, q) })))(state, lastState)

    case Exists(Repeat(path), query1) if testOnly(path) =>
      log.debug(s"st = $state\n  st |== $query\n  ~~> st |== $query1")
      evaluateQuery(query1)(state, lastState)

    case Exists(Repeat(path), query1) =>
      log.debug(s"st = $state\n  st |== $query\n  ~~> || st |== $query1\n  ~~> || st |== ${Exists(path, Exists(Repeat(path), query1))}")
      join(
        evaluateQuery(query1)(state, lastState),
        evaluateQuery(Exists(path, Exists(Repeat(path), query1)))(state, lastState)
      )

    case All(AssertFact(fact), query1) if !lastState && evaluateAtState(fact, state) =>
      // for all `AssertFact(fact)` steps (whilst not in last trace step)
      log.debug(s"st = $state\n  st |== $query\n  ~~> && st |=/= End \t## TRUE ##\n  ~~> && st |== $fact \t## TRUE ##\n  ~~> && st |== '$query1'")
      UnstableValue(query1)

    case All(AssertFact(_), _) =>
      // No `AssertFact(_)` steps possible
      log.debug(s"st = $state\n  st |== $query\n  ~~> ## TRUE ##")
      StableValue(result = true)

    case All(Test(query1), query2) =>
      log.debug(s"st = $state\n  st |== $query\n  ~~> || st |== ~ $query1\n  ~~> || st |== $query2")
      join(evaluateQuery(QueryModel.not(query1))(state, lastState), evaluateQuery(query2)(state, lastState))

    case All(Choice(path1, path2, remainingPaths @ _*), query1) =>
      log.debug(s"st = $state\n  st |== $query${(path1 +: path2 +: remainingPaths).map(p => s"\n  ~~> && st |== All($p, $query1)").mkString("")}")
      evaluateQuery(And(All(path1, query1), All(path2, query1), remainingPaths.map(p => All(p, query1)): _*))(state, lastState)

    case All(Sequence(path1, path2, remainingPaths @ _*), query1) =>
      log.debug(s"st = $state\n  st |== $query\n  ~~> st |== ${All(path1, All(path2, remainingPaths.foldRight(query1) { case (p, q) => All(p, q) }))}")
      evaluateQuery(All(path1, All(path2, remainingPaths.foldRight(query1) { case (p, q) => All(p, q) })))(state, lastState)

    case All(Repeat(path), query1) if testOnly(path) =>
      log.debug(s"st = $state\n  st |== $query\n  ~~> st |== $query1")
      evaluateQuery(query1)(state, lastState)

    case All(Repeat(path), query1) =>
      log.debug(s"st = $state\n  st |== $query\n  ~~> && st |== $query1\n  ~~> && st |== ${All(path, All(Repeat(path), query1))}")
      meet(
        evaluateQuery(query1)(state, lastState),
        evaluateQuery(All(path, All(Repeat(path), query1)))(state, lastState)
      )
  }

}
