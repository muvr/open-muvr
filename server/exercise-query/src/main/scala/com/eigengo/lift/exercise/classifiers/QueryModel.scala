package com.eigengo.lift.exercise.classifiers

object QueryModel {

  /**
   * Facts that may hold of sensor data.
   */
  sealed trait Fact

  case class Neg(fact: GroundFact) extends Fact {
    override def toString = s"~$fact"
  }

  /**
   * Ground facts logically model named ground predicates (Scala values are embedded as constants)
   */
  case class GroundFact(name: String, values: Any*) extends Fact

  /**
   * We query exercise models using a DSL based upon a linear-time dynamic logic. Exercise sessions define the finite
   * length trace over which our queries will be evaluated. Paths are used to define (logical) windows over which queries
   * are to hold.
   *
   * NOTE:
   *   1. we intentionally have negation as being defined (rather than as a language primitive) - that way queries et al
   *      may be kept in negation normal form (NNF).
   *   2. exercise models are responsible for interpreting query language semantics in a meaningful way!
   *
   * REFERENCE:
   *   [2014] LTLf and LDLf Monitoring by Giuseppe De Giacomo, Riccardo De Masellis, Marco Grasso, Fabrizio Maria Maggi
   *   and Marco Montali
   */

  sealed trait Proposition

  case object True extends Proposition {
    override def toString = "true"
  }

  case object False extends Proposition {
    override def toString = "false"
  }

  case class Assert(fact: Fact) extends Proposition {
    override def toString = fact.toString
  }

  case class Conjunction(fact1: Proposition, fact2: Proposition, remainingFacts: Proposition*) extends Proposition {
    override def toString = (fact1 +: fact2 +: remainingFacts).mkString("(", " && ", ")")
  }

  case class Disjunction(fact1: Proposition, fact2: Proposition, remainingFacts: Proposition*) extends Proposition {
    override def toString = (fact1 +: fact2 +: remainingFacts).mkString("(", " || ", ")")
  }

  def not(fact: Proposition): Proposition = fact match {
    case True =>
      False

    case False =>
      True

    case Assert(Neg(fact1)) =>
      Assert(fact1)

    case Assert(fact1: GroundFact) =>
      Assert(Neg(fact1))

    case Conjunction(fact1, fact2, remaining @ _*) =>
      Disjunction(not(fact1), not(fact2), remaining.map(not): _*)

    case Disjunction(fact1, fact2, remaining @ _*) =>
      Conjunction(not(fact1), not(fact2), remaining.map(not): _*)
  }

  /**
   * Path language - we encode path regular expressions here
   */
  sealed trait Path

  case class AssertFact(fact: Proposition) extends Path {
    override def toString = fact.toString
  }

  case class Test(query: Query) extends Path {
    override def toString = s"($query ?)"
  }

  case class Choice(path1: Path, path2: Path, remaining: Path*) extends Path {
    override def toString = (path1 +: path2 +: remaining).mkString("(", " + ", ")")
  }

  case class Sequence(path1: Path, path2: Path, remaining: Path*) extends Path {
    override def toString = (path1 +: path2 +: remaining).mkString("(", "; ", ")")
  }

  case class Repeat(path: Path) extends Path {
    override def toString = s"($path *)"
  }

  /**
   * Auxillary function that determines if a path only involves combinations of `Test` expressions (used by standard
   * model).
   *
   * @param path path to be tested
   */
  def testOnly(path: Path): Boolean = path match {
    case AssertFact(_) =>
      false

    case Test(_) =>
      true

    case Choice(path1, path2, remainingPaths @ _*) =>
      (path1 +: path2 +: remainingPaths).map(testOnly).fold(true) { case (x, y) => x && y }

    case Sequence(path1, path2, remainingPaths @ _*) =>
      (path1 +: path2 +: remainingPaths).map(testOnly).fold(true) { case (x, y) => x && y }

    case Repeat(path1) =>
      testOnly(path1)
  }

  /**
   * Query language - we encode linear-time dynamic logic here
   */
  sealed trait Query

  case class Formula(fact: Proposition) extends Query {
    override def toString = fact.toString
  }

  case object TT extends Query {
    override def toString = "TT"
  }

  case object FF extends Query {
    override def toString = "FF"
  }

  case class And(query1: Query, query2: Query, remainingQueries: Query*) extends Query {
    override def toString = (query1 +: query2 +: remainingQueries).mkString("(", " && ", ")")
  }

  case class Or(query1: Query, query2: Query, remainingQueries: Query*) extends Query {
    override def toString = (query1 +: query2 +: remainingQueries).mkString("(", " || ", ")")
  }

  /**
   * Logical expressions that operate on path prefixes:
   *   - `Exists` asserts existence of a path prefix;
   *   - `All` asserts query for all path prefixes.
   *
   * @param path  path prefix at end of which query should hold
   * @param query query that is to hold at end of a path prefix
   */
  case class Exists(path: Path, query: Query) extends Query {
    override def toString = s"<$path> $query"
  }

  case class All(path: Path, query: Query) extends Query {
    override def toString = s"[$path] $query"
  }

  /**
   * Convenience function that provides negation on queries, whilst keeping them in NNF. Translation is linear in the
   * size of the query.
   */
  def not(query: Query): Query = query match {
    case Formula(fact) =>
      Formula(not(fact))

    case TT =>
      FF

    case FF =>
      TT

    case And(query1, query2, remaining @ _*) =>
      Or(not(query1), not(query2), remaining.map(not): _*)

    case Or(query1, query2, remaining @ _*) =>
      And(not(query1), not(query2), remaining.map(not): _*)

    case Exists(path, query1) =>
      All(path, not(query1))

    case All(path, query1) =>
      Exists(path, not(query1))
  }

  /**
   * Indicates that the exercise session has completed (remaining trace is empty)
   */
  val End: Query = All(Test(Formula(True)), FF)

  /**
   * Denotes the last step of the exercise session
   */
  val Last: Query = All(AssertFact(True), End)

  /**
   * Following definitions allow linear-time logic to be encoded within the current logic. Translation here is linear in
   * the size of the formula.
   */

  /**
   * At the next point of the exercise session, the query will hold
   */
  def Next(query: Query): Query = Exists(AssertFact(True), query)

  /**
   * At some point in the exercise session, the query will hold
   */
  def Diamond(query: Query): Query = Exists(Repeat(AssertFact(True)), query)

  /**
   * For all points in the exercise session, the query holds
   */
  def Box(query: Query): Query = All(Repeat(AssertFact(True)), query)

  /**
   * Until query2 holds, query1 will hold in the exercise session. Query2 will hold at some point during the exercise
   * session.
   */
  def Until(query1: Query, query2: Query): Query = Exists(Repeat(Sequence(Test(query1), AssertFact(True))), query2)

  /**
   * Values representing the current evaluation state of a given query:
   *   - stable queries are values that hold now and, no matter how the model develops, will remain in their current state
   *   - unstable queries are values that hold now and, for some sequence of possible events updates, may deviate from
   *     their current value
   */
  sealed trait QueryValue
  /**
   * @param result validity of linear dynamic logic statement at this and future points in time
   */
  case class StableValue(result: Boolean) extends QueryValue
  /**
   * @param state  positive propositional description of the next states for an alternating automaton over words
   */
  case class UnstableValue(state: Query) extends QueryValue

  /**
   * Auxillary functions that support QueryValue lattice structure
   */

  def meet(value1: QueryValue, value2: QueryValue): QueryValue = (value1, value2) match {
    case (StableValue(result1), StableValue(result2)) =>
      StableValue(result1 && result2)

    case (UnstableValue(atom1), UnstableValue(atom2)) =>
      UnstableValue(And(atom1, atom2))

    case (StableValue(true), result2 @ UnstableValue(_)) =>
      result2

    case (result1 @ StableValue(false), UnstableValue(_)) =>
      result1

    case (result1 @ UnstableValue(_), StableValue(true)) =>
      result1

    case (UnstableValue(_), result2 @ StableValue(false)) =>
      result2
  }

  def join(value1: QueryValue, value2: QueryValue): QueryValue = (value1, value2) match {
    case (StableValue(result1), StableValue(result2)) =>
      StableValue(result1 || result2)

    case (UnstableValue(atom1), UnstableValue(atom2)) =>
      UnstableValue(Or(atom1, atom2))

    case (result1 @ StableValue(true), UnstableValue(_)) =>
      result1

    case (StableValue(false), result2 @ UnstableValue(_)) =>
      result2

    case (UnstableValue(_), result2 @ StableValue(true)) =>
      result2

    case (result1 @ UnstableValue(_), StableValue(false)) =>
      result1
  }

  def complement(value: QueryValue): QueryValue = value match {
    case StableValue(result) =>
      StableValue(!result)

    case UnstableValue(atom) =>
      UnstableValue(not(atom))
  }

}
