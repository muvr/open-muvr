package com.eigengo.lift.exercise.classifiers.workflows

import com.eigengo.lift.exercise._
import com.eigengo.lift.exercise.classifiers.QueryModel.GroundFact

object ClassificationAssertions {

  /**
   * Named gesture matches with probability >= `matchProbability`
   */
  def Gesture(name: String, matchProbability: Double, sensor: SensorDataSourceLocation): GroundFact =
    new GroundFact(name, matchProbability, sensor) {
      override def toString: String = {
        s"($name@$sensor >= $matchProbability)"
      }
    }

  /**
   * Bind inferred (e.g. machine learnt) assertions to sensors in a network of sensors.
   *
   * @param facts   facts true of this location
   * @param value   raw sensor network data that assertion holds for
   */
  case class BindToSensors(facts: Set[GroundFact], value: SensorNetValue)

}
