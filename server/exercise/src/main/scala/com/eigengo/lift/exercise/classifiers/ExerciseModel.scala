package com.eigengo.lift.exercise.classifiers

import akka.actor.{ActorRef, ActorLogging}
import akka.event.LoggingReceive
import akka.stream.{ActorFlowMaterializer, ActorFlowMaterializerSettings}
import akka.stream.actor.{ActorPublisherMessage, ActorPublisher}
import akka.stream.scaladsl._
import scala.annotation.tailrec
import scala.async.Async._
import com.eigengo.lift.exercise.UserExercisesClassifier.ClassifiedExercise
import com.eigengo.lift.exercise.classifiers.model.SMTInterface
import com.eigengo.lift.exercise.classifiers.workflows.ClassificationAssertions.BindToSensors
import com.eigengo.lift.exercise.classifiers.workflows.SlidingWindow
import com.eigengo.lift.exercise._
import scala.concurrent.Future
import scala.language.higherKinds

/**
 * Exercising model interface. Implementations of this abstract class define specific exercising models that may be updated and queried.
 * Querying determines the states or points in time at which a `watch` query is satisfiable.
 */
abstract class ExerciseModel(name: String, sessionProps: SessionProperties, toWatch: Set[QueryModel.Query] = Set.empty)(implicit prover: SMTInterface)
  extends ActorPublisher[(SensorNetValue, ActorRef)]
  with ActorLogging {

  import ActorPublisherMessage._
  import context.dispatcher
  import QueryModel._
  import FlowGraph.Implicits._

  val config = context.system.settings.config
  val settings = ActorFlowMaterializerSettings(context.system)
  // Received sensor data is buffered - configuration data determines buffer size
  val bufferSize = config.getInt("classification.buffer")
  val samplingRate = config.getInt("classification.frequency")
  val maxBufferSize = config.getInt("classification.model.receive.buffer")

  implicit val materializer = ActorFlowMaterializer(settings)

  var buffer = Vector.empty[(SensorNetValue, ActorRef)]
  var seenStop: Boolean = false
  var calledOnComplete: Boolean = false

  /**
   * Defined by implementing subclasses. Given a new event in our sensor trace, determines the next state that our model
   * evaluator will assume.
   *
   * @param current   current model evaluator state
   * @param event     new event received by the sensor network
   * @param lastState determines if this is the last event to be received by the sensor network or not
   */
  protected def evaluateQuery(current: Query)(event: Set[GroundFact], lastState: Boolean): QueryValue

  /**
   * Defined by implementing subclasses. Configurable flow that determines the (optional) message sent back to the
   * UserExercisesProcessor.
   *
   * @param query  query that we have been requested to watch
   */
  protected def makeDecision(query: Query): Flow[QueryValue, Option[ClassifiedExercise], Unit]

  /**
   * Defined by implementing subclasses. Configurable flow defined by implementing subclasses
   */
  protected def workflow: Flow[SensorNetValue, BindToSensors, Unit]

  /**
   * Flow that defines how per query evaluation influences decision making (and so messages received by UserExercisesProcessor)
   */
  private[classifiers] def evaluate(query: Query) = {
    require(toWatch.contains(query))

    var currentState: Query = query
    var stableState: Option[QueryValue] = None

    // NOTE: as mutable state is updated by this flow, we need to ensure evaluation occurs in a "synchronous" step
    Flow[List[BindToSensors]].mapAsync { sensorData =>
      log.debug(if (sensorData.length == 1) s"\n  LAST EVENT: ${sensorData.head}" else s"\n  EVENT: ${sensorData.head}")

      val result = sensorData match {
        case _ if stableState.isDefined =>
          stableState.get

        case List(event) =>
          evaluateQuery(currentState)(event.facts, lastState = true)

        case List(event, _) =>
          evaluateQuery(currentState)(event.facts, lastState = false)
      }
      log.debug(s"\n  EVALUATE: $currentState\n  ~~> $result")
      result match {
        case UnstableValue(nextQuery) =>
          async {
            // We interact with the prover concurrently to determine validity of, satisfiability of and simplify `nextQuery`
            val validQuery = prover.valid(nextQuery)
            val satisfiableQuery = prover.satisfiable(nextQuery)
            val simplifiedQuery = prover.simplify(nextQuery)

            if (await(validQuery)) {
              // `nextQuery` is valid - so any LDL unwinding of this formula will allow it to become true
              StableValue(result = true)
            } else if (await(satisfiableQuery)) {
              // We need to unwind LDL formula further in order to determine its validity
              currentState = await(simplifiedQuery)
              // We pass on next query here to "facilitate" decision making on repeated query matching
              UnstableValue(nextQuery)
            } else {
              // `nextQuery` is unsatisfiable - so no LDL unwinding of this formula will allow it to become true
              StableValue(result = false)
            }
          }

        case value: StableValue =>
          stableState = Some(value)

          Future(value)
      }
    }
  }

  /**
   * Flow graph that defines:
   *   - how sensor events (received by this actor) are added to a (i.e. essentially `Source[SensorNetValue]`) trace
   *   - how (`watch`) queries are evaluated (i.e. `evaluateQuery`) over that trace
   *   - how decision messages (i.e. result of `makeDecision`) are relayed back to `UserExercisesProcessor`.
   */
  private def model: RunnableFlow[Unit] = {
    if (toWatch.isEmpty) {
      // No queries to watch, so we ignore all SensorNetValue's
      FlowGraph.closed() { implicit builder =>
        Source(ActorPublisher(self)) ~> Sink.ignore
      }
    } else if (toWatch.size == 1) {
      // We have at least one query to watch and report upon
      FlowGraph.closed() { implicit builder =>
        val split = builder.add(Unzip[SensorNetValue, ActorRef]())
        val join = builder.add(Zip[Option[ClassifiedExercise], ActorRef]())
        val query = toWatch.head

        Source(ActorPublisher.apply[(SensorNetValue, ActorRef)](self)) ~> split.in
        // 2 element sliding window allows workflow to look ahead one step and determine if the event trace is in its last state or not
        split.out0 ~> workflow.transform(() => SlidingWindow[BindToSensors](2)) ~> evaluate(query) ~> makeDecision(query) ~> join.in0
        split.out1 ~> join.in1
        join.out ~> Flow[(Option[ClassifiedExercise], ActorRef)]
          .filter(_._1.nonEmpty)
          .map { case (ex, ref) => (ex.get, ref) } ~> Sink.foreach[(ClassifiedExercise, ActorRef)] { case (exercise, ref) => ref ! exercise }
      }
    } else {
      // We have multiple queries to watch and report upon
      FlowGraph.closed() { implicit builder =>
        val split = builder.add(Unzip[SensorNetValue, ActorRef]())
        val listener = builder.add(Broadcast[ActorRef](toWatch.size))
        val event = builder.add(Broadcast[List[BindToSensors]](toWatch.size))

        Source(ActorPublisher.apply[(SensorNetValue, ActorRef)](self)) ~> split.in
        // 2 element sliding window allows workflow to look ahead one step and determine if the event trace is in its last state or not
        split.out0 ~> workflow.transform(() => SlidingWindow[BindToSensors](2)) ~> event
        split.out1 ~> listener
        for (query <- toWatch) {
          val join = builder.add(Zip[Option[ClassifiedExercise], ActorRef]())

          event ~> evaluate(query) ~> makeDecision(query) ~> join.in0
          listener ~> join.in1
          join.out ~> Flow[(Option[ClassifiedExercise], ActorRef)]
            .filter(_._1.nonEmpty)
            .map { case (ex, ref) => (ex.get, ref) } ~> Sink.foreach[(ClassifiedExercise, ActorRef)] { case (exercise, ref) => ref ! exercise }
        }
      }
    }
  }

  override def preStart() = {
    log.debug(s"Started model $name evaluation workflow for the queries: $toWatch")
    // Setup model evaluation workflow
    model.run()
  }

  def receive = LoggingReceive {
    // TODO: refactor code so that the following assumptions may be weakened further!
    case event: SensorNet =>
      require(
        event.toMap.values.forall(_.forall(_.values.nonEmpty)),
        "all sensor points in a network should produce some sensor value"
      )
      val blockSize = event.toMap.values.head.head.values.length
      require(
        event.toMap.values.forall(_.forall(_.values.length == blockSize)),
        "all sensor points in a network produce the same number of sensor values"
      )
      require(
        event.toMap.values.forall(_.forall(_.samplingRate == samplingRate)),
        "all sensor points have a fixed known sample rate"
      )

      val sensorEvents = (0 until blockSize).map(block => SensorNetValue(event.toMap.mapValues(data => (0 until data.size).map(point => data(point).values(block)).toVector)))

      for (evt <- sensorEvents) {
        self.tell(evt, sender())
      }

    /**
     * We use a buffer to hold `SensorNetValue`s when there is no downstream demand
     */

    case event: SensorNetValue if buffer.size == maxBufferSize =>
      log.error(s"No demand for the actor publisher and we received a SensorNet event, so dropping $event")

    case event: SensorNetValue =>
      if (seenStop) {
        log.debug(s"Actor publisher has already received a stop message, so dropping $event")
      } else if (isActive && buffer.isEmpty && totalDemand > 0) {
        onNext((event, sender()))
      } else if (isActive) {
        buffer :+= (event, sender())
        deliverSensorNetValue()
      } else {
        log.warning(s"Actor publisher is inactive and we received a SensorNet event, so dropping $event")
      }

    case Request(_) =>
      if (!calledOnComplete) {
        deliverSensorNetValue()
        if (seenStop && buffer.isEmpty) {
          onComplete()
          calledOnComplete = true
        }
      }

    // Allows the actor publisher's stream to be closed or completed
    case 'Stop =>
      if (!calledOnComplete) {
        seenStop = true
        if (buffer.isEmpty) {
          onComplete()
          calledOnComplete = true
        }
      }
  }

  @tailrec private def deliverSensorNetValue(): Unit = {
    if (totalDemand > 0) {
      // totalDemand is a Long and could be larger than what buf.splitAt can accept
      if (totalDemand <= Int.MaxValue) {
        val (use, keep) = buffer.splitAt(totalDemand.toInt)
        buffer = keep
        use.foreach { case (snv, ref) => onNext((snv, ref)) }
      } else {
        val (use, keep) = buffer.splitAt(Int.MaxValue)
        buffer = keep
        use.foreach { case (snv, ref) => onNext((snv, ref)) }
        deliverSensorNetValue()
      }
    }
  }

}
