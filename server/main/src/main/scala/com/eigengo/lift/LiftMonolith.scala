package com.eigengo.lift

import akka.actor.{ActorPath, Props, ActorSystem}
import akka.io.IO
import com.eigengo.lift.exercise.ExerciseBoot
import com.eigengo.lift.notification.NotificationBoot
import com.eigengo.lift.profile.ProfileBoot
import com.typesafe.config.{ConfigFactory, Config}
import spray.can.Http
import spray.routing._

/**
 * A service that wraps together multiple routes
 * @param routes the routes to be combined
 */
class LiftMonolithService(routes: Route*) extends HttpServiceActor {
  override def receive: Receive = runRoute(routes.reduce(_ ~ _))
}

/**
 * Defines the lift monolith that constructs all services that make up the Lift
 * application.
 *
 * Subclasses must provide ``config`` and implement ``journalStartUp`` in the
 * appropriate fashion.
 */
trait LiftMonolith {

  val LiftActorSystem = "Lift"

  /**
   * Implementations must return the entire ActorSystem configuraiton
   * @return the configuration
   */
  def config: Config

  /**
   * Implementations can perform any logic required to start or join a journal
   * @param system the ActorSystem that needs the journal starting or looking up
   */
  def journalStartUp(system: ActorSystem): Unit

  /**
   * Starts up the Lift ActorSystem, binding Akka remoting to ``port`` and exposing all
   * rest services at ``0.0.0.0:restPort``.
   * @param port the Akka port
   * @param restPort the REST services port
   */
  final def actorSystemStartUp(port: Int, restPort: Int): Unit = {
    // Create an Akka system
    implicit val system = ActorSystem(LiftActorSystem, ConfigFactory.parseString(s"akka.remote.netty.tcp.port=$port").withFallback(config))

    // Startup the journal - typically this is only used when running locally with a levelDB journal
    journalStartUp(system)

    // boot the microservices
    val profile = ProfileBoot.boot
    val notification = NotificationBoot.boot
    val exercise = ExerciseBoot.boot(notification.notification, profile.userProfile)

    startupHttpService(system, restPort, exercise.route(system.dispatcher), profile.route(system.dispatcher))
  }

  /**
   * Startup the REST API handler
   * @param system the (booted) ActorSystem
   * @param port the port
   * @param routes the routes
   */
  private def startupHttpService(system: ActorSystem, port: Int, routes: Route*): Unit = {
    val restService = system.actorOf(Props(classOf[LiftMonolithService], routes))
    IO(Http)(system) ! Http.Bind(restService, interface = "0.0.0.0", port = port)
  }
}
