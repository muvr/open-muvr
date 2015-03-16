package com.eigengo.lift

import collection.JavaConversions._
import akka.actor._
import akka.persistence.journal.leveldb.{SharedLeveldbJournal, SharedLeveldbStore}
import akka.util.Timeout
import com.eigengo.lift.common.MicroserviceApp.MicroserviceProps
import com.typesafe.config.ConfigFactory
import spray.routing.{HttpServiceActor, Route}

import scala.concurrent.Await

/**
 * CLI application for the exercise app
 */
object LiftLocalMonolithApp extends App with LiftMonolith {
  var store: Option[ActorRef] = None

  lazy val config = {
    val microserviceProps = MicroserviceProps("Lift")
    val clusterShardingConfig = ConfigFactory.parseString(s"akka.contrib.cluster.sharding.role=${microserviceProps.role}")
    val clusterRoleConfig = ConfigFactory.parseString(s"akka.cluster.roles=[${microserviceProps.role}]")

    clusterShardingConfig
      .withFallback(clusterRoleConfig)
      .withFallback(ConfigFactory.load("main.conf"))
  }

  def getOrStartStore(system: ActorSystem): ActorRef = this.synchronized {
    store.getOrElse {
      val ref = system.actorOf(Props[SharedLeveldbStore], "store")
      store = Some(ref)
      ref
    }
  }

  override def journalStartUp(system: ActorSystem): Unit = {
    // Start the shared journal one one node (don't crash this SPOF)
    // This will not be needed with a distributed journal
    val ref = getOrStartStore(system)
    SharedLeveldbJournal.setStore(ref, system)
  }

  val ports = config.getIntList("akka.cluster.jvm-ports")
  ports.foreach(port ⇒ actorSystemStartUp(port, 10000 + port))
}
