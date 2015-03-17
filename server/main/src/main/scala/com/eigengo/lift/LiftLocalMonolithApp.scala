package com.eigengo.lift

import collection.JavaConversions._
import akka.actor._
import akka.persistence.journal.leveldb.{SharedLeveldbJournal, SharedLeveldbStore}
import com.eigengo.lift.common.MicroserviceApp.MicroserviceProps
import com.typesafe.config.ConfigFactory

/**
 * CLI application for the exercise app
 */
object LiftLocalMonolithApp extends App with LiftMonolith {
  private var store: Option[ActorRef] = None

  lazy val config = {
    val microserviceProps = MicroserviceProps("Lift")
    val clusterShardingConfig = ConfigFactory.parseString(s"akka.contrib.cluster.sharding.role=${microserviceProps.role}")
    val clusterRoleConfig = ConfigFactory.parseString(s"akka.cluster.roles=[${microserviceProps.role}]")

    clusterShardingConfig
      .withFallback(clusterRoleConfig)
      .withFallback(ConfigFactory.load("main.conf"))
  }

  // Guaranteed to be race-free as long it is called only `journalStartUp` which in turn is called only from
  // `actorSystemStartUp` which is called in a `foreach` in the constructor below
  private def getOrStartStore(system: ActorSystem): ActorRef =
    store.getOrElse {
      val ref = system.actorOf(Props[SharedLeveldbStore], "store")
      store = Some(ref)
      ref
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
