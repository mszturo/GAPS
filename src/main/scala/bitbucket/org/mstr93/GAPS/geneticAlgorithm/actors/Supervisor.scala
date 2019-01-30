package bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors

import akka.actor.{Actor, Props}
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.Individual

class Supervisor extends Actor {
  override def receive: PartialFunction[Any, Unit] = {
    case _ =>

  }
}

object Supervisor {
  def props = Props(classOf[Supervisor])

  case class PartialGeneration(generation: Vector[Individual])

  case class Children(child1: Individual, child2: Individual)

}
