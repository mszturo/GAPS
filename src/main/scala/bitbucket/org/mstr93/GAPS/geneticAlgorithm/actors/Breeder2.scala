package bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors

import akka.actor.Actor
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.Individual
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Breeder2._

class Breeder2(mutProb: Double) extends Actor {
  override def receive: PartialFunction[Any, Unit] = {
    case Cross(parent1, parent2) =>
      val children = parent1.cross(parent2)
      val child1 = children._1.mutate(mutProb)
      val child2 = children._2.mutate(mutProb)
      sender ! Children(child1, child2)
    case PassOn(parent1, parent2) =>
      sender ! Children(parent1.mutate(mutProb),
        parent2.mutate(mutProb))
    case unknown =>
      println("Breeder received " + unknown.getClass)
      new Exception().printStackTrace()
  }
}

object Breeder2 {

  case class Cross(parent1: Individual, parent2: Individual)

  case class PassOn(parent1: Individual, parent2: Individual)

  case class Children(child1: Individual, child2: Individual)

}