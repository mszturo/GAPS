package bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors

import akka.actor.Actor
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Breeder._
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.{GeneticAlgorithm, Individual}

class Breeder(algorithm: GeneticAlgorithm) extends Actor {
  override def receive: PartialFunction[Any, Unit] = {
    case Cross(parent1, parent2) =>
      val (child1, child2) = parent1.cross(parent2)
      sender ! Children(Vector(
        child1.mutate(algorithm.mutProb),
        child2.mutate(algorithm.mutProb)))
      context.stop(self)
    case PassOn(parent1, parent2) =>
      sender ! Children(Vector(
        parent1.mutate(algorithm.mutProb),
        parent2.mutate(algorithm.mutProb)))
      context.stop(self)
    case unknown =>
      errorHandling(unknown)
  }

  private def errorHandling(unknown: Any): Unit ={
    println(getClass + " received " + unknown.getClass)
    new Exception().printStackTrace()
    context.stop(self)
  }
}

object Breeder {

  case class Cross(parent1: Individual, parent2: Individual)

  case class PassOn(parent1: Individual, parent2: Individual)

  case class Children(pair: Vector[Individual])

}