package bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors

import akka.actor.{Actor, Props}
import bitbucket.org.mstr93.GAPS.geneticAlgorithm._
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Breeder._
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Supervisor._

//TODO implement tree of Supervisors with required number of Individuals
class Supervisor(algorithm: GeneticAlgorithm) extends Actor {
  override def receive: PartialFunction[Any, Unit] = {
    case InitiateSupervisor =>
      createAndSendToBreeders(2)
      context.become(aggregating)
    case unknown =>
      errorHandling(unknown)
  }

  private def createAndSendToBreeders(number: Int): Unit = {
    if (number > 0) {
      val breeder = context.actorOf(
        Props(classOf[Breeder], algorithm))
      val (parent1, parent2) = algorithm.takePotentialParents()

      if (math.random < algorithm.crossProb)
        breeder ! Cross(parent1, parent2)
      else
        breeder ! PassOn(parent1, parent2)

      createAndSendToBreeders(number - 1)
    }
  }

  def aggregating: Receive = {
    case Children(firstPart) =>
      println("got first breeder")
      context.become {
        case Children(secondPart) =>
          println("got second breeder")
          context.parent ! PartialGeneration(firstPart ++ secondPart)
          context.stop(self)
      }
    case unknown =>
      errorHandling(unknown)
  }

  private def errorHandling(unknown: Any): Unit = {
    println(getClass + " received " + unknown.getClass)
    new Exception().printStackTrace()
    context.stop(self)
  }
}

object Supervisor {

  case object InitiateSupervisor

  case class PartialGeneration(generation: Vector[Individual])

}
