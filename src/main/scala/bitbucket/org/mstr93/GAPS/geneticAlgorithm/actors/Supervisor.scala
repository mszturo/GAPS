package bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors

import akka.actor.{Actor, Props}
import bitbucket.org.mstr93.GAPS.geneticAlgorithm._
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Breeder._
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Supervisor._

class Supervisor(algorithm: GeneticAlgorithm, neededInds: Int) extends Actor {
  override def receive: Receive = {
    case InitiateSupervisor =>
      neededInds match {
        case 1 =>
          createAndSendToBreeders()
          context.become(awaitingChildren)
        case 2 =>
          createAndSendToBreeders()
          context.become(awaitingChildren)
        case x =>
          if (x > 0) {
            val (left, right) = split(neededInds)
            createSupervisor(left) ! InitiateSupervisor
            createSupervisor(right) ! InitiateSupervisor
          } else {
            context.stop(self)
          }
          context.become(aggregatingPartialGens)
      }
    case unknown =>
      errorHandling(unknown)
  }

  private def createSupervisor(neededInds: Int) =
    context.actorOf(Props(classOf[Supervisor], algorithm, neededInds))

  private def createAndSendToBreeders(): Unit = {
    if (neededInds > 0) {
      val breeder = context.actorOf(Props(classOf[Breeder], algorithm))
      val (parent1, parent2) = algorithm.takePotentialParents()

      if (math.random < algorithm.crossProb)
        breeder ! Cross(parent1, parent2)
      else
        breeder ! PassOn(parent1, parent2)
    }
  }

  private def aggregatingPartialGens: Receive = {
    case PartialGeneration(firstPart) =>
      println("got first partGen")
      context.become {
        case PartialGeneration(secondPart) =>
          println("got second partGen")
          context.parent ! PartialGeneration(firstPart ++ secondPart)
          context.stop(self)
      }
    case unknown =>
      errorHandling(unknown)
  }

  private def awaitingChildren: Receive = {
    case Children(children) =>
      println("got from breeder")
      if (neededInds == 1)
        context.parent ! PartialGeneration(children.drop(1))
      else
        context.parent ! PartialGeneration(children)
      context.stop(self)
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
  def split(length: Int): (Int, Int) = {
    val first = length / 2
    val second = length - first
    (first, second)
  }

  case object InitiateSupervisor

  case class PartialGeneration(generation: Vector[Individual])

}
