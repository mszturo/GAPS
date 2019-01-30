package bitbucket.org.mstr93.GAPS.geneticAlgorithm

import akka.actor._
import Breeder._

class Breeder() extends Actor {
  override def receive: PartialFunction[Any, Unit] = {
    case Cross(parent1, parent2) =>
      val children = parent1.cross(parent2)
      sender ! Children(children._1, children._2)
    case PassOn(parent1, parent2) =>
      sender ! Children(parent1, parent2)
    case unknown =>
      println("Breeder received " + unknown.getClass)
      new Exception().printStackTrace()
  }
}

object Breeder {
  def props() = Props(classOf[Breeder])

  case class Cross(parent1: Individual, parent2: Individual)

  case class PassOn(parent1: Individual, parent2: Individual)

  case class Children(child1: Individual, child2: Individual)
}
