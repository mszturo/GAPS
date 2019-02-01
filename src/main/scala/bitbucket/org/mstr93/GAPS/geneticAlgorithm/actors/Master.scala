package bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors

import akka.actor.{Actor, ActorSystem, Props}
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.{GeneticAlgorithm, Individual}
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Master._
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Supervisor.PartialGeneration

class Master(oldGen: Vector[Individual], crossProb: Double, mutProb: Double) extends Actor {
  val popSize: Int = oldGen.length
  //  val treeHeight: Double = floor(log(popSize) / log(2)).toInt
  var wholeGeneration: Vector[Individual] = Vector.empty

  override def receive: Receive = {
    case WholeGeneration(generation) =>
    //TODO
    case PartialGeneration(part) =>
      wholeGeneration = part ++ wholeGeneration
      val currentSize = wholeGeneration.length
      if (currentSize == popSize)
        self ! WholeGeneration(wholeGeneration)

      if (currentSize > popSize) {
        println("New generation exceeded by " +
          (currentSize - popSize))
        new Exception().printStackTrace()
      }
  }
}

object Master {
  def runParallel(algorithm: GeneticAlgorithm): Vector[Individual] = {
    val system = ActorSystem()
    val master = system.actorOf(Props(classOf[Master],
      algorithm.generation, algorithm.crossProb, algorithm.mutProb))


    Vector.empty
  }

  case class WholeGeneration(generation: Vector[Individual])

}