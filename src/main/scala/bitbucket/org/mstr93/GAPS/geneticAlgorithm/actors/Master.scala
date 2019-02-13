package bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import bitbucket.org.mstr93.GAPS.geneticAlgorithm._
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Master._
import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Supervisor._

import scala.concurrent.Await
import scala.concurrent.duration._

class Master(algorithm: GeneticAlgorithm) extends Actor {
  override def receive: Receive = {
    case Initiate =>
      val supervisors = algorithm.popSize / 4 + 1
      createAndSendToSupervisors(supervisors)
      context.become(aggregating)
      self ! PartialGeneration(Vector(algorithm.bestSolution))
    case unknown =>
      errorHandling(unknown)
  }

//  TODO maybe think about reducing producing unnecessary supervisors?
  private def createAndSendToSupervisors(number: Int): Unit = {
    if (number > 0) {
      val supervisor = context.actorOf(
        Props(classOf[Supervisor], algorithm))
      supervisor ! InitiateSupervisor
      createAndSendToSupervisors(number - 1)
    }
  }

  def aggregating: Receive = {
    case PartialGeneration(firstPart) =>
      val firstPartSize = firstPart.length
      if (firstPartSize == algorithm.popSize) {
        context.become(done)
        self ! WholeGeneration(firstPart)
      }
      else if (firstPartSize > algorithm.popSize) {
        self ! PartialGeneration(firstPart.take(firstPartSize - 1))
      }
      else {
        context.become {
          case PartialGeneration(secondPart) =>
            context.become(aggregating)
            self ! PartialGeneration(firstPart ++ secondPart)
        }
      }
    case unknown =>
      errorHandling(unknown)
  }

  def done: Receive = {
    case WholeGeneration(generation) =>
      println("WHOLE")
      println(generation)
//      TODO
      context.stop(self)
    case _:PartialGeneration =>
    case unknown =>
      errorHandling(unknown)
  }

  private def errorHandling(unknown: Any): Unit ={
    println(getClass + " received " + unknown.getClass)
    new Exception().printStackTrace()
    context.stop(self)
  }
}

object Master {
  val system = ActorSystem()

  def runParallel(algorithm: GeneticAlgorithm): Vector[Individual] = {
    val master = system.actorOf(Props(classOf[Master],
      algorithm))

    //    TODO get the result Vector out of actor system
    implicit val timeout: Timeout = Timeout(5 seconds)
    val future = master ? Initiate
    val result = Await.result(future, timeout.duration)
      .asInstanceOf[WholeGeneration]
    println("Dpa" + result)

    result.generation
  }

  case object Initiate

  case class WholeGeneration(generation: Vector[Individual])

}