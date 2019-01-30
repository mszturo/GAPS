package bitbucket.org.mstr93.GAPS.geneticAlgorithm

import akka.actor._
import Breeder._
import BreederGuardian._

class BreederGuardian(_generation: List[Individual], crossProb: Double)
  extends Actor {
  private val popSize = _generation.length

  override def receive: PartialFunction[Any, Unit] = {
    case Start =>
      for (i <- 0 to popSize/2){
        val parent1 = chooseBetterInd()
        val parent2 = chooseBetterInd()

        if (math.random < crossProb)
          newGuardian ! Cross(parent1, parent2)
        else
          newGuardian ! PassOn(parent1, parent2)
      }
    case msg:Cross =>
      newBreeder ! msg
    case msg:PassOn =>
      newBreeder ! msg
//      TODO handle aggregating children
    case Children(child1, child2) =>
      newGuardian ! Generation(child1 :: child2 :: Nil)
    case Generation(generation) =>
      sender ! Generation(generation)
    case unknown =>
      println(getClass.getSimpleName + " received " + unknown.getClass)
      new Exception().printStackTrace()
  }

  def newBreeder:ActorRef = context.actorOf(Breeder.props())

  def newGuardian:ActorRef = context.actorOf(BreederGuardian.props(_generation, crossProb))

  private def chooseBetterInd(): Individual = {
    val candidate1 = chooseRandomInd()
    val candidate2 = chooseRandomInd()

    List(candidate1, candidate2).max
  }

  private def chooseRandomInd(): Individual = {
    val randIndex = util.Random.nextInt(popSize - 1)
    _generation(randIndex)
  }
}

object BreederGuardian {
  def init(generation: List[Individual], crossProb: Double): List[Individual] = {
    val guardian = ActorSystem().actorOf(props(generation, crossProb))

    guardian ! Start

    val newGen = List[Individual]()
    newGen
  }

  def props(generation: List[Individual], crossProb: Double) =
    Props(classOf[BreederGuardian], generation, crossProb)

  case object Start

  case class Generation(generation:List[Individual])
}



