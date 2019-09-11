package bitbucket.org.mstr93.gaps.algorithm

import bitbucket.org.mstr93.gaps.algorithm.config.AlgorithmConfig
import bitbucket.org.mstr93.gaps.domain.Individual

import scala.annotation.tailrec

case class GeneticAlgorithmImpl private(config: AlgorithmConfig, generation: Seq[Individual]) extends GeneticAlgorithm {

  val popSize: Int = config.populationSize
  val crossProb: Double = config.crossProbability
  val mutProb: Double = config.mutationProbability
  val adaptiveRates: Boolean = config.adaptiveRates
  val betterHalf: Boolean = config.betterHalf

  override val bestIndividual: Individual = generation.max

  override def calculate(iterations: Int): GeneticAlgorithm =
    if (config.adaptiveRates)
      calculateDynamic(iterations)
    else
      calculateStatic(iterations)

  def calculateStatic(iterations: Int): GeneticAlgorithmImpl =
    calculateGen(this, iterations, iterations, sequentialNewGeneration, staticParams)

  def calculateDynamic(iterations: Int): GeneticAlgorithmImpl =
    calculateGen(this, iterations, iterations, sequentialNewGeneration, dynamicParams)

  def takePotentialParents(): (Individual, Individual) =
    (chooseBetterInd(), chooseBetterInd())

  //  private def parallelNewGeneration(oldGA: GeneticAlgorithm): Seq[Individual] =
  //    Master.runParallel(oldGA)

  override def toString: String =
    "Population size: " + popSize +
      "\nCross probability: " + crossProb +
      "\nMutation probability: " + mutProb

  @tailrec
  private def calculateGen(oldGA: GeneticAlgorithmImpl,
                           iterationsLeft: Int,
                           totalIterations: Int,
                           newGenCalcType: GeneticAlgorithmImpl => Seq[Individual],
                           adapt: (Int, Int) => (Double, Double)): GeneticAlgorithmImpl = {
    //    PRINTING
    //    println("Population: " + oldGA.generation.mkString(", "))
    //    println(oldGA.solution())

    if (iterationsLeft > 0) {
      val newGeneration = newGenCalcType(oldGA)
      val (newMutProb, newCrossProb) = adapt(iterationsLeft, totalIterations)

      val nextGA = new GeneticAlgorithmImpl(config, newGeneration) {
        override val crossProb: Double = newCrossProb
        override val mutProb: Double = newMutProb
      }

      nextGA.calculateGen(
        nextGA,
        iterationsLeft - 1,
        totalIterations,
        newGenCalcType,
        adapt)
    }
    else
      this
  }

  private def sequentialNewGeneration(oldGA: GeneticAlgorithmImpl): Seq[Individual] = {
    @tailrec
    def newGenTail(newGen: Seq[Individual],
                   reminder: Int): Seq[Individual] = {
      if (reminder > 0) {
        val (child1, child2) = pairOfChildren()
        newGenTail(
          Seq(child1, child2) ++ newGen,
          reminder - 2)
      } else {
        newGen
      }
    }

    val newGen = newGenTail(
      Seq(oldGA.bestIndividual), popSize - 1)
    if (newGen.size > oldGA.generation.size)
      newGen.takeRight(popSize)
    else
      newGen
  }

  private def dynamicParams(iterationsLeft: Int,
                            totalIterations: Int): (Double, Double) = {
    (adjustCrossProb(iterationsLeft, totalIterations),
      adjustMutProb(iterationsLeft, totalIterations))
  }

  private def staticParams(iterationsLeft: Int,
                           totalIterations: Int): (Double, Double) = {
    (crossProb, mutProb)
  }

  //  TODO implement in the future
  private def adjustCrossProb(iterationsLeft: Int,
                              totalIterations: Int): Double = {
    crossProb
  }

  //  TODO implement in the future
  private def adjustMutProb(iterationsLeft: Int,
                            totalIterations: Int): Double = {
    mutProb
  }

  private def pairOfChildren(): (Individual, Individual) = {
    val (parent1, parent2) = takePotentialParents()

    if (math.random < crossProb)
      parent1.cross(parent2, mutProb)
    else
      (parent1.mutate(mutProb), parent2.mutate(mutProb))
  }

  private def chooseBetterInd(): Individual = {
    val candidate1 = chooseRandom()
    val candidate2 = chooseRandom()

    List(candidate1, candidate2).max
  }

  private def chooseRandom(): Individual =
    if (config.betterHalf)
      chooseRandomFromBetterHalf()
    else
      chooseRandomInd()

  private def chooseRandomInd(): Individual = {
    val randIndex = util.Random.nextInt(popSize - 1)
    generation(randIndex)
  }

  //  FOR FURTHER OPTIMIZATION
  private def chooseRandomFromBetterHalf(): Individual = {
    val betterHalf = generation.sorted.takeRight(popSize / 2)
    val randIndex = util.Random.nextInt(betterHalf.length)
    betterHalf(randIndex)
  }
}

object GeneticAlgorithmImpl {
  def apply(config: AlgorithmConfig,
            genomeLen: Int,
            fitFunc: Individual => Double): GeneticAlgorithmImpl = {
    GeneticAlgorithmImpl(
      config,
      generateRandomPop(config.populationSize, genomeLen, fitFunc))
  }

  private def generateRandomPop(popSize: Int,
                                genomeLen: Int,
                                fitFunc: Individual => Double
                               ): Seq[Individual] =
    Seq.fill(popSize)(Individual(genomeLen, fitFunc))
}