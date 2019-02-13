package bitbucket.org.mstr93.GAPS.geneticAlgorithm

import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Master

class GeneticAlgorithm(val popSize: Int,
                       val crossProb: Double,
                       val mutProb: Double,
                       val generation: Vector[Individual]) {
  val bestSolution: Individual = generation.max

  def calculateSequentialStatic(iterations: Int): GeneticAlgorithm =
    calculate(iterations)(sequentialNewGeneration, staticParams)

  def calculateSequentialDynamic(iterations: Int): GeneticAlgorithm =
    calculate(iterations)(sequentialNewGeneration, dynamicParams)

  def calculateParallelStatic(iterations: Int): GeneticAlgorithm =
    calculate(iterations)(parallelNewGeneration, staticParams)

  def calculateParallelDynamic(iterations: Int): GeneticAlgorithm =
    calculate(iterations)(parallelNewGeneration, dynamicParams)

  private def calculate(iterations: Int)
                       (newGenCalcType: GeneticAlgorithm => Vector[Individual],
                        adapt: (Int, Int) => (Double, Double)): GeneticAlgorithm = {
    calculateGen(this, iterations, iterations)(newGenCalcType, adapt)
  }

  private def calculateGen(oldGA: GeneticAlgorithm,
                           iterationsLeft: Int,
                           totalIterations: Int)
                          (newGenCalcType: GeneticAlgorithm => Vector[Individual],
                           adapt: (Int, Int) => (Double, Double)): GeneticAlgorithm = {
    //    PRINTING
    //    println("Population: " + oldGA.generation.mkString(", "))
    //    println(oldGA.solution())

    if (iterationsLeft > 0) {
      val newGeneration = newGenCalcType(oldGA)
      val (newMutProb, newCrossProb) =
        adapt(iterationsLeft, totalIterations)

      val nextGA = GeneticAlgorithm(
        oldGA.popSize,
        newCrossProb,
        newMutProb,
        newGeneration)

      nextGA.calculateGen(nextGA,
        iterationsLeft - 1,
        totalIterations)(newGenCalcType, adapt)
    }
    else
      this
  }

  private def sequentialNewGeneration(oldGA: GeneticAlgorithm): Vector[Individual] = {
    def newGenTail(newGen: Vector[Individual],
                   reminder: Int): Vector[Individual] = {
      if (reminder > 0) {
        val (child1, child2) = pairOfChildren()
        newGenTail(
          Vector(child1, child2) ++ newGen,
          reminder - 2)
      } else {
        newGen
      }
    }

    val newGen = newGenTail(
      Vector(oldGA.bestSolution), popSize - 1)
    if (newGen.size > oldGA.generation.size)
      newGen.takeRight(popSize)
    else
      newGen
  }

  private def parallelNewGeneration(oldGA: GeneticAlgorithm): Vector[Individual] =
    Master.runParallel(oldGA)

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

  def takePotentialParents(): (Individual, Individual) =
    (chooseBetterInd(), chooseBetterInd())

  private def chooseBetterInd(): Individual = {
    val candidate1 = chooseRandomInd()
    val candidate2 = chooseRandomInd()

    List(candidate1, candidate2).max
  }

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

  def solution(): String =
    "Best Solution: " + bestSolution + " Fitness: " + bestSolution.fitness

  override def toString: String =
    "Population size: " + popSize +
      "\nCross probability: " + crossProb +
      "\nMutation probability: " + mutProb
}

object GeneticAlgorithm {
  def apply(popSize: Int,
            crossProb: Double,
            mutProb: Double,
            genomeLen: Int,
            fitFunc: Vector[Boolean] => Double): GeneticAlgorithm = {
    new GeneticAlgorithm(popSize,
      crossProb,
      mutProb,
      generateRandomPop(popSize,
        genomeLen, fitFunc))
  }

  private def apply(popSize: Int,
                    crossProb: Double,
                    mutProb: Double,
                    generation: Vector[Individual]): GeneticAlgorithm = {
    new GeneticAlgorithm(popSize,
      crossProb,
      mutProb,
      generation)
  }

  private def generateRandomPop(popSize: Int,
                                genomeLen: Int,
                                fitFunc: Vector[Boolean] => Double
                               ): Vector[Individual] =
    Vector.fill(popSize)(Individual(genomeLen, fitFunc))
}