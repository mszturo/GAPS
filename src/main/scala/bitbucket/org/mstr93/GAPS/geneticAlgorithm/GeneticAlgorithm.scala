package bitbucket.org.mstr93.GAPS.geneticAlgorithm

import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Master

class GeneticAlgorithm(val popSize: Int,
                       val crossProb: Double,
                       val mutProb: Double,
                       val generation: Vector[Individual]) {
  val bestSolution: Individual = generation.max

  def calculate(iterations: Int): GeneticAlgorithm = {
    calculateGen(this, iterations, iterations)
  }

  private def calculateGen(oldGA: GeneticAlgorithm,
                           iterationsLeft: Int,
                           totalIterations: Int): GeneticAlgorithm = {
    //    PRINTING
    //    println("Best Solution: " + bestSolution +
    //      " Fitness: " + bestSolution.fitness)

    if (iterationsLeft > 0) {
      val nextGA = GeneticAlgorithm(
        oldGA.popSize,
        oldGA.adjustCrossProb(iterationsLeft,
          totalIterations),
        oldGA.adjustMutProb(iterationsLeft,
          totalIterations),
        newGeneration())

      nextGA.calculateGen(nextGA,
        iterationsLeft - 1,
        totalIterations)
    } else
      this
  }

  //  TODO optimize in the future
  private def adjustCrossProb(iterationsLeft: Int,
                              totalIterations: Int): Double = {
    crossProb
  }

  //  TODO optimize in the future
  private def adjustMutProb(iterationsLeft: Int,
                            totalIterations: Int): Double = {
    mutProb
  }

  private def newGeneration(): Vector[Individual] = {
    def newGenTail(newGen: Vector[Individual],
                   reminder: Int): Vector[Individual] = {
      if (reminder > 0) {
        val children = pairOfChildren()
        newGenTail(
          Vector(children._1,
            children._2) ++ newGen,
          reminder - 2)
      } else {
        newGen
      }
    }

    val newGen = newGenTail(
      Vector(bestSolution), popSize - 1)
    if (newGen.size > generation.size)
      newGen.takeRight(popSize)
    else
      newGen
  }

  private def pairOfChildren(): (Individual, Individual) = {
    val parent1 = chooseBetterInd()
    val parent2 = chooseBetterInd()

    if (math.random < crossProb)
      parent1.cross(parent2, mutProb)
    else
      (parent1.mutate(mutProb), parent2.mutate(mutProb))
  }

  private def chooseBetterInd(): Individual = {
    val candidate1 = chooseRandomFromBetterHalf()
    val candidate2 = chooseRandomFromBetterHalf()

    Vector(candidate1, candidate2).max
  }

  private def chooseRandomFromBetterHalf(): Individual = {
    val betterHalf = generation.takeRight(popSize / 2)
    val randIndex = util.Random.nextInt(popSize / 2 - 1)
    betterHalf(randIndex)
  }

  private def chooseRandomInd(): Individual = {
    val randIndex = util.Random.nextInt(popSize - 1)
    generation(randIndex)
  }

  //  TODO
  private def parallelNewGeneration(oldGen: Vector[Individual]
                                   ): Vector[Individual] = {
    def parallelNewGenTail(oldGen: Vector[Individual],
                           newGen: Vector[Individual],
                           reminder: Int): Vector[Individual] = {
      if (reminder > 0) {
        Master.runParallel(this)
      } else {
        newGen
      }
    }

    parallelNewGenTail(oldGen, Vector.empty, popSize)
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