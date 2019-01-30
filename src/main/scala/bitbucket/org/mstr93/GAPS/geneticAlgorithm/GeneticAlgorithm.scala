package bitbucket.org.mstr93.GAPS.geneticAlgorithm

import bitbucket.org.mstr93.GAPS.geneticAlgorithm.actors.Master

class GeneticAlgorithm(private val popSize: Int,
                       private val crossProb: Double,
                       private val mutProb: Double,
                       private val genomeLen: Int,
                       private val fitFunc: Vector[Boolean] => Double,
                       private val generation: Vector[Individual],
                       private val iterationsLeft: Int) {
  val bestSolution: Individual = generation.max

  def calculate(): Unit = {
    calculate(this, iterationsLeft)
  }

  private def calculate(oldGA: GeneticAlgorithm,
                        totalIterations: Int): Unit = {
    println("Generation: " + generation)
    println("Best Solution: " + bestSolution +
      " Fitness: " + bestSolution.fitness)
    val newGen = newGeneration()
    val nextGA = GeneticAlgorithm(
      oldGA.popSize,
      oldGA.adjustCrossProb(totalIterations),
      oldGA.adjustMutProb(totalIterations),
      oldGA.genomeLen,
      oldGA.fitFunc,
      newGen,
      oldGA.iterationsLeft - 1)
    if (iterationsLeft > 0)
      nextGA.calculate(nextGA,
        totalIterations: Int)
  }

  //  TODO optimize in the future
  def adjustCrossProb(totalIterations: Int): Double = {
    crossProb
  }

  //  TODO optimize in the future
  def adjustMutProb(totalIterations: Int): Double = {
    mutProb
  }

  def newGeneration(): Vector[Individual] = {
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
  def parallelNewGeneration(oldGen: Vector[Individual]): Vector[Individual] = {
    def parallelNewGenTail(oldGen: Vector[Individual],
                           newGen: Vector[Individual],
                           reminder: Int): Vector[Individual] = {
      if (reminder > 0) {
        Master.runParallel(oldGen, crossProb, mutProb)
      } else {
        newGen
      }
    }

    parallelNewGenTail(oldGen, Vector.empty, popSize)
  }

  override def toString: String =
    "Population size: " + popSize +
      "\nCross probability: " + crossProb +
      "\nMutation probability: " + mutProb +
      "\nGenome length: " + genomeLen
}

object GeneticAlgorithm {
  def apply(popSize: Int,
            crossProb: Double,
            mutProb: Double,
            genomeLen: Int,
            fitFunc: Vector[Boolean] => Double,
            generation: Vector[Individual],
            iterationsLeft: Int
           ): GeneticAlgorithm = {
    new GeneticAlgorithm(popSize,
      crossProb,
      mutProb,
      genomeLen,
      fitFunc,
      generation,
      iterationsLeft)
  }

  def apply(popSize: Int,
            crossProb: Double,
            mutProb: Double,
            genomeLen: Int,
            fitFunc: Vector[Boolean] => Double,
            iterationsLeft: Int
           ): GeneticAlgorithm = {
    new GeneticAlgorithm(popSize,
      crossProb,
      mutProb,
      genomeLen,
      fitFunc,
      generateRandomPop(popSize,
        genomeLen, fitFunc),
      iterationsLeft)
  }

  private def generateRandomPop(popSize: Int,
                                genomeLen: Int,
                                fitFunc: Vector[Boolean] => Double): Vector[Individual] =
    Vector.fill(popSize)(Individual(genomeLen, fitFunc))
}