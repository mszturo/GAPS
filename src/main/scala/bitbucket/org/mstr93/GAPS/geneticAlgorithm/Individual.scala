package bitbucket.org.mstr93.GAPS.geneticAlgorithm

import bitbucket.org.mstr93.GAPS.geneticAlgorithm.Individual._

class Individual(private val genotype: Vector[Boolean],
                 private val fitFunc: Vector[Boolean] => Double)
  extends Ordered[Individual] {
  private val genomeLen = genotype.length
  val fitness = fitFunc(genotype)

  def this(length: Int,
           fitFunc: Vector[Boolean] => Double) {
    this(generateRandomGenotype(length), fitFunc)
  }

  //  MUTATION
  def mutate(mutProb: Double): Individual = {
    Individual(
      genotype.map(gene => mutateGene(gene, mutProb)),
      fitFunc
    )
  }

  private def mutateGene(gene: Boolean,
                         mutProb: Double) = {
    if (math.random < mutProb)
      !gene
    else
      gene
  }

  //  CROSSING
  def cross(other: Individual, mutProb: Double = defaultMutProb): (Individual, Individual) = {
    val pivot = util.Random.nextInt(genomeLen - 1)
    (produceChild(this, other, pivot, mutProb),
      produceChild(other, this, pivot, mutProb))
  }

  override def toString: String =
    genotype.map {
      case true => "1"
      case false => "0"
    }
      .foldLeft("")((s1, s2) => s1 + s2)

  override def compare(that: Individual): Int =
    this.fitness.compare(that.fitness)
}

object Individual {
  private val defaultMutProb: Double = 0.0

  def apply(genotype: Vector[Boolean],
            fitFunc: Vector[Boolean] => Double): Individual =
    new Individual(genotype, fitFunc)

  def apply(length: Int,
            fitFunc: Vector[Boolean] => Double): Individual =
    new Individual(length, fitFunc)

  private def generateRandomGenotype(length: Int): Vector[Boolean] = {
    Vector.fill(length)(randomGene)
  }

  private def randomGene: Boolean = {
    math.random < 0.5
  }

  private def produceChild(parent1: Individual, parent2: Individual, pivot: Int, mutProb: Double = defaultMutProb): Individual = {
    val leftGenome = parent1.genotype.take(pivot + 1)
    val rightLen = parent2.genomeLen - pivot - 1
    val rightGenome = parent2.genotype.takeRight(rightLen)
    Individual(leftGenome ++ rightGenome, parent1.fitFunc)
      .mutate(mutProb)
  }
}
