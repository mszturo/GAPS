package bitbucket.org.mstr93.gaps.domain

import bitbucket.org.mstr93.gaps.domain.Individual._

class Individual(val genotype: Vector[Bit],
                 private val fitFunc: Individual => Double)
  extends Ordered[Individual] {
  private lazy val genomeLen = genotype.length
  lazy val fitness: Double = fitFunc(this)

  def this(length: Int,
           fitFunc: Individual => Double) {
    this(generateRandomGenotype(length), fitFunc)
  }

  //  MUTATION
  def mutate(mutProb: Double): Individual = {
    Individual(
      genotype.map(gene => mutateGene(gene, mutProb)),
      fitFunc
    )
  }

  private def mutateGene(gene: Bit,
                         mutProb: Double):Bit = {
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
      case One => "1"
      case Zero => "0"
    }
      .foldLeft("")((s1, s2) => s1 + s2)

  override def compare(that: Individual): Int =
    this.fitness.compare(that.fitness)
}

object Individual {
  private val defaultMutProb: Double = 0.0

  def apply(genotype: Vector[Bit],
            fitFunc: Individual => Double): Individual =
    new Individual(genotype, fitFunc)

  def apply(length: Int,
            fitFunc: Individual => Double): Individual =
    new Individual(length, fitFunc)

  private def generateRandomGenotype(length: Int): Vector[Bit] = {
    Vector.fill(length)(randomGene)
  }

  private def randomGene: Bit = {
    if (math.random < 0.5)
      Zero
    else
      One
  }

  private def produceChild(parent1: Individual, parent2: Individual, pivot: Int, mutProb: Double = defaultMutProb): Individual = {
    val leftGenome = parent1.genotype.take(pivot + 1)
    val rightLen = parent2.genomeLen - pivot - 1
    val rightGenome = parent2.genotype.takeRight(rightLen)
    Individual(leftGenome ++ rightGenome, parent1.fitFunc)
      .mutate(mutProb)
  }
}
