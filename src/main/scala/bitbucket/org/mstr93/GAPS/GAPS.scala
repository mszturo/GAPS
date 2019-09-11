package bitbucket.org.mstr93.gaps

import bitbucket.org.mstr93.gaps.algorithm.GeneticAlgorithmImpl
import bitbucket.org.mstr93.gaps.algorithm.config.AlgorithmConfigImpl
import bitbucket.org.mstr93.gaps.domain.{Individual, One, Zero}

object GAPS extends App {
  //  testRandomIndividual()
  //  testCrossing(10)
  val mockGA = getTestGA
  val iterations = 500
  testGA(mockGA, iterations)

  def getTestGA: GeneticAlgorithmImpl = {
    val genomeLength = 12

    val testFunc: Individual => Double = { individual: Individual =>
      individual.genotype.map {
        case One => 1
        case Zero => 0
      }.sum
    }

    val config = new AlgorithmConfigImpl

    GeneticAlgorithmImpl(config, genomeLength, testFunc)
  }

  def testGA(ga: GeneticAlgorithmImpl, iterations: Int): Unit = {
    println("\nSINGLE THREAD")

    println("Generation 0")
    println("Population: " + ga.generation.mkString(", "))
    println(ga.bestIndividual)

    val finalGA = ga.calculateStatic(iterations)
    println("Generation " + iterations)
    println("Population: " + finalGA.generation.mkString(", "))
    println(finalGA.bestIndividual)
  }

  def testRandomIndividual(): Unit = {
    val testFunc = { ind: Individual => ind.genotype.length.toDouble }
    val individual: Individual = Individual(5, testFunc)
    println(individual)
  }

  def testCrossing(tries: Int): Unit = {
    val testFunc = { ind: Individual => ind.genotype.size.toDouble }
    val genome1 = Vector(One, One, One, One, One)
    val genome2 = Vector(Zero, Zero, Zero, Zero, Zero)
    val parent1 = Individual(genome1, testFunc)
    val parent2 = Individual(genome2, testFunc)

    println("Parents")
    println(parent1)
    println(parent2)

    for (i <- 1 to tries) {
      println("Try: " + i)
      val pair = parent1.cross(parent2)
      println(pair._1)
      println(pair._2)
    }
  }

}
