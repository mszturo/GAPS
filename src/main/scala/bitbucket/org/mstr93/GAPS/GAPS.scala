package bitbucket.org.mstr93.gaps

import bitbucket.org.mstr93.gaps.algorithm.GeneticAlgorithmImpl
import bitbucket.org.mstr93.gaps.algorithm.config.AlgorithmConfigImpl
import bitbucket.org.mstr93.gaps.domain.Individual

object GAPS extends App {
  //  testRandomIndividual()
  //  testCrossing(10)
  val mockGA = getTestGA
  val iterations = 500
  testGA(mockGA, iterations)

  def getTestGA: GeneticAlgorithmImpl = {
    val genomeLength = 12

    val testFunc = { xs: Seq[Boolean] =>
      xs.map {
        case true => 1.0
        case false => 0.0
      }.fold(0.0)((a, b) => a + b)
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
    val testFunc = { xs: Vector[Boolean] => xs.length.toDouble }
    val individual: Individual = Individual(5, testFunc)
    println(individual)
  }

  def testCrossing(tries: Int): Unit = {
    val testFunc = { xs: Vector[Boolean] => xs.size.toDouble }
    val genome1 = Vector(true, true, true, true, true)
    val genome2 = Vector(false, false, false, false, false)
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
