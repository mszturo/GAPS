package bitbucket.org.mstr93.GAPS

import bitbucket.org.mstr93.GAPS.geneticAlgorithm.{GeneticAlgorithm, Individual}

object GAPS extends App {
  override def main(args: Array[String]): Unit = {
    //        testRandomIndividual
    //        testCrossing(10)
    testGA()
  }

  def testGA(): Unit = {
    val popSize = 12
    val crossProb = 0.9
    val mutProb = 0.3
    val genomeLength = 10
    val iterations = 50
    val testFunc = { xs: Vector[Boolean] =>
      xs.map {
        case true => 1.0
        case false => 0.0
      }.fold(0.0)((a, b) => a + b)
    }

    val ga = GeneticAlgorithm(popSize,
      crossProb,
      mutProb,
      genomeLength,
      testFunc,
      iterations)
    ga.calculate()
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
    var i = 0

    for (i <- 1 to tries) {
      println("Try: " + i)
      val pair = parent1.cross(parent2)
      println(pair._1)
      println(pair._2)
    }
  }

}
