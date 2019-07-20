package bitbucket.org.mstr93.gaps.algorithm

import bitbucket.org.mstr93.gaps.domain.Individual

trait GeneticAlgorithm {
  def calculate(iterations: Int): GeneticAlgorithm

  def generation: Seq[Individual]

  def bestIndividual: Individual
}
