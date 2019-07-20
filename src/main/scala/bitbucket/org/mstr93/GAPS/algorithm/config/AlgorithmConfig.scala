package bitbucket.org.mstr93.gaps.algorithm.config

trait AlgorithmConfig {
  def populationSize: Int
  def crossProbability: Double
  def mutationProbability: Double
  def adaptiveRates: Boolean
  def betterHalf: Boolean
}
