package bitbucket.org.mstr93.gaps.algorithm.config

import com.typesafe.config.ConfigFactory

class AlgorithmConfigImpl extends AlgorithmConfig {
  private val config = ConfigFactory.load().getConfig("gaps")
  override def populationSize: Int = config.getInt("population-size")

  override def crossProbability: Double = config.getDouble("cross-probability")

  override def mutationProbability: Double = config.getDouble("mutation-probability")

  override def adaptiveRates: Boolean = config.getBoolean("optimization.adaptive-rates")

  override def betterHalf: Boolean = config.getBoolean("optimization.better-half")
}
