# GAPS

Genetic Algorithm Problem Solver

Single-threaded (currently working on concurrency) program to solve problems using genetic algorithm.

## Domain

**Individual**

Represents single solution to a problem with a sequence of 0's and 1's.

Defined with genome length: (Int) and function: (Vector[Boolean] => Double) to compute fitness of the solution.

Public interface contains methods:
 1) mutate: (Double => Individual) creates new Individual based on a mutation probability. 
 2) cross: ((Individual, Double) => (Individual, Individual)) produces pair of children from 
 crossing two Individuals.

**GeneticAlgorithm**

Defined with population size, crossing probability, mutation probability and a population 
(generation).

To create GA it is needed to provide fitness function: (Vector[Boolean] => Double) as well (*will 
be changed*).

Public interface contains methods:
 1) calculateSequentialStatic: (Int => GeneticAlgorithm) calculates given iterations in a single 
 thread without adjusting mutation and crossing probabilities.
 2) solution: (String) returns the solution with the highest fitness value of the generation 
 contained in this GeneticAlgorithm object.
 
> Working on implementation of:
> 1) calculateSequentialDynamic: (Int => GeneticAlgorithm)
> 2) calculateParallelStatic: (Int => GeneticAlgorithm)
> 3) calculateParallelDynamic: (Int => GeneticAlgorithm)
 
## Author

* **Michal Szturo** - *Initial work* - [mstr93](https://bitbucket.org/mstr93/)

## License

> Version 1.1, December 2016

> Copyright (C) 2018 Michal Szturo

Everyone is permitted to copy and distribute verbatim or modified
copies of this license document.

> TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

1. Do whatever you like with the original work, just don't be a dick.

   Being a dick includes - but is not limited to - the following instances:

 1a. Outright copyright infringement - Don't just copy this and change the name.
 
 1b. Selling the unmodified original with no work done what-so-ever, that's REALLY being a dick.
 
 1c. Modifying the original work to contain hidden harmful content. That would make you a PROPER dick.
 

2. If you become rich through modifications, related works/services, or supporting the original work,
share the love. Only a dick would make loads off this work and not buy the original work's
creator(s) a pint.

3. Code is provided with no warranty. Using somebody else's code and bitching when it goes wrong makes
you a DONKEY dick. Fix the problem yourself. A non-dick would submit the fix back.

## Acknowledgments

* **Phil Sturgeon** - *Providing a license* - [philsturgeon](https://github.com/philsturgeon)

* **Stephen McDonald** - *Providing examples for MergeSort in Akka* - [stephenmcd](https://github.com/stephenmcd)

