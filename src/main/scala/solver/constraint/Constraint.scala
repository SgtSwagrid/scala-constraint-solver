package solver.constraint

import solver.Variable
import solver.solution.PartialSolution

trait Constraint[T](val variables: Set[Variable[T]]):
  
  def reduce(soln: PartialSolution[T], variable: Variable[T]): PartialSolution[T]