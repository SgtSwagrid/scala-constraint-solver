package solver.solution

import solver.Variable
import solver.Problem
import PartialSolution.*

case class PartialSolution[T] (
  values: Map[Variable[T], Set[T]],
  free: Vector[Variable[T]],
  fixed: Set[Variable[T]],
  invalid: Set[Variable[T]],
  entropy: Int,
  previous: Option[PartialSolution[T]]
):
  
  def remove(variable: Variable[T], cutset: Set[T]): PartialSolution[T] =
    
    val removed = (values(variable) & cutset).size
    if (removed == 0) this else
      
      val remaining = values(variable) -- cutset
      val newValues = values + (variable -> remaining)
      
      val updated = remaining.size match
        case 0 => copy(free = free.filter(_ != variable), invalid = invalid + variable)
        case 1 => copy(free = free.filter(_ != variable), fixed = fixed + variable)
        case _ => copy(free = free.sortBy(newValues(_).size))
  
      updated.copy(
        values = newValues,
        entropy = entropy - removed,
        previous = Some(this)
      )

object PartialSolution:
  
  def apply[T](problem: Problem[T]): PartialSolution[T] =
    
    val domain = problem.variables.map(v => v -> v.domain.toSet).toMap
    
    PartialSolution(
      values = domain,
      free = problem.variables.filter(domain(_).size > 1).toVector,
      fixed = problem.variables.filter(domain(_).size == 1),
      invalid = problem.variables.filter(domain(_).size == 0),
      entropy = domain.values.map(_.size).sum,
      previous = None
    )