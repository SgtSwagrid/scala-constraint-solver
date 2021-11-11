package solver.constraint

import solver.Variable
import solver.solution.PartialSolution

class AllDifferent[T](vars: Set[Variable[T]]) extends Constraint[T](vars):
  
  def reduce(soln: PartialSolution[T], variable: Variable[T]): PartialSolution[T] =
    
    val cs = constrainingSets(
      soln,
      Set(variable),
      (variables - variable).toList,
      soln.values(variable)
    )
    
    cs.foldLeft(soln) { (s, cs) =>
      if (cs.span.size > cs.included.size)
        cs.included.foldLeft(s)((s, v) => s.remove(v, cs.span))
      else cs.excluded.foldLeft(s)((s, v) => s.remove(v, cs.span))
    }
  
  private case class ConstrainingSet(
    included: Set[Variable[T]],
    excluded: Set[Variable[T]],
    span: Set[T]
  )
  
  private def constrainingSets(
    soln: PartialSolution[T],
    included: Set[Variable[T]],
    remaining: List[Variable[T]],
    span: Set[T]
  ): Set[ConstrainingSet] =
  
    if (span.size > included.size + remaining.size) Set()
  
    else remaining match
    
      case next :: rest =>
        val include = constrainingSets(soln, included + next, rest, span | soln.values(next))
        val exclude = constrainingSets(soln, included, rest, span)
        include | exclude
    
      case Nil =>
        if (span.size <= included.size)
          Set(ConstrainingSet(included, variables -- included, span))
        else Set()