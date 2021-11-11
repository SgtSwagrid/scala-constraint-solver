package solver

import solver.Variable
import solver.constraint.Constraint
import solver.solution.{PartialSolution, Solution}

import scala.annotation.tailrec

class Problem[T](
  val variables: Set[Variable[T]],
  val constraints: Set[Constraint[T]]
):
  
  val constraining: Map[Variable[T], Set[Constraint[T]]] =
    variables.map(v => v -> constraints.filter(_.variables.contains(v))).toMap
  
  @tailrec
  private def reductionSearch(
    soln: PartialSolution[T],
    queue: Set[(Variable[T], Constraint[T])]
  ): PartialSolution[T] =
    
    queue.headOption match
      case None => soln
      
      case Some(variable, constraint) =>
        
        val reduced = constraint.reduce(soln, variable)
        
        val variables = constraint.variables.filter { v =>
          reduced.values(v).size < soln.values(v).size
        }
        
        val modified = for
          v <- variables - variable
          c <- constraining(v) - constraint
        yield (v, c)
        
        reductionSearch(reduced, queue.tail ++ modified)
        
  private def backtrackingSearch(soln: PartialSolution[T]): LazyList[Solution[T]] =
  
    if (soln.invalid.nonEmpty) LazyList()
    else if (soln.free.isEmpty) LazyList(soln).flatMap(Solution.apply)

    else
      val variable = soln.free.head
      val values = soln.values(variable)
  
      val options = values.to(LazyList).map { value =>
        val guess = soln.remove(variable, values - value)
        val queue = constraining(variable).map((variable, _))
        reductionSearch(guess, queue)
      }
      
      options.sortBy(-_.entropy).flatMap(backtrackingSearch)
  
  def solve: LazyList[Solution[T]] =
    
    val queue = for
      c <- constraints
      v <- c.variables
    yield (v, c)
    
    val start = reductionSearch(PartialSolution(this), queue)
    backtrackingSearch(start)