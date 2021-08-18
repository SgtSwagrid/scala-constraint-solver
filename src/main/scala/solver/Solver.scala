package solver

object Solver:
  
  case class Variable[T](name: String, domain: Set[T])
  
  sealed trait Constraint[T](val variables: Set[Variable[T]]):
    def reduce(variable: Variable[T], soln: PartialSolution[T]): PartialSolution[T]
  
  type Solution[T] = Map[Variable[T], T]
  type Solutions[T] = LazyList[Solution[T]]
  
  class Problem[T](
    val variables: Set[Variable[T]],
    val constraints: Set[Constraint[T]]
  ):
  
    val constraining: Map[Variable[T], Set[Constraint[T]]] =
      variables.map(v => v -> constraints.filter(_.variables.contains(v))).toMap
    
    def solve: Solutions[T] =
      
      def solveRec(soln: PartialSolution[T]): Solutions[T] =
        
        if (soln.invalid.nonEmpty) LazyList()
        else if (soln.free.isEmpty)
          LazyList(soln.values.view.mapValues(_.head).toMap)
  
        else
          soln.values(soln.free.head).to(LazyList)
            .map(v => soln.remove(soln.free.head, soln.values(soln.free.head) - v))
            .sortBy(-_.entropy)
            .flatMap(solveRec)
      
      solveRec(PartialSolution(this))
  
  case class PartialSolution[T] (
    problem: Problem[T],
    values: Map[Variable[T], Set[T]],
    free: Vector[Variable[T]],
    pivots: Set[Variable[T]],
    invalid: Set[Variable[T]],
    entropy: Int
  ):
    
    def remove(variable: Variable[T], cutset: Set[T]): PartialSolution[T] =
      
      val dEntropy = (values(variable) & cutset).size
      
      if (dEntropy == 0) this
      else
        
        val remaining = values(variable) -- cutset
        val newValues = values + (variable -> remaining)
        
        val updated = (remaining.size match
          case 0 => copy(free = free.filter(_ != variable), invalid = invalid + variable)
          case 1 => copy(free = free.filter(_ != variable), pivots = pivots + variable)
          case _ => copy(free = free.sortBy(newValues(_).size))
        ).copy(values = newValues, entropy = entropy - dEntropy)
        
        problem.constraining(variable)
          .foldLeft(updated)((s, c) => c.reduce(variable, s))
  
  object PartialSolution:
    
    def apply[T](problem: Problem[T]): PartialSolution[T] =
      
      val domain = problem.variables.map(v => v -> v.domain.toSet).toMap
      
      PartialSolution (
        problem = problem,
        values = domain,
        free = problem.variables.filter(domain(_).size > 1).toVector,
        pivots = problem.variables.filter(domain(_).size == 1),
        invalid = problem.variables.filter(domain(_).size == 0),
        entropy = domain.values.map(_.size).sum
      )
  
  case class Different[T](vars: Set[Variable[T]]) extends Constraint[T](vars.toSet) :
    
    def reduce(variable: Variable[T], soln: PartialSolution[T]): PartialSolution[T] =
      
      case class ConstrainingSet(
        included: Set[Variable[T]],
        excluded: Set[Variable[T]],
        span: Set[T]
      )
      
      def constrainingSets(
        included: Set[Variable[T]],
        remaining: List[Variable[T]],
        span: Set[T]
      ): Set[ConstrainingSet] =
        
        if (span.size > included.size + remaining.size) Set()
        
        else remaining match
          
          case next :: rest =>
            lazy val include = constrainingSets(included + next, rest, span | soln.values(next))
            lazy val exclude = constrainingSets(included, rest, span)
            include | exclude
          
          case Nil =>
            if (span.size <= included.size)
              Set(ConstrainingSet(included, variables -- included, span))
            else Set()
  
      val cs = constrainingSets(
        Set(variable),
        (variables - variable).toList,
        soln.values(variable)
      )
  
      cs.foldLeft(soln) { (s, cs) =>
        if (cs.span.size > cs.included.size)
          cs.included.foldLeft(s)((s, v) => s.remove(v, cs.span))
        else cs.excluded.foldLeft(s)((s, v) => s.remove(v, cs.span))
      }