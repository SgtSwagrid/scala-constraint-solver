package solver.solution

import solver.Variable

case class Solution[T](
  values: Map[Variable[T], T],
  history: Seq[PartialSolution[T]]
)

object Solution:

  def apply[T](soln: PartialSolution[T]): Option[Solution[T]] =
  
    Option.when(soln.invalid.isEmpty && soln.free.isEmpty) {
      val values = soln.values.view.mapValues(_.head).toMap
      val history = Seq.unfold(soln)(_.previous.map(p => (p, p)))
      Solution(values, history)
    }