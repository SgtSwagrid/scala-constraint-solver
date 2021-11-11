package sudoku

import solver.{Problem, Variable}
import solver.solution.Solution
import solver.constraint.AllDifferent

object SudokuSolver:
  
  def solve(sudoku: Sudoku): LazyList[Sudoku] =
    
    val vars = Vec2.positions.map { pos =>
      pos -> Variable(
        pos.toString,
        sudoku.digitAt(pos).map(Set(_)).getOrElse(Set.range(1, 9 + 1))
      )
    }.toMap
    
    val rows = Set.range(0, 9).map(y => AllDifferent(Vec2.row(y).map(vars)))
    val cols = Set.range(0, 9).map(x => AllDifferent(Vec2.col(x).map(vars)))
    val boxes = Vec2.regions.map(a => AllDifferent(Vec2.regions.map(b => vars(a * 3 + b))))
    
    val solutions: LazyList[Solution[Int]] =
      Problem(vars.values.toSet, rows ++ cols ++ boxes).solve
    
    solutions.map { soln =>
      val numbers =
        for (y <- 0 until 9; x <- 0 until 9)
          yield soln.values(vars(Vec2(x, y)))
      Sudoku(numbers *)
    }