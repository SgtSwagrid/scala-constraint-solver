import solver.Solver.*
import solver.SudokuSolver
import solver.SudokuSolver.*

object Test extends App:

  val s = Sudoku (
    4, 0, 7, 0, 0, 0, 1, 6, 2,
    0, 8, 0, 0, 0, 2, 4, 7, 0,
    9, 1, 2, 7, 6, 0, 0, 3, 8,
    0, 4, 0, 9, 0, 5, 6, 0, 0,
    0, 0, 5, 0, 2, 0, 9, 1, 0,
    7, 9, 0, 0, 0, 0, 2, 0, 4,
    6, 0, 0, 0, 0, 1, 3, 4, 0,
    0, 0, 8, 0, 4, 0, 0, 0, 6,
    0, 0, 0, 2, 5, 6, 0, 0, 0
  )
  
  println(SudokuSolver.solve(s).head)