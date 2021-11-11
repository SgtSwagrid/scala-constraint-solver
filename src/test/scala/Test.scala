import scala.io.Source
import sudoku.{Sudoku, SudokuSolver}

object Test extends App:
  
  val input = Source.fromFile("sudoku.txt").getLines.mkString.map(_.asDigit)
  val sudoku = Sudoku(input*)
  val t0 = System.currentTimeMillis()
  for (i <- 1 to 100)
    println(SudokuSolver.solve(sudoku).head)
  val t1 = System.currentTimeMillis()
  println(s"Average solution time: ${(t1-t0)/100}ms.")