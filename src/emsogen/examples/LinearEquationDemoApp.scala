import com.microsoft.z3.*
import emsogen.z3.*
import emsogen.z3.Ops.*
import zio.*

// solve x > 2 and y < 10 and x + 2y = 7
private def demoEquation = Z3Solver.withImplicitContext:
  val ctx: Context = summon[Context]

  val (x, y) = mkIntConst("x") -> mkIntConst("y")

  val solver = mkSolver()
  solver.add(x > 2)
  solver.add(y < 10)
  solver.add(x + 2 * y === 7)

  solver.check() match
    case Status.SATISFIABLE =>
      val model = solver.getModel

      println("Solution found:")
      val xVal = model.evaluate(x, false).asInstanceOf[IntNum].getInt64
      val yVal = model.evaluate(y, false).asInstanceOf[IntNum].getInt64
      println(s"(example solution) x = $xVal, y = $yVal")
    case status             =>
      println("No solution found: " + status)

object LinearEquationDemoApp extends ZIOAppDefault:
  def run = demoEquation
