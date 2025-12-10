import com.microsoft.z3.*
import emsogen.z3.*
import emsogen.z3.Ops.*
import zio.*
import java.time.LocalDate

private def currentDate: (Int, Int, Int) =
  val today = LocalDate.now()
  (today.getYear, today.getMonthValue, today.getDayOfMonth)

def emsoGen(n: Int = 1): Task[List[String]] = Z3Solver.withImplicitContext:
  // Global.setParameter("verbose", "10")

  val ctx: Context = summon[Context]

  val digits = (0 until 13).map(i => mkIntConst(s"digit-$i")).toArray[IntExpr]

  val solver = mkSolver()

  // All digits must be between 0 and 9
  for d <- digits do solver.add(d >= 0 && d <= 9)

  // 0 and 1 digits represent month (01-31)
  solver.add(digits(0) >= 0 && digits(0) <= 3)
  solver.add(digits(0) === 0 ==> digits(1) >= 1) // Day can't be 00
  solver.add(digits(0) === 3 ==> digits(1) <= 1) // Day max is 31

  // digits(2) and digits(3) represent month (01-12)
  solver.add(digits(2) >= 0 && digits(2) <= 1)
  solver.add(digits(2) === 0 ==> digits(3) >= 1) // Month can't be 00
  solver.add(digits(2) === 1 ==> digits(3) <= 2) // Month max is 12

  // digits(7), digits(8) represent registration region (10-99). Region must be >= 10
  solver.add(digits(7) * 10 + digits(8) >= 10)

  // digits(9), digits(10), digits(11) represent serial number
  // Serial number: digits 9-11, at least make it non-zero
  solver.add(digits(9) >= 1 || digits(10) >= 1 || digits(11) >= 1)

  // digits(12) is the control digit - apply the checksum formula
  val factorMap   = Array(7, 6, 5, 4, 3, 2, 7, 6, 5, 4, 3, 2)
  val weightedSum = (0 until 12).map(i => digits(i) * factorMap(i)).reduce(_ + _)

  // Valid dates
  // Helper: combine two digits into a number (e.g., digits(0)*10 + digits(1) = day)
  val day   = digits(0) * 10 + digits(1)
  val month = digits(2) * 10 + digits(3)

  // Day: 01-31 (basic range)
  solver.add(day >= 1 && day <= 31)

  // Month: 01-12
  solver.add(month >= 1 && month <= 12)

  // Month-specific day limits:
  // Months with 30 days: April(4), June(6), September(9), November(11)
  solver.add(month === 4 ==> day <= 30)
  solver.add(month === 6 ==> day <= 30)
  solver.add(month === 9 ==> day <= 30)
  solver.add(month === 11 ==> day <= 30)

  // February: max 29 days (we can't easily check leap year with only 3-year digits)
  solver.add(month === 2 ==> day <= 29)

  // Current date
  val (currentYear, currentMonth, currentDay) = currentDate
  val year3                                   = digits(4) * 100 + digits(5) * 10 + digits(6)
  val minYear3                                = (currentYear - 120) % 1000 // 905 (for 1905)
  val maxYear3                                = currentYear         % 1000 // 025 (for 2025)
  solver.add(
    (year3 >= minYear3 && year3 <= 999) || // 1905-1999
      (year3 >= 0 && year3 <= maxYear3)    // 2000-2025
  )
  // Not future
  val isCurrentYear  = year3 === maxYear3
  val monthNotFuture = month < currentMonth || (month === currentMonth && day <= currentDay)

  solver.add(isCurrentYear ==> monthNotFuture)

  // Control digit formula: if sum % 11 == 0 then 0 else 11 - (sum % 11)
  val remainder = mkMod(weightedSum, ctx.mkInt(11))
  solver.add(remainder !== 1)

  val controlDigit = ctx
    .mkITE(remainder === 0, mkInt(0), 11 - remainder)
    .asInstanceOf[ArithExpr[IntSort]]

  solver.add(digits(12) === controlDigit)

  // trololo
  // Sum of all 13 digits equals 42
  // solver.add(mkAdd(digits*) === 42)

  val results = collection.mutable.ListBuffer[String]()
  while results.size < n && solver.check() == Status.SATISFIABLE do
    val model  = solver.getModel
    val values = digits.map(d => model.evaluate(d, false).asInstanceOf[IntNum].getInt)
    val emso   = values.mkString
    results += emso

    // Block this solution - at least one digit must be different
    solver.add(ctx.mkOr(digits.zip(values).map((d, v) => d !== v)*))

  results.toList

object NumbersDemoApp extends ZIOAppDefault:
  def run = for
    _       <- zio.Console.printLine("Generating numbers")
    numbers <- emsoGen(10)
    _        = numbers.foreach(println(_))
  yield ()
