package emsogen.z3
import com.microsoft.z3.*

object Ops:
  // Solver
  def mkSolver()(using ctx: Context): Solver = ctx.mkSolver()

  // Constants
  def mkIntConst(name: String)(using ctx: Context): IntExpr   = ctx.mkIntConst(name)
  def mkBoolConst(name: String)(using ctx: Context): BoolExpr = ctx.mkBoolConst(name)
  def mkRealConst(name: String)(using ctx: Context): RealExpr = ctx.mkRealConst(name)

  // Literals
  def mkInt(v: Int)(using ctx: Context): IntNum        = ctx.mkInt(v)
  def mkInt(v: Long)(using ctx: Context): IntNum       = ctx.mkInt(v)
  def mkBool(v: Boolean)(using ctx: Context): BoolExpr = ctx.mkBool(v)

  // Arithmetic
  def mkAdd(args: ArithExpr[? <: ArithSort]*)(using ctx: Context): ArithExpr[? <: ArithSort]             =
    ctx.mkAdd(args.asInstanceOf[Seq[Expr[? <: ArithSort]]]*)
  def mkMul(args: ArithExpr[? <: ArithSort]*)(using ctx: Context): ArithExpr[? <: ArithSort]             =
    ctx.mkMul(args.asInstanceOf[Seq[Expr[? <: ArithSort]]]*)
  def mkSub(args: ArithExpr[? <: ArithSort]*)(using ctx: Context): ArithExpr[? <: ArithSort]             =
    ctx.mkSub(args.asInstanceOf[Seq[Expr[? <: ArithSort]]]*)
  def mkMod(left: ArithExpr[?], right: ArithExpr[?])(using ctx: Context): IntExpr                        =
    ctx.mkMod(left.asInstanceOf[IntExpr], right.asInstanceOf[IntExpr])
  def mkSum(args: ArithExpr[?]*)(using ctx: Context): IntExpr                                            =
    ctx.mkAdd(args.asInstanceOf[Seq[Expr[? <: ArithSort]]]*).asInstanceOf[IntExpr]
  def mkITE(cond: BoolExpr, thenExpr: ArithExpr[?], elseExpr: ArithExpr[?])(using ctx: Context): IntExpr =
    ctx.mkITE(cond, thenExpr, elseExpr).asInstanceOf[IntExpr]
  
  // Comparisons
  def mkEq(left: Expr[?], right: Expr[?])(using ctx: Context): BoolExpr           = ctx.mkEq(left, right)
  def mkGt(left: ArithExpr[?], right: ArithExpr[?])(using ctx: Context): BoolExpr = ctx.mkGt(left, right)

  def mkLt(left: ArithExpr[?], right: ArithExpr[?])(using ctx: Context): BoolExpr = ctx.mkLt(left, right)
  def mkGe(left: ArithExpr[?], right: ArithExpr[?])(using ctx: Context): BoolExpr = ctx.mkGe(left, right)
  def mkLe(left: ArithExpr[?], right: ArithExpr[?])(using ctx: Context): BoolExpr = ctx.mkLe(left, right)

  // Boolean
  def mkAnd(args: BoolExpr*)(using ctx: Context): BoolExpr = ctx.mkAnd(args*)
  def mkOr(args: BoolExpr*)(using ctx: Context): BoolExpr  = ctx.mkOr(args*)
  def mkNot(arg: BoolExpr)(using ctx: Context): BoolExpr   = ctx.mkNot(arg)

  // Implicit conversions
  given intToZ3(using ctx: Context): Conversion[Int, IntNum] = ctx.mkInt(_)

  // Infix syntax (exp left)
  extension (left: ArithExpr[?])(using ctx: Context)
    def >(right: ArithExpr[?]): BoolExpr                  = ctx.mkGt(left, right)
    def >(right: Int): BoolExpr                           = ctx.mkGt(left, ctx.mkInt(right))
    def <(right: ArithExpr[?]): BoolExpr                  = ctx.mkLt(left, right)
    def <(right: Int): BoolExpr                           = ctx.mkLt(left, ctx.mkInt(right))
    def >=(right: ArithExpr[?]): BoolExpr                 = ctx.mkGe(left, right)
    def >=(right: Int): BoolExpr                          = ctx.mkGe(left, ctx.mkInt(right))
    def <=(right: ArithExpr[?]): BoolExpr                 = ctx.mkLe(left, right)
    def <=(right: Int): BoolExpr                          = ctx.mkLe(left, ctx.mkInt(right))
    def ===(right: ArithExpr[?]): BoolExpr                = ctx.mkEq(left, right)
    def ===(right: Int): BoolExpr                         = ctx.mkEq(left, ctx.mkInt(right))
    def !==(right: ArithExpr[?]): BoolExpr                = ctx.mkNot(ctx.mkEq(left, right))
    def !==(right: Int): BoolExpr                         = ctx.mkNot(ctx.mkEq(left, ctx.mkInt(right)))
    def +(right: ArithExpr[?]): ArithExpr[? <: ArithSort] =
      ctx.mkAdd(left, right).asInstanceOf[ArithExpr[? <: ArithSort]]
    def +(right: Int): ArithExpr[? <: ArithSort]          =
      ctx.mkAdd(left, ctx.mkInt(right)).asInstanceOf[ArithExpr[? <: ArithSort]]
    def -(right: ArithExpr[?]): ArithExpr[? <: ArithSort] =
      ctx.mkSub(left, right).asInstanceOf[ArithExpr[? <: ArithSort]]
    def -(right: Int): ArithExpr[? <: ArithSort]          =
      ctx.mkSub(left, ctx.mkInt(right)).asInstanceOf[ArithExpr[? <: ArithSort]]
    def *(right: ArithExpr[?]): ArithExpr[? <: ArithSort] =
      ctx.mkMul(left, right).asInstanceOf[ArithExpr[? <: ArithSort]]
    def *(right: Int): ArithExpr[? <: ArithSort]          =
      ctx.mkMul(left, ctx.mkInt(right)).asInstanceOf[ArithExpr[? <: ArithSort]]

  extension (left: Int)(using ctx: Context)
    def +(right: ArithExpr[?]): ArithExpr[? <: ArithSort] =
      ctx.mkAdd(ctx.mkInt(left), right).asInstanceOf[ArithExpr[? <: ArithSort]]
    def -(right: ArithExpr[?]): ArithExpr[? <: ArithSort] =
      ctx.mkSub(ctx.mkInt(left), right).asInstanceOf[ArithExpr[? <: ArithSort]]
    def *(right: ArithExpr[?]): ArithExpr[? <: ArithSort] =
      ctx.mkMul(ctx.mkInt(left), right).asInstanceOf[ArithExpr[? <: ArithSort]]

  extension (left: BoolExpr)(using ctx: Context)
    def &&(right: BoolExpr): BoolExpr  = ctx.mkAnd(left, right)
    def ||(right: BoolExpr): BoolExpr  = ctx.mkOr(left, right)
    def unary_! : BoolExpr             = ctx.mkNot(left)
    def ==>(right: BoolExpr): BoolExpr = ctx.mkImplies(left, right) // implication
