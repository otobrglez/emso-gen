package emsogen.z3

import zio.*
import com.microsoft.z3.*

object Z3Solver:
  private def mkContext: RIO[Scope, Context] = ZIO.fromAutoCloseable(ZIO.attemptBlocking(new Context()))

  final def withContext[Out](f: Context => Out): Task[Out] = ZIO.scoped(mkContext.map(f))

  final def withImplicitContext[Out](f: Context ?=> Out): Task[Out] =
    ZIO.scoped(mkContext.map(ctx => f(using ctx)))

  def layer: RLayer[Scope, Context] = ZLayer.fromZIO(mkContext)
