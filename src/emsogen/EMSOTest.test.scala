package emsogen

import zio.*
import zio.test.*

object EMSOTest extends ZIOSpecDefault:

  def spec = suite("EMSOTest")(
    test("invalid") {
      assertTrue(
        !EMSO.verify(null) && !EMSO.verify("1")
      )
    },

    test("valid") {
      assertTrue(
        EMSO.verify("0101006500006") &&
          EMSO.verify("1406000000000")
      )
    }
  )
