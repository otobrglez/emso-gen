package emsogen

object EMSO:
  private val factors = Array(7, 6, 5, 4, 3, 2, 7, 6, 5, 4, 3, 2)

  def verify(emso: String): Boolean =
    if emso == null || emso.length != 13 then return false

    if !emso.forall(c => c >= '0' && c <= '9') then return false

    val sum = (0 until 12).map(i => (emso(i) - '0') * factors(i)).sum

    val controlDigit = if sum % 11 == 0 then 0 else 11 - (sum % 11)

    controlDigit == emso(12) - '0'

  def generateFast(count: Int): List[String] =
    val random = scala.util.Random
    (1 to count).map { _ =>
      val day      = f"${random.nextInt(28) + 1}%02d"
      val month    = f"${random.nextInt(12) + 1}%02d"
      val year     = f"${random.nextInt(100) + 950}%03d".takeRight(3)
      val region   = f"${50 + random.nextInt(10)}%02d" // Slovenia
      val serial   = f"${random.nextInt(1000)}%03d"
      val base     = s"$day$month$year$region$serial"
      val checksum = calculateChecksum(base)

      if checksum <= 9 then base + checksum.toString
      else generateFast(1).head
    }.toList

  private def calculateChecksum(base: String): Int =
    val sum = base.zipWithIndex.map((c, i) => (c - '0') * factors(i)).sum
    if sum % 11 == 0 then 0 else 11 - (sum % 11)
