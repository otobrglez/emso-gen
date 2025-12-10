package emsogen

object EMSO:
  def verify(emso: String): Boolean =
    if emso == null || emso.length != 13 then return false

    if !emso.forall(c => c >= '0' && c <= '9') then return false

    val factorMap = Array(7, 6, 5, 4, 3, 2, 7, 6, 5, 4, 3, 2)

    val sum = (0 until 12).map(i => (emso(i) - '0') * factorMap(i)).sum

    val controlDigit = if sum % 11 == 0 then 0 else 11 - (sum % 11)

    controlDigit == emso(12) - '0'