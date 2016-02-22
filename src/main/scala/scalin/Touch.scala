package scalin

/** Describes which part of a matrix is used in the computation. */
final class Touch(val raw: Long) extends AnyVal { lhs =>

  def multiIfNotClean = lhs match {
    case Touch.Clean() => Touch.Clean()
    case _ => Touch.Multi()
  }

  override def toString = lhs match {
    case Touch.Clean() => "Touch.Clean()"
    case Touch.Row(row) => s"Touch.Row($row)"
    case Touch.Col(col) => s"Touch.Col($col)"
    case Touch.AsIs() => s"Touch.AsIs()"
    case Touch.Multi() => "Touch.Multi()"
  }

  @inline def isClean: Boolean = raw == 0L
  @inline def touchType: Int = (raw >> 32).toInt

  @inline def value: Int = raw.toInt

  @inline def merge(rhs: Touch): Touch =
    if (lhs.raw == rhs.raw || rhs.isClean) lhs
    else if (lhs.isClean) rhs
    else Touch.Multi()

}

object Touch {

  @inline final def CLEAN = 0
  @inline final def ROW = 1
  @inline final def COL = 2
  @inline final def ASIS = 3
  @inline final def MULTI = 4

  object Clean {
    def apply(): Touch = new Touch(CLEAN.toLong << 32)
    def unapply(d: Touch): Boolean = d.touchType == CLEAN
  }

  final class Row(val row: Int) extends AnyVal {
    def isEmpty = (row == -1)
    def get = row
  }

  object Row {
    def apply(row: Int): Touch = new Touch((ROW.toLong << 32) + row)
    def unapply(d: Touch): Row =
      if (d.touchType == ROW) new Row(d.value) else new Row(-1)
  }

  final class Col(val col: Int) extends AnyVal {
    def isEmpty = (col == -1)
    def get = col
  }

  object Col {
    def apply(col: Int): Touch = new Touch((COL.toLong << 32) + col)
    def unapply(d: Touch): Col =
      if (d.touchType == COL) new Col(d.value) else new Col(-1)
  }

  object AsIs {
    def apply(): Touch = new Touch(ASIS.toLong << 32)
    def unapply(d: Touch): Boolean = d.touchType == ASIS
  }

  object Multi {
    def apply(): Touch = new Touch(MULTI.toLong << 32)
    def unapply(d: Touch): Boolean = d.touchType == MULTI
  }

}
