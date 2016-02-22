package scalin

import scala.annotation.tailrec

import spire.syntax.cfor._

object Printer {

  val terminalHeight = 80
  val terminalWidth = 80
  val newline = "\n"

  // TODO: allow for value elision when vector is too big
  @inline def vec[A](vec: Vec[A]): String = {
    val sb = new StringBuilder
    sb.append("Vec(")
    var prefix = ""
    cforRange(0 until vec.length) { k =>
      sb.append(prefix)
      sb.append(vec(k).toString)
      prefix = ", "
    }
    sb.append(")")
    sb.toString
  }

  @inline def mat[A](mat: Mat[A],
    maxLines: Int = terminalHeight,
    maxWidth: Int = terminalWidth): String = {
    val rows = mat.rows
    val cols = mat.cols
    val showRows = if (rows > maxLines) maxLines - 1 else rows

    @tailrec def colWidth(c: Int, r: Int = 0, width: Int = 0): Int =
      if (r < showRows) {
        val a = mat.apply(r, c)
        if (a == null)
          colWidth(c, r + 1, spire.math.max(width, 3))
        else
          colWidth(c, r + 1, spire.math.max(width, a.toString.length + 2))
      } else width

    val colWidths = new scala.collection.mutable.ArrayBuffer[Int]
    var colWidthsSum = 0
    var c = 0
    while (c < cols && colWidthsSum < maxWidth) {
      val cw = colWidth(c)
      colWidthsSum += cw
      colWidths += colWidth(c)
      c += 1
    }

    // make space for "... (K total)"
    if (colWidths.size < cols) {
      val addWidth = cols.toString.length + 12
      while (colWidthsSum + addWidth  >= maxWidth) {
        if (colWidths.isEmpty) {
          return "%d x %d matrix".format(rows, cols)
        }
        val ind = colWidths.size - 1
        colWidthsSum -= colWidths(ind)
        colWidths.remove(ind)
      }
    }

    val rv = new scala.StringBuilder
    cforRange(0 until showRows) { r =>
      cforRange(0 until colWidths.size) { c =>
        val cell = if (mat(r, c) != null) mat(r, c).toString else "--"
        rv.append(" " * (colWidths(c) - cell.length))
        rv.append(cell)
      }
      if (colWidths.size < cols) {
        rv.append("...")
        if (r == 0) {
          rv.append(" (")
          rv.append(cols)
          rv.append(" total)")
        }
      }
      if (r + 1 < showRows)
        rv.append(newline)
    }

    if (rows > showRows) {
      rv.append(newline)
      rv.append("... (")
      rv.append(rows)
      rv.append(" total)")
    }

    rv.toString
  }

}
