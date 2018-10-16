package scalin.util

import spire.algebra.{Field, Signed}
import spire.syntax.signed._
import spire.syntax.field._

class ComplexDivision[@specialized(Double, Float) A:Field:Signed] {
  var cdivr: A = Field[A].zero
  var cdivi: A = Field[A].zero
  def cdiv(xr: A, xi: A, yr: A, yi: A): Unit = {
    var r = Field[A].zero
    var d = Field[A].zero
    if (yr.abs > yi.abs) {
      r = yi / yr
      d = yr + r * yi
      cdivr = (xr + r * xi) / d
      cdivi = (xi - r * xr) / d
    }
    else {
      r = yr / yi
      d = yi + r * yr
      cdivr = (r * xr + xi) / d
      cdivi = (r * xi - xr) / d
    }
  }
}
