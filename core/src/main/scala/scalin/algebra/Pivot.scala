package scalin
package algebra

import spire.math._

trait Pivot[A] extends Any {

  /** Function used to determine the priority of pivot element selection.
    * Higher values are chosen in priority. 
    * 
    * Required properties:
    * - `pivotPriority(a) >= 0`,
    * - `pivotPriority(a) == 0` if and only if `a == 0`.
    * 
    */
  def priority(a: A): Double

  /** Returns whether `a` is close to zero, up to chosen tolerance. */
  def closeToZero(a: A): Boolean

}

object Pivot {

  val tolerance = 1e-10

  def double(tolerance: Double): Pivot[Double] = new Pivot[Double] {

    def priority(x: Double) = x.abs

    def closeToZero(a: Double) = a.abs < tolerance

  }

  implicit object safeLong extends Pivot[SafeLong] {

    def priority(x: SafeLong) = x.toDouble.abs // TODO: implement simplest denominator/numerator bitlength selection

    def closeToZero(x: SafeLong) = x.isZero
  }

  implicit object rational extends Pivot[Rational] {

    def priority(x: Rational) = x.toDouble.abs // TODO: implement simplest denominator/numerator bitlength selection

    def closeToZero(x: Rational) = x.isZero
  }

}
