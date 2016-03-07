package scalin
package algebra

import scala.reflect.ClassTag

import spire.algebra.Ring

trait Determinant[A] {

  def determinant(lhs: Mat[A]): A

}

object Determinant {

  /** Matrix-in-ring determinant algorithm from Mahajan and Vinay, see
    * http://cjtcs.cs.uchicago.edu/articles/1997/5/cj97-05.pdf
    * 
    * TODO: implement optimizations present in
    * https://github.com/gap-system/gap/blob/master/lib/matrix.gi
    */
  object MahajanVinay {

    implicit def mahajanVinay[A, M[A] <: mutable.Mat[A]](implicit MA: MatRing[A, M[A]]): Determinant[A] =
      new Determinant[A] {

        def determinant(lhs: Mat[A]): A = {
          import scala.Predef.{any2stringadd => _}
          import spire.syntax.cfor._
          import spire.syntax.ring._
          import scalin.syntax.all._
          import MA.scalar
          val n = lhs.rows
          require(lhs.cols == n)
          var current = new Array[mutable.Mat[A]](2)
          val b = n % 2
          current(b) = eye[A](n)
          current(1 - b) = zeros[A](n, n)
          var next = Array[mutable.Mat[A]](zeros[A](n, n), zeros[A](n, n))
          cforRange(0 to n - 2) { i =>
            cforRange(0 until n) { v =>
              cforRange(0 to v) { u =>
                cforRange(0 to 1) { p =>
                  cforRange(u + 1 until n) { w =>
                    // TODO: move macros to separate subproject, and restore syntax below
//                    next(p)(u,w) := next(p)(u, w) + current(p)(u, v) * lhs(v, w)
//                    next(1-p)(w,w) := next(1-p)(w,w) + current(p)(u,v) * lhs(v,u)
                    next(p).set(u,w,next(p)(u, w) + current(p)(u, v) * lhs(v, w))
                    next(1-p).set(w,w, next(1-p)(w,w) + current(p)(u,v) * lhs(v,u))
                  }
                }
              }
            }
            val temp = current
            current = next
            cforRange(0 until n) { v =>
              cforRange(0 until n) { u =>
                cforRange(0 to 1) { p =>
//                  temp(p)(u,v) := scalar.zero
                  temp(p).set(u,v, scalar.zero)
                }
              }
            }
            next = temp
          }
          var tplus = scalar.zero
          var tminus = scalar.zero
          cforRange(0 until n) { v =>
            cforRange(0 to v) { u =>
              tplus += current(1)(u, v) * lhs(v, u)
              tminus += current(0)(u, v) * lhs(v, u)
            }
          }
          tplus - tminus
        }

      }

  }
  
}
