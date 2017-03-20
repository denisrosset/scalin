package scalin

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import spire.syntax.field._

import scalin.syntax.assign._

trait VecEngine[A, +VA <: Vec[A]] { self  =>

  implicit def VA: VecEngine[A, VA] = self

  //// Minimal methods to implement

  /** Creates a vector from length and a value function index => vec(index). */
  def tabulate(length: Int)(f: Int => A): VA

  /** Builds a vector from length and a user-provided function that mutates
    * a temporary mutable vector previously filled with the provided `default` value.
    */
  def fromMutable(length: Int, default: A)(updateFun: scalin.mutable.Vec[A] => Unit): VA

  /** Similar to fromMutable, but requires `updateFun` to update every element of the passed
    * mutable matrix. */

  def fromMutableUnsafe(length: Int)(updateFun: scalin.mutable.Vec[A] => Unit): VA

  /** Builds a vector from the processing applied on a mutable copy of the provided vector. */
  def fromMutable(vec: Vec[A])(updateFun: scalin.mutable.Vec[A] => Unit): VA =
    fromMutableUnsafe(vec.length) { res =>
      res(::) := vec
      updateFun(res)
    }

  //// Helper methods

  def pointwiseUnary(lhs: Vec[A])(f: A => A): VA = tabulate(lhs.length)(k => f(lhs(k)))

  def pointwiseBinary(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => A): VA = {
    require(lhs.length == rhs.length)
    tabulate(lhs.length)( k =>  f(lhs(k), rhs(k)) )
  }

  def booleanBinaryAnd(lhs: Vec[A], rhs: Vec[A])(f: (A, A) => Boolean): Boolean =
    (lhs.length == rhs.length) && {
      cforRange(0 until lhs.length) { k =>
        if (!f(lhs(k), rhs(k))) return false
      }
      true
    }

  def pointwiseBooleanUnary[B](lhs: Vec[B])(f: B => Boolean)(implicit ev: Boolean =:= A): VA = tabulate(lhs.length)( k => f(lhs(k)) )

  def pointwiseBooleanBinary[B](lhs: Vec[B], rhs: Vec[B])(f: (B, B) => Boolean)(implicit ev: Boolean =:= A): VA = {
    require(lhs.length == rhs.length)
    tabulate(lhs.length)( k => f(lhs(k), rhs(k)) )
  }

  type Ret <: VA // hack for the return type of Vec.flatten

  //// Creation

  def empty: VA = tabulate(0)(i => sys.error("Cannot be called"))

  def sparse(length: Int)(i: Vec[Int], v: Vec[A])(implicit A: Sparse[A]): VA =
    fromMutable(length, A.zero) { res =>
      require(i.length == v.length)
      cforRange(0 until i.length) { k =>
        res(i(k)) := v(k)
      }
    }

  def fill(length: Int)(a: => A): VA = tabulate(length)( k => a )

  // Alternative   def fillConstant(length: Int)(a: A): VA = fill(length)(a)

  def fillConstant(length: Int)(a: A): VA = fromMutable(length, a) { res =>
    // do nothing, already filled up
  }

  def fromSeq(elements: Seq[A]): VA = tabulate(elements.size)( elements(_) )

  def fromVec(vec: Vec[A]): VA = tabulate(vec.length)( k => vec(k) )

  //// Collection-like methods

  /* Alternative

  def cat(lhs: Vec[A], rhs: Vec[A]): VA = {
    val nl = lhs.length
    val nr = rhs.length
    tabulate(nl + nr)( k => if (k < nl) lhs(k) else rhs(k - nl) )
  }
   */

  def cat(lhs: Vec[A], rhs: Vec[A]): VA = {
    val nl = lhs.length
    val nr = rhs.length
    fromMutableUnsafe(nl + nr) { res =>
      res(0 until nl) := lhs
      res(nl until nl + nr) := rhs
    }
  }

  protected def catArray(array: Array[Vec[A]]): VA = {
    val n = array.length
    var len = 0
    cforRange(0 until n) { j =>
      len += array(j).length
    }
    fromMutableUnsafe(len) { res =>
      var i = 0
      cforRange(0 until n) { j =>
        val lenj = array(j).length
        res(i until (i + lenj)) := array(j)
        i += lenj
      }
    }
  }

  /* Alternative

  def flatMap[B](lhs: Vec[B])(f: B => Vec[A]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) map[A](f(lhs(0)))(identity)
    else {
      var acc: VA = cat(f(lhs(0)), f(lhs(1)))
      cforRange(2 until lhs.length) { k =>
        acc = cat(acc, f(lhs(k)))
      }
      acc
    }
   */

  def flatMap[B](lhs: Vec[B])(f: B => Vec[A]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) fromVec(f(lhs(0)))
    else {
      val els = new Array[Vec[A]](lhs.length)
      cforRange(0 until lhs.length) { j =>
        els(j) = f(lhs(j))
      }
      catArray(els)
    }

  /* Alternative

  def flatten[B <: Vec[A]](lhs: Vec[B]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) map[A](lhs(0))(identity)
    else {
      var acc = cat(lhs(0), lhs(1))
      cforRange(2 until lhs.length) { k =>
        acc = cat(acc, lhs(k))
      }
      acc
    }
   */

  def flatten[B <: Vec[A]](lhs: Vec[B]): VA =
    if (lhs.length == 0) empty
    else if (lhs.length == 1) fromVec(lhs(0))
    else {
      val els = new Array[Vec[A]](lhs.length)
      cforRange(0 until lhs.length) { j =>
        els(j) = lhs(j)
      }
      catArray(els)
    }

  def map[B](lhs: Vec[B])(f: B => A): VA = tabulate(lhs.length)( k => f(lhs(k)) )

  def colSeq(lhs: Mat[A]): IndexedSeq[VA] = IndexedSeq.tabulate(lhs.nCols)(c => colSlice(lhs, ::, c))

  def rowSeq(lhs: Mat[A]): IndexedSeq[VA] = IndexedSeq.tabulate(lhs.nRows)(r => rowSlice(lhs, r, ::))

  //// Slices

  def slice(vec: Vec[A], sub: Subscript): VA = {
    val ind = sub.forLength(vec.length)
    tabulate(ind.length)( k => vec(ind(k)) )
  }

  def slice(mat: Mat[A], sub: Subscript): VA = {
    val ind = sub.forLength(mat.nRows * mat.nCols)
    tabulate(ind.length) { k =>
      val ik = ind(k)
      val r = ik % mat.nRows
      val c = ik / mat.nRows
      mat(r, c)
    }
  }

  def diag(mat: Mat[A]): VA = tabulate(spire.math.min(mat.nRows, mat.nCols))( k => mat(k, k) )

  /** Slices a vector from a matrix, for the row `r` and column subscript `cs`. */
  def rowSlice(mat: Mat[A], r: Int, cs: Subscript): VA = {
    val ci = cs.forLength(mat.nCols)
    tabulate(ci.length)( k => mat(r, ci(k)) )
  }

  /** Slices a vector from a matrix, for the column `c` and the row subscript `rs`. */
  def colSlice(mat: Mat[A], rs: Subscript, c: Int): VA = {
    val ri = rs.forLength(mat.nRows)
    tabulate(ri.length)( k => mat(ri(k), c) )
  }

  //// With `Boolean =:= A`

  def pointwiseEqual[B](lhs: Vec[B], rhs: B)(implicit ev: Boolean =:= A): VA =
    pointwiseBooleanUnary(lhs)(_ == rhs)

  def pointwiseEqual[B](lhs: Vec[B], rhs: Vec[B])(implicit ev: Boolean =:= A): VA =
    pointwiseBooleanBinary(lhs, rhs)(_ == _)

  def pointwiseNotEqual[B](lhs: Vec[B], rhs: B)(implicit ev: Boolean =:= A): VA =
    pointwiseBooleanUnary(lhs)(_ != rhs)

  def pointwiseNotEqual[B](lhs: Vec[B], rhs: Vec[B])(implicit ev: Boolean =:= A): VA =
    pointwiseBooleanBinary(lhs, rhs)(_ != _)

  def pointwiseEqv[B](lhs: Vec[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): VA =
    pointwiseBooleanUnary(lhs)(_ === rhs)

  def pointwiseEqv[B](lhs: Vec[B], rhs: Vec[B])(implicit B: Eq[B], ev: Boolean =:= A): VA =
    pointwiseBooleanBinary(lhs, rhs)(_ === _)

  def pointwiseNeqv[B](lhs: Vec[B], rhs: B)(implicit B: Eq[B], ev: Boolean =:= A): VA =
    pointwiseBooleanUnary(lhs)(_ =!= rhs)

  def pointwiseNeqv[B](lhs: Vec[B], rhs: Vec[B])(implicit B: Eq[B], ev: Boolean =:= A): VA =
    pointwiseBooleanBinary(lhs, rhs)(_ =!= _)

  //// With `Eq[A]`

  def eqv(lhs: Vec[A], rhs: Vec[A])(implicit eqv: Eq[A]): Boolean = booleanBinaryAnd(lhs, rhs)(_ === _)

  //// REQUIRES
  //// MultiplicativeMonoid[A]

  //// Creation

  def ones(length: Int)(implicit A: MultiplicativeMonoid[A]): VA = fillConstant(length)(A.one)

  //// With `MultiplicativeMonoid[A]`, returning vector

  def times(lhs: A, rhs: Vec[A])(implicit A: MultiplicativeMonoid[A]): VA = pointwiseUnary(rhs)(lhs * _)

  def times(lhs: Vec[A], rhs: A)(implicit A: MultiplicativeMonoid[A]): VA = pointwiseUnary(lhs)(_ * rhs)

  def pointwiseTimes(lhs: Vec[A], rhs: Vec[A])(implicit A: MultiplicativeMonoid[A]): VA = pointwiseBinary(lhs, rhs)(_ * _)

  /* Alternative

  def kron(lhs: Vec[A], rhs: Vec[A])(implicit A: MultiplicativeMonoid[A]): VA =
    tabulate(lhs.length * rhs.length) { i =>
      val ri = i % rhs.length
      val li = i / rhs.length
      lhs(li) * rhs(ri)
    }
   */

  def kron(x: Vec[A], y: Vec[A])(implicit A: MultiplicativeMonoid[A]): VA = {
    val nx = x.length
    val ny = y.length
    fromMutableUnsafe(nx * ny) { res =>
      var i = 0
      cforRange(0 until nx) { ix =>
        cforRange(0 until ny) { iy =>
          res(i) := x(ix) * y(iy)
          i += 1
        }
      }
    }
  }

  //// REQUIRES
  //// Additive semigroup/monoid/group

  def zeros(length: Int)(implicit A: AdditiveMonoid[A]): VA = fillConstant(length)(A.zero)

  def plus(lhs: Vec[A], rhs: Vec[A])(implicit A: AdditiveSemigroup[A]): VA = pointwiseBinary(lhs, rhs)(A.plus)

  def negate(lhs: Vec[A])(implicit A: AdditiveGroup[A]): VA = pointwiseUnary(lhs)(A.negate)

  def minus(lhs: Vec[A], rhs: Vec[A])(implicit A: AdditiveGroup[A]): VA = pointwiseBinary(lhs, rhs)(A.minus)

  def pointwisePlus(lhs: Vec[A], rhs: A)(implicit A: AdditiveSemigroup[A]): VA = pointwiseUnary(lhs)(A.plus(_, rhs))

  def pointwiseMinus(lhs: Vec[A], rhs: A)(implicit A: AdditiveGroup[A]): VA = pointwiseUnary(lhs)(A.minus(_, rhs))

  //// REQUIRES
  //// Semiring

  /* Alternative

  def times(lhs: Vec[A], rhs: Mat[A]): VA = {
    val n = lhs.length
    require(n == rhs.nRows)
    if (n == 0)
      zeros(rhs.nCols)
    else
      tabulate(rhs.nCols) { c =>
        var sum = lhs(0) * rhs(0, c)
        cforRange(1 until n) { r =>
          sum += lhs(r) * rhs(r, c)
        }
        sum
      }
  }
   */

  /** Vector-matrix product. The vector is interpreted as a row vector. */
  def times(lhs: Vec[A], rhs: Mat[A])(implicit A: Semiring[A]): VA = {
    val n = lhs.length
    require(n == rhs.nRows)
    if (n == 0)
      zeros(rhs.nCols)
    else {
      fromMutableUnsafe(rhs.nCols) { res =>
        cforRange(0 until rhs.nCols) { c =>
          var sum = lhs(0) * rhs(0, c)
          cforRange(1 until n) { r =>
            sum += lhs(r) * rhs(r, c)
          }
          res(c) := sum
        }
      }
    }
  }

  /* Alternative

  def times(lhs: Mat[A], rhs: Vec[A]): VA = {
    val n = rhs.length
    require(n == lhs.nCols)
    if (n == 0)
      zeros(lhs.nRows)
    else
      tabulate(lhs.nRows) { r =>
        var sum = lhs(r, 0) * rhs(0)
        cforRange(1 until n) { c =>
          sum += lhs(r, c) * rhs(c)
        }
        sum
      }
  }

   */

  /** Matrix-vector product. The vector is interpreted as a column vector. */
  def times(lhs: Mat[A], rhs: Vec[A])(implicit A: Semiring[A]): VA = {
    val n = rhs.length
    require(n == lhs.nCols)
    if (n == 0)
      zeros(lhs.nRows)
    else {
      fromMutableUnsafe(lhs.nRows) { res =>
        cforRange(0 until lhs.nRows) { r =>
          var sum = lhs(r, 0) * rhs(0)
          cforRange(1 until n) { c =>
            sum += lhs(r, c) * rhs(c)
          }
          res(r) := sum
        }
      }
    }
  }

  /** Dot product. Equivalent to the real inner product, but not the complex inner product.*/
  def dot(lhs: Vec[A], rhs: Vec[A])(implicit A: Semiring[A]): A = {
    val n = lhs.length
    require(n == rhs.length)
    if (n == 0) A.zero
    else {
      var sum = lhs(0) * rhs(0)
      cforRange(1 until n) { k =>
        sum += lhs(k) * rhs(k)
      }
      sum
    }
  }

  //// REQUIRES
  //// EuclideanRing[A]

  // TODO: quot mod quotmod

  //// REQUIRES
  //// Field[A]

  def pointwiseDiv(lhs: Vec[A], rhs: Vec[A])(implicit A: Field[A]): VA = pointwiseBinary(lhs, rhs)(_ / _)

  def div(lhs: Vec[A], rhs: A)(implicit A: Field[A]): VA = pointwiseUnary(lhs)(_ / rhs)

}
