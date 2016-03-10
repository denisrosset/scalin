An introduction to Scalin
=========================

Let's start with some imports:
```tut:silent
import spire.implicits._ // for the spire.std instances
import spire.math.{Rational, SafeLong} // exact types used
import scalin.syntax.assign._ // for the := syntax
import scalin.syntax.build._ // for the zeros/ones/eye/fill/vec/rowMajor/colMajor/rowMat/colMat helper functions
import scalin.mutable.dense._ // to specify that the matrices and vectors returned are dense and mutable
```

Now we can construct matrices:

```tut
val m1 = zeros[Rational](3, 3)
val m2 = eye[SafeLong](3)
val v1 = zeros[Int](3)
val m3 = rowMajor[Rational](4, 4)(
  1,3,1,4,
  2,7,3,9,
  1,5,3,1,
  1,2,0,8)
val m4 = m3 * 2
```

All these matrices have a mutable type. If we had used the `scalin.immutable.dense._` import,
these matrices would be immutable instead. The import only specifies the return type,
computations can be done on a mix of mutable/immutable instances.

```tut
val fact = m3.rankFactorization
fact.rref
val C = fact.matC(m3) // recover the C matrix from rank factorization
val F = fact.matF // recover the F matrix from rank factorization
(C * F).pointwise === m3
m1
m1(0,::) := m3(0, 0 to 2)
m1
```

