Design document for Scalin
==========================

- default column major ordering, as in Matlab
- indexing is 0-based, as in Numpy
- core concepts: matrices and column vectors
- copy on write semantics like Matlab (safer), unlike Breeze
- left-hand side assignments/mutable operations are converted to `update`-like methods by macros
- case use: exact linear algebra, so no specialization, and we assume that scalar operations are
  expensives. Scalin is not optimized at all for primitive types; there, Breeze should be used.

Type safety of Scalin
---------------------

Writing a linear algebra library in Scala requires some trade-offs. We made the following choices:

- for the scalars, we use the type classes from Spire; we reuse the type safety design from Spire:
  undefined mathematical operations can throw, for example when dividing by zero, or when 
  reaching a complexity limit.
- exceptions are propagated to the callee. Matrices and vectors on the right-hand side should never
  be modified; those on the left-hand side of a assignment can (and will) be left in an inconsistent
  state.
- matrix-matrix, vector-matrix, matrix-vector, vector-vector products are only defined when the shapes
  of their arguments are compatible. Exceptions are used to signal undefined operations.
- in the Breeze library, slices of vectors and matrices are read- and write-through. In the Scalin library,
  slices are never read-through, and slices of mutable objects are copies. Slicing operations on the left-hand
  side of a `:=` operation are syntax sugar for calls to the relevant `set` method.
- `scalin.Mat` and `scalin.Vec` instances are generic in the sense that they can be mutable or immutable. Use
  and request the types `scalin.immutable.Mat` or `scalin.immutable.Vec` to force immutability.
- all matrices and vectors inherit from respective generic base traits in the `scalin` package. The generic
  `scalin.Mat` and `scalin.Vec` instances are universally valid arguments for operations in `scalin`, i.e.
  the sum of two generic matrices is generic, mutable or immutable depending on the implicit `MatRing`
  in scope.


Valid scalars
-------------

To avoid combinatorial explosion of the type classes, vectors and matrics can be constructed out
of the following scalar types:

- (non-commutative or commutative) multiplicative monoids,
- (non-commutative or commutative) rings,
- real or complex fields.

Operation classes
-----------------

### Opaque elements

For these operations, vectors and matrices are simple containers, and Scalin vectors are not much
different from `IndexedSeq`s, except that the storage of Scalin vectors can be optimized depending
on the content (sparsity, element type).

The following operations are possible on vectors, without requiring any implicits:

- on `scalin.Vec[A]`: element retrieval using `v(ind)` where `ind` can be an `Int`, 
  a `Seq[Int]` or a mask `Vec[Boolean]`. This is translated by the Scala compiler
  to a call to the overloaded `apply` method: `v.apply(ind)`.
  
- on `scalin.mutable.Vec[A]` : assignment is done using the syntax `v(ind) := el`, where `ind`
  can again be an `Int`, a `Seq[Int]` or a mask `Vec[Boolean]`. This is translated
  by the syntax helper in `scalin.syntax.assign` to `v.set(ind, el)`. The type of
  `el` can either be a scalar `A` or a vector `scalin.Vec[A]` of compatible size.
