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

- generic objects the vectors and matrices are then dumb containers,
- (non-commutative or commutative) multiplicative monoids,
- (non-commutative or commutative) rings,
- Euclidean rings,
- real fields.

There is (yet) no special support for complex rings/fields such as Gaussian integers, rationals or 
cyclotomics. However, we try to avoid ambiguities in methods: the "dot product" operation is named
"dot product" and not "inner product"; the product of a column vector with its transpose is called
"dyadic product" and not "outer product".

Operations that combine a type class in the hierarchy above and another instance such as `Order` or
`Eq` are defined as methods that take additional evidence; we thus avoid combinatorial explosion of
instances such as `MatRing[A, MA] with MatEq[A, MA]`, `MatRing[A, MA] with MatOrder[A, MA]`, etc...

Where is the code for operation X
---------------------------------

The location of the code performing an operation depends on the type of the 
result of the operation:

- if the result is an instance of `Vec[A]`, the code is located in a `VecTrait[A, VA]`/`VecRing[A, VA]` 
  implicit for some `VA <: Vec[A]`, the exact `VA` type depends on the selected implicit,
- if the result is an instance of `Mat[A]`, the code is located in a `MatTrait[A, MA]`/`MatRing[A, MA]`
  implicit for some `MA <: Mat[A]`, the exact `MA` type depends on the selected implicit,
- if the result is a scalar `A` and the operation involves a single `Vec[A]` or `Mat[A]` instance, the
  code is located in the method in an implicit `VecXXX[A, _]` or `MatXXX[A, _]`,
- if the result is a scalar `A` and the operation involves two `Vec[A]`, or two `Mat[A]` instances, a
  `VecXXX[A, _]` or `MatXXX[A, _]` implicit is used,
- if the result is a scalar `A` and the operation involves mixed arguments, for example one `Vec[A]` and
  one `Mat[A]`, the code is located in a `MatXXX[A, _]` implicit (`Mat[A]` is supposedly the
  most complex object involved, which would benefit most from optimizations).

