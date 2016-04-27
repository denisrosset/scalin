# scalin
Exact linear algebra in Scala/Spire

For performance reasons, both mutable and immutable vectors/matrices are implemented, as many algorithms in the literature need mutation. The mutable variants act as builders, so a .result() creates an immutable variant.

Only dense vector/matrices are implemented for now. Care is taken that `hashCode` and `equals` is consistent across the different
matrix/vector implementations; it is not guaranteed to be consistent for different element types. Also, all matrices/vectors are
invariant in their scalar type.

The instances are not specialized: the focus is exact linear algebra, and scalars are expected to be `<: AnyRef`. 
Specialization is also tricky to get right in generic code. For example, higher-kinded types do not mix well with specialization.

For fast floating-point computation, use [Breeze](https://github.com/scalanlp/breeze) instead.

In the future, a more general kind of specialization will be implemented, to support also generic value-like types, such
as small finite fields encoded using `AnyVal`s.

Very sparse documentation and few tests. In the meantime, have a look at the `tut` tutorial files:

https://github.com/denisrosset/scalin/tree/master/core/src/main/tut
