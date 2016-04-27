# scalin
Exact linear algebra in Scala/Spire

For performance reasons, both mutable and immutable vectors/matrices - many algorithms in the literature are mutable.

The mutable variants act as builders, so a .result() creates an immutable variant.

Only dense vector/matrices are implemented for now. 

The instances are not specialized: the focus is exact linear algebra, and scalars are expected to be `<: AnyRef`. 
Specialization is also tricky to get right in generic code. For example, higher-kinded types do not mix well with specialization.

In the future, a more general kind of specialization will be implemented, to support also generic value-like types, such
as small finite fields encoded using `AnyVal`s.

Very sparse documentation and few tests. In the meantime, have a look at the `tut` tutorial files:

https://github.com/denisrosset/scalin/tree/master/core/src/main/tut
