Design document for Scalin
==========================

- default column major ordering, as in Matlab
- indexing is 0-based, as in Numpy
- core concepts: matrices and column vectors
- copy on write semantics like Matlab (safer), unlike Breeze
- left-hand side assignments/mutable operations are converted to `update`-like methods by macros

Emphasis on exact computations

Operations that require a `Ring`
--------------------------------
