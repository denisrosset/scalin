package scalin
package syntax

final class AssignOps[A](lhs: A) {

  import macros.AssignMacro

  def :=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

  // Semigroup
  def |+|:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def |-|:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

  // Ring
  def +:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def -:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def *:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def **:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

  // EuclideanRing
  def /~:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def %:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

  // Field
  def /:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

  // BooleanAlgebra
  def ^:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def |:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def &:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

  // BitString
  def <<:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def >>:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def >>>:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

  // VectorSpace
  def *::=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def :*:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def :/:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

  // GroupAction
  def <|+|:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def <+:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]
  def <*:=[B](rhs: B): Unit = macro AssignMacro.updateOp[B]

}
