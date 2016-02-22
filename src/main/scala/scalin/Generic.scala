package scalin

trait Generic {

  type AsMutable <: mutable.Mutable
  type AsImmutable <: immutable.Immutable

  def mutableCopy: AsMutable

  def toImmutable: AsImmutable

}
