package scalin
package mutable

/** Trait for objects that can be used to build immutable instances, without
  * copying overhead. After calling `.result()` on the builder to obtain
  * the immutable object, the builder instance is said to be exported, and
  *  cannot be used anymore.
  */
trait Builder extends Generic {

  def exported: Boolean

  def result(): AsImmutable

}
