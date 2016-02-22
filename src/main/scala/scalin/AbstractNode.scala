package scalin

import spire.util.Opt

trait AbstractNode {

  def intersectsMutable(rhs: mutable.Vec[_], ks: Opt[Range]): Boolean

  def intersectsMutable(rhs: mutable.Mat[_], rs: Opt[Range], cs: Opt[Range]): Boolean

  def intersectsMutable(rhs: mutable.Vec[_]): Boolean = intersectsMutable(vec, Opt.empty[Range])

  def intersectsMutable(rhs: mutable.Mat[_]): Boolean = intersectsMutable(mat, Opt.empty[Range], Opt.empty[Range])

}
