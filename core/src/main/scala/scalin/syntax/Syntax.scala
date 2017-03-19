package scalin
package syntax

trait AssignSyntax {

  implicit def assignOps[A](lhs: A): AssignOps[A] = new AssignOps[A](lhs)

}

trait AllSyntax
    extends AssignSyntax
