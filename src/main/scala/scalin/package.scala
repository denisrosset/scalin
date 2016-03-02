import scala.language.experimental.macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}

package object scalin {

  object Ops {

    def mergeParts(parts: Seq[String]): String = parts.map(_.capitalize).mkString("")

    def decodeIfNotEmpty(str: String): String =
      if (str.isEmpty) str else spire.macros.Ops.operatorNames(str)

    def updateOp[B:c.WeakTypeTag](c: Context)(rhs: c.Expr[B]): c.Expr[Unit] = {
      import c.universe._
      val Pattern = """(.*)\$colon\$eq""".r
      def transform(tn: TermName): TermName = {
        val TermName(Pattern(op)) = tn//.toString
        TermName("set" + decodeIfNotEmpty(op).capitalize)
      }
      c.macroApplication match {
        case q"scalin.`package`.updateOps[$_]($lhs.apply(..$args)).$method[$_]($_)" =>
          c.Expr[Unit](q"$lhs.${transform(method)}(..$args, $rhs)")
        case q"scalin.`package`.updateOps[$_]($lhs).$method[$_]($_)" =>
          c.Expr[Unit](q"$lhs.${transform(method)}($rhs)")
        case _ => sys.error("Not implemented")
      }
    }

  }

  class UpdateOps[A](lhs: A) {

    def :=[B](rhs: B): Unit = macro Ops.updateOp[B]

    // Semigroup
    def |+|:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def |-|:=[B](rhs: B): Unit = macro Ops.updateOp[B]

    // Ring
    def +:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def -:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def *:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def **:=[B](rhs: B): Unit = macro Ops.updateOp[B]

    // EuclideanRing
    def /~:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def %:=[B](rhs: B): Unit = macro Ops.updateOp[B]

    // Field
    def /:=[B](rhs: B): Unit = macro Ops.updateOp[B]

    // BooleanAlgebra
    def ^:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def |:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def &:=[B](rhs: B): Unit = macro Ops.updateOp[B]

    // BitString
    def <<:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def >>:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def >>>:=[B](rhs: B): Unit = macro Ops.updateOp[B]

    // VectorSpace
    def *::=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def :*:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def :/:=[B](rhs: B): Unit = macro Ops.updateOp[B]

    // GroupAction
    def <|+|:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def <+:=[B](rhs: B): Unit = macro Ops.updateOp[B]
    def <*:=[B](rhs: B): Unit = macro Ops.updateOp[B]

  }

  implicit def updateOps[A](lhs: A): UpdateOps[A] = new UpdateOps[A](lhs)

}
