package scalin
package macros

import spire.macros.compat.{termName, Context}

object AssignMacro {

  val operatorNames = spire.macros.Ops.operatorNames ++ Map("" -> "", "==" -> "equal", "!=" -> "notEqual") // ==, != included for documentation

  def updateOp[B](c: Context)(rhs: c.Expr[B]): c.Expr[Unit] = {
    import c.universe._
    val Pattern = """(.*)\$colon\$eq""".r
    def transform(tn: TermName): TermName = tn.toString match {
      case Pattern(op) => termName(c)("set" + operatorNames(op).capitalize)
    }
    c.macroApplication match {
      case q"$x1.assignOps[$x2]($lhs.apply(...$args)).$method[$x3]($x4)" =>
        c.Expr[Unit](q"$lhs.${transform(method)}(..${args.head}, $rhs)")
      case q"$x1.assignOps[$x2]($lhs.apply[$x3](...$args)).$method[$x4]($x5)" =>
        c.Expr[Unit](q"$lhs.${transform(method)}(..${args.head}, $rhs)")
      case q"$x1.assignOps[$x2]($lhs).$method[$x3]($x4)" =>
        c.Expr[Unit](q"$lhs.${transform(method)}($rhs)")
      case _ =>
        sys.error(s"Not implemented for tree ${showRaw(c.macroApplication)} ${show(c.macroApplication)}")
    }
  }

}
