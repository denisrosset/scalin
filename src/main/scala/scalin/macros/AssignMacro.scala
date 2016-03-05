package scalin
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}

object AssignMacro {

  val operatorNames = spire.macros.Ops.operatorNames ++ Map("" -> "", "==" -> "equal", "!=" -> "notEqual") // ==, != included for documentation

  def updateOp[B](c: Context)(rhs: c.Expr[B]): c.Expr[Unit] = {
    import c.universe._
    val Pattern = """(.*)\$colon\$eq""".r
    def transform(tn: TermName): TermName = tn match {
      case TermName(Pattern(op)) => TermName("set" + operatorNames(op).capitalize)
    }
    c.macroApplication match {
      case q"$_.assignOps[$_]($lhs.apply[$_](...$args)).$method[$_]($_)" =>
        c.Expr[Unit](q"$lhs.${transform(method)}(..${args.head}, $rhs)")
      case q"$_.assignOps[$_]($lhs).$method[$_]($_)" =>
        c.Expr[Unit](q"$lhs.${transform(method)}($rhs)")
      case _ =>
        sys.error(s"Not implemented for tree ${showRaw(c.macroApplication)} ${show(c.macroApplication)}")

    }
  }

}
