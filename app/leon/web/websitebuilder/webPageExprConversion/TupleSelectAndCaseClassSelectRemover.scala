package leon.web.websitebuilder.webPageExprConversion

import leon.purescala.Expressions.{AsInstanceOf, _}

/**
  * Created by dupriez on 4/25/16.
  */
object TupleSelectAndCaseClassSelectRemover {
  def removeTopLevelTupleSelectsAndCaseClassSelects(expr: Expr): Expr = {
    // Just to have a shorter name to use inside
    def recurse(expr1: Expr): Expr = {
      expr1 match {
        case TupleSelect(Tuple(args), i) =>
          recurse(args(i - 1))
        case TupleSelect(arg, i) =>
          recurse(TupleSelect(recurse(arg), i))
        case CaseClassSelector(cct, CaseClass(ct, args), id) =>
          recurse(args(cct.classDef.selectorID2Index(id)))
        case CaseClassSelector(cct, inExpr, id) =>
          val i = recurse(inExpr)
          if(i eq inExpr) {
            expr1
          } else recurse(CaseClassSelector(cct, i, id))
        case AsInstanceOf(expr2, classType) =>
          recurse(expr2)
        case MapApply(FiniteMap(pairs, keyType, valueType), theKey) =>
          recurse(pairs(theKey))
        case _ => expr1
      }
    }
    recurse(expr)
  }
}
