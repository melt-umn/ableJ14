grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import  edu:umn:cs:melt:ableJ14:terminals;

abstract production if_then
ifthen::Stmt ::= t::If_t expr::Expr thenbody::Stmt {
  ifthen.pp = "if (" ++ expr.pp ++ ")\n" ++ space(thenbody.pp_indent) ++ thenbody.pp;
  forwards to if_then_else (t, expr, thenbody, empty_stmt());

  thenbody.pp_indent = ifthen.pp_indent + 3;
}

abstract production if_then_else
ifthenelse::Stmt ::= t::If_t expr::Expr thenbody::Stmt elsebody::Stmt {
  ifthenelse.pp = "if (" ++ expr.pp ++ ")\n" ++ space(thenbody.pp_indent) ++ thenbody.pp ++ "\n" ++ space(ifthenelse.pp_indent) ++ "else " ++ elsebody.pp;
  ifthenelse.basepp = "if (" ++ expr.basepp ++ ")\n" ++ space(thenbody.pp_indent) ++ thenbody.basepp ++ "\n" ++ space(ifthenelse.pp_indent) ++ "else " ++ elsebody.basepp;
  ifthenelse.errors := my_errors ++ expr.errors ++ thenbody.errors ++ elsebody.errors;
  ifthenelse.defs = thenbody.defs ++ elsebody.defs;
  ifthenelse.type_defs = thenbody.type_defs ++ elsebody.type_defs;

  local attribute my_errors :: [Error];
  my_errors = if expr.typerep.isUnknownType || equality_check (expr.typerep, booleanTypeRep ())
		then []
		else [mkError (t.line, "Condition " ++ expr.pp ++ " in if statement does not have boolean type")];

  thenbody.pp_indent = ifthenelse.pp_indent + 3;
  elsebody.pp_indent = ifthenelse.pp_indent + 3;
}

