grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals;

abstract production while_prod
while::Stmt ::= t::While_t cond::Expr body::Stmt {
  while.pp = "while (" ++ cond.pp ++ ")\n" ++ space(body.pp_indent) ++ body.pp;
  while.basepp = "while (" ++ cond.basepp ++ ")\n" ++ space(body.pp_indent) ++ body.basepp;
  while.errors := my_errors ++ cond.errors ++ body.errors;
  while.defs = body.defs;
  while.type_defs = body.type_defs;

  local attribute my_errors :: [Error];
  my_errors = if cond.typerep.isUnknownType || equality_check (cond.typerep, booleanTypeRep ())
		then []
		else [mkError (t.line, "Condition " ++ cond.pp ++ " in while statement does not have boolean type")];

  body.pp_indent = while.pp_indent + 3;
}

abstract production do
dowhile::Stmt ::= body::Stmt t::While_t cond::Expr {
  dowhile.pp = "do " ++ body.pp ++ " while (" ++ cond.pp ++ ");";
  forwards to stmt_seq (body, while_prod (t, cond, body));
}

abstract production label_prod
lstmt::Stmt ::= id::Id_t stmt::Stmt {
  lstmt.pp = id.lexeme ++ ": " ++ stmt.pp;
  lstmt.basepp = id.lexeme ++ ": " ++ stmt.basepp;
  lstmt.defs = stmt.defs;
  lstmt.type_defs = stmt.type_defs;
  lstmt.errors := stmt.errors;
}

abstract production break_prod
break::Stmt ::= {
  break.pp = "break;";
  break.basepp = "break;";
  break.errors := [];
  break.defs = [];
  break.type_defs = [];
}

abstract production break_label
break::Stmt ::= id::Id_t {
  break.pp = "break " ++ id.lexeme ++ ";";
  break.basepp = "break " ++ id.lexeme ++ ";";
  break.errors := [];
  break.defs = [];
  break.type_defs = [];
}

abstract production continue_prod
continue::Stmt ::= {
  continue.pp = "continue;";
  continue.basepp = "continue;";
  continue.errors := [];
  continue.defs = [];
  continue.type_defs = [];
}

abstract production continue_label
continue::Stmt ::= id::Id_t {
  continue.pp = "continue " ++ id.lexeme ++ ";";
  continue.basepp = "continue " ++ id.lexeme ++ ";";
  continue.errors := [];
  continue.defs = [];
  continue.type_defs = [];
}
