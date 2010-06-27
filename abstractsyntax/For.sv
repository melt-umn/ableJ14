grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

import  edu:umn:cs:melt:ableJ14:terminals;

nonterminal Stmt_Exprs with pp, basepp, env, type_env, errors, enclosingType, pp_indent ;
nonterminal For_Init with   pp, basepp, env, type_env, errors, enclosingType, pp_indent, defs ;
nonterminal For_Test with   pp, basepp, env, type_env, errors, enclosingType;
nonterminal For_Update with pp, basepp, env, type_env, errors, enclosingType, pp_indent;

abstract production for
forstmt::Stmt ::= init::For_Init test::For_Test update::For_Update body::Stmt {
  forstmt.pp = "for (" ++  init.pp ++ ";" ++ test.pp ++ ";" ++ update.pp ++ ")\n" ++ space(body.pp_indent) ++ body.pp;
  forstmt.basepp = "for (" ++  init.basepp ++ ";" ++ test.basepp ++ ";" ++ update.basepp ++ ")\n" ++ space(body.pp_indent) ++ body.basepp;
  forstmt.errors := init.errors ++ test.errors ++ update.errors ++ body.errors;
  forstmt.defs = [];
  forstmt.type_defs = [];

  test.env = appendWithinScope (init.defs, forstmt.env);
  update.env = appendWithinScope (init.defs, forstmt.env);
  body.env = appendWithinScope (init.defs, forstmt.env);
  body.pp_indent = forstmt.pp_indent + 3;
}

abstract production for_init_empty
forinit::For_Init ::= {
  forinit.pp = "";
  forinit.basepp = "";
  forinit.errors := [];
  forinit.defs = [];
}

abstract production for_init_some
forinit::For_Init ::= ses::Stmt_Exprs {
  forinit.pp = ses.pp;
  forinit.basepp = ses.basepp;
  forinit.errors := ses.errors;
  forinit.defs = [];
}

abstract production for_init_dcl
forinit::For_Init ::= dcl::Local_Var_Dcl {
  forinit.pp = dcl.pp;
  forinit.basepp = dcl.basepp;
  forinit.errors := dcl.errors;
  forinit.defs = dcl.defs;
}

abstract production for_test_none
fortest::For_Test ::= {
  fortest.pp = "";
  fortest.basepp = "";
  fortest.errors := [];
}

abstract production for_test_one
fortest::For_Test ::= e::Expr {
  fortest.pp = e.pp;
  fortest.basepp = e.basepp;
  fortest.errors := e.errors;
}

abstract production for_update_empty
forupdate::For_Update ::= {
  forupdate.pp = "";
  forupdate.basepp = "";
  forupdate.errors := [];
}

abstract production for_update_some
forupdate::For_Update ::= ses::Stmt_Exprs {
  forupdate.pp = ses.pp;
  forupdate.basepp = ses.basepp;
  forupdate.errors := ses.errors;
}

abstract production stmt_exprs_one
ses::Stmt_Exprs ::= se::Stmt_Expr {
  ses.pp = se.pp;
  ses.basepp = se.basepp;
  ses.errors := se.errors;
}

abstract production stmt_exprs_snoc
ses::Stmt_Exprs ::= ses1::Stmt_Exprs se::Stmt_Expr {
  ses.pp = ses1.pp ++ ", " ++ se.pp;
  ses.basepp = ses1.basepp ++ ", " ++ se.basepp;
  ses.errors := ses1.errors ++ se.errors;
}
