grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;

nonterminal Block with   pp, basepp, pp_indent, env, type_env, errors, enclosingType, my_return_type;
nonterminal Stmt with    pp, basepp, pp_indent, env, type_env, errors, enclosingType, my_return_type, defs, type_defs;
nonterminal Catch with   pp, basepp, pp_indent, env, type_env, errors, enclosingType, my_return_type;
nonterminal Catches with pp, basepp, pp_indent, env, type_env, errors, enclosingType, my_return_type;

abstract production block
b::Block ::= stmt::Stmt {
  b.pp = "{\n" ++ space(stmt.pp_indent) ++ stmt.pp ++ "\n" ++ space(b.pp_indent) ++ "}";
  b.basepp = "{\n" ++ space(stmt.pp_indent) ++ stmt.basepp ++ "\n" ++ space(b.pp_indent) ++ "}";
  b.errors := stmt.errors;

  stmt.pp_indent = b.pp_indent + 3;
}

abstract production empty_block
b::Block ::= {
  b.pp = "{}";
  b.basepp = "{}";
  b.errors := [];
}

abstract production stmt_seq
seq::Stmt ::= stmt1::Stmt stmt2::Stmt {
  seq.pp = stmt1.pp ++ "\n" ++ space(stmt2.pp_indent) ++ stmt2.pp;
  seq.basepp = stmt1.basepp ++ "\n" ++ space(stmt2.pp_indent) ++ stmt2.basepp;
  seq.defs = stmt1.defs ++ stmt2.defs;
  seq.type_defs = stmt1.type_defs ++ stmt2.type_defs;
  seq.errors := stmt1.errors ++ stmt2.errors;

  stmt2.env = appendWithinScope (stmt1.defs, seq.env);
  stmt2.type_env = appendWithinScope (stmt1.type_defs, seq.type_env);
}

abstract production stmt_block
stmt::Stmt ::= b::Block {
  stmt.pp = b.pp;
  stmt.basepp = b.basepp;
  stmt.defs = [];
  stmt.type_defs = [];
  stmt.errors := b.errors;
}

abstract production empty_stmt
s::Stmt ::= {
  s.pp = ";";
  s.basepp = ";";
  s.defs = [];
  s.type_defs = [];
  s.errors := [];
}

abstract production stmt_stmt_expr
s::Stmt ::= s1::Stmt_Expr {
  s.pp = s1.pp ++ ";";
  s.basepp = s1.basepp ++ ";";
  s.defs = [];
  s.type_defs = [];
  s.errors := s1.errors;
}

abstract production block_stmt_class
s::Stmt ::= cdcl::Class_Dcl {
  s.pp = cdcl.pp;
  s.basepp = cdcl.basepp;
  s.defs = [];
  s.errors := cdcl.errors;
  s.type_defs = convertEnvItems (cdcl.type_defs, s.type_env);

  cdcl.type_env = appendWithinScope (s.type_defs, s.type_env);
}

abstract production block_stmt_interface
s::Stmt ::= idcl::Interface_Dcl {
  s.pp = idcl.pp;
  s.basepp = idcl.basepp;
  s.defs = [];
  s.errors := idcl.errors;
  s.type_defs = convertEnvItems (idcl.type_defs, s.type_env);

  idcl.type_env = appendWithinScope (s.type_defs, s.type_env);
}

abstract production synchronized
s::Stmt ::= e::Expr b::Block {
  s.pp = "synchronized (" ++ e.pp ++ ")" ++ b.pp;
  s.basepp = "synchronized (" ++ e.basepp ++ ")" ++ b.basepp;
  s.defs = [];
  s.type_defs = [];
  s.errors := e.errors ++ b.errors;
}

abstract production throw
s::Stmt ::= e::Expr {
  s.pp = "throw " ++ e.pp ++ ";";
  s.basepp = "throw " ++ e.basepp ++ ";";
  s.defs = [];
  s.type_defs = [];
  s.errors := e.errors;
}

abstract production try
s::Stmt ::= b::Block c::Catches {
  s.pp = "try " ++ b.pp ++ " " ++ c.pp;
  s.basepp = "try " ++ b.basepp ++ " " ++ c.basepp;
  s.defs = [];
  s.type_defs = [];
  s.errors := b.errors ++ c.errors;
}

abstract production try_finally
s::Stmt ::= b::Block c::Catches b1::Block {
  s.pp = "try " ++ b.pp ++ "\n" ++ space(s.pp_indent) ++ c.pp ++ "\n" ++ space(s.pp_indent) ++ "finally " ++ b1.pp;
  s.basepp = "try " ++ b.basepp ++ "\n" ++ space(s.pp_indent) ++ c.basepp ++ "\n" ++ space(s.pp_indent) ++ "finally " ++ b1.basepp;
  s.defs = [];
  s.type_defs = [];
  s.errors := b.errors ++ c.errors ++ b1.errors;
}

abstract production assert
s::Stmt ::= expr::Expr {
  s.pp = "assert " ++ expr.pp ++ ";";
  s.basepp = "assert " ++ expr.basepp ++ ";";
  s.defs = [];
  s.type_defs = [];
  s.errors := expr.errors;
}

abstract production assert_colon
s::Stmt ::= expr1::Expr expr2::Expr {
  s.pp = "assert " ++ expr1.pp ++ " : " ++ expr2.pp ++ ";";
  s.basepp = "assert " ++ expr1.basepp ++ " : " ++ expr2.basepp ++ ";";
  s.defs = [];
  s.type_defs = [];
  s.errors := expr1.errors ++ expr2.errors;
}

abstract production catches_none
c::Catches ::= {
  c.pp = "";
  c.basepp = "";
  c.errors := [];
}

abstract production catches_snoc
c::Catches ::= cs::Catches cc::Catch {
  c.pp = cs.pp ++ "\n" ++ space(cs.pp_indent) ++ cc.pp;
  c.basepp = cs.basepp ++ "\n" ++ space(cs.pp_indent) ++ cc.basepp;
  c.errors := cs.errors ++ cc.errors;
}

abstract production catch
c::Catch ::= fp::Formal_Param b::Block {
  c.pp = "catch (" ++ fp.pp ++ ") " ++ b.pp;
  c.basepp = "catch (" ++ fp.basepp ++ ") " ++ b.basepp;
  c.errors := fp.errors ++ b.errors;
  b.env = appendNewScope (fp.defs, c.env);
}
