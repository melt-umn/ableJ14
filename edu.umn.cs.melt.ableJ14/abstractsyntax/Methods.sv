grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;
import edu:umn:cs:melt:ableJ14:terminals;

nonterminal Method_Dcl 		with errors, pp, basepp, env, type_env, method_defs, pp_indent, enclosingType;
nonterminal Method_Header 	with errors, pp, basepp, env, type_env, method_defs, modlist, defs, return_type, param_types;
nonterminal Method_Declarator 	with errors, pp, basepp;
nonterminal Formal_Params 	with errors, pp, basepp, env, type_env, defs, param_types;
nonterminal Formal_Param 	with errors, pp, basepp, env, type_env, defs, typerep;
nonterminal Throws 		with errors, pp, basepp, type_env;

synthesized attribute return_type :: TypeRep;
synthesized attribute param_types :: [ TypeRep ];
autocopy attribute my_return_type :: TypeRep;

abstract production method_dcl_prod
mdcl::Method_Dcl ::=  mh::Method_Header b::Block {
  mdcl.pp = mh.pp ++ " " ++ b.pp ++ ";";
  mdcl.basepp = mh.basepp ++ " " ++ b.basepp ++ ";";
  mdcl.method_defs = mh.method_defs;
  mdcl.errors := mh.errors ++ b.errors;

  b.env = appendNewScope (mh.defs, mdcl.env);
  b.my_return_type = mh.return_type;
}

abstract production method_dcl_no_body
mdcl::Method_Dcl ::= mh::Method_Header {
  mdcl.pp = mh.pp ++ ";";
  mdcl.basepp = mh.basepp ++ ";";
  mdcl.method_defs = mh.method_defs;
  mdcl.errors := mh.errors;
}

abstract production method_header_prod
mh::Method_Header ::= mods::Modifiers return_ty::Type mname::Id_t fps::Formal_Params th::Throws {
  mh.pp = mods.pp ++ return_ty.pp ++ " " ++ mname.lexeme ++ "(" ++ fps.pp ++ ")" ++ th.pp;
  mh.basepp = mods.basepp ++ return_ty.basepp ++ " " ++ mname.lexeme ++ "(" ++ fps.basepp ++ ")" ++ th.basepp;
  mh.modlist = mods.modlist;
  mh.method_defs = [ envItem (	mname.lexeme, 
				fully_qualified_name_none (), 
				methodDcl (	mname.lexeme, 
						mods.modlist, 
						return_ty.resolvedTypeRep, 
						fps.resolvedTypeReps)) ];
  mh.defs = fps.defs;
  mh.errors := mods.errors ++ return_ty.errors ++ fps.errors ++ th.errors;
  mh.param_types = fps.param_types;
  mh.return_type = return_ty.typerep;
}

abstract production formal_params_none
fps::Formal_Params ::= {
  fps.pp = "";
  fps.basepp = "";
  fps.defs = [];
  fps.errors := [];
  fps.param_types = [];
}

abstract production formal_params_one
fps::Formal_Params ::= fp::Formal_Param {
  fps.pp = fp.pp;
  fps.basepp = fp.basepp;
  fps.defs = fp.defs;
  fps.errors := fp.errors;
  fps.param_types = [fp.typerep];
}

abstract production formal_params_snoc
fps::Formal_Params ::= fps1::Formal_Params fp::Formal_Param {
  fps.pp = fps1.pp ++ ", " ++ fp.pp;
  fps.basepp = fps1.basepp ++ ", " ++ fp.basepp;
  fps.defs = fps1.defs ++ fp.defs;
  fps.errors := fps1.errors ++ fp.errors;
  fps.param_types = fps1.param_types ++ [fp.typerep];
}

abstract production formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
  fp.pp = t.pp ++ " " ++ vid.pp ;
  fp.basepp = t.basepp ++ " " ++ vid.basepp ;
  fp.defs = [envItem (	vid.name, 
			fully_qualified_name_none (),
                        paramDcl (vid.name, typeWithDeclarator)) ];
                        -- not resolved, since we're not recording this in the defs file

  fp.errors := t.errors ++ vid.errors;
  fp.typerep = typeWithDeclarator;

  local attribute typeWithDeclarator :: TypeRep;
  typeWithDeclarator = getTypeWithDeclarator (t.typerep, vid.dimensions);
}

abstract production return_statement
s::Stmt ::= t::Return_t {
  s.pp = "return;";
  s.basepp = "return;";
  s.errors := if equality_check (s.my_return_type, voidTypeRep ())
		then []
		else [mkError (t.line,  "Expression required in return")];
  s.defs = [];
  s.type_defs = [];
}

abstract production return_expr
s::Stmt ::= t::Return_t e::Expr {
  s.pp = "return " ++ e.pp ++ ";";

  forwards to return_expr_copy (t, copy (e, s.my_return_type, t.line));
}

abstract production return_expr_copy
s::Stmt ::= t::Return_t e::Expr {
  s.pp = "return " ++ e.pp ++ ";";
  s.basepp = "return " ++ e.basepp ++ ";";
  s.errors := if equality_check (s.my_return_type, voidTypeRep ()) 
		then [mkError (t.line,  "Return should not have expression")] 
		else e.errors;

  s.defs = [];
  s.type_defs = [];
}

-- Method Declarators
---------------------

synthesized attribute method_id :: Id_t occurs on Method_Declarator;
synthesized attribute formal_params :: Formal_Params occurs on Method_Declarator;
attribute dimensions occurs on Method_Declarator;

abstract production method_header_declarator
mh::Method_Header ::= mods::Modifiers t::Type md::Method_Declarator th::Throws {
  mh.pp = mods.pp ++ t.pp ++ " " ++ md.pp ++ th.pp;
  forwards to method_header_prod (mods, newReturnType, md.method_id, md.formal_params, th);

  local attribute newReturnType :: Type;
  newReturnType = if md.dimensions == 0
			then t
			else reference_type (array_type (case t'' of
								primitive_type (pt) -> primitive_array (pt, md.dimensions) |
								reference_type (name_type (tn)) -> name_array (tn, md.dimensions) |
								void_type () -> error ("void method declarator array in " ++ md.method_id.lexeme) | 
								_ -> error ("Internal compiler error in method_header_declarator, unhandled type " ++ t.pp)
							 end));
}

abstract production method_declarator
md::Method_Declarator ::= id::Id_t fps::Formal_Params {
  md.pp = id.lexeme ++ "(" ++ fps.pp ++ ")[]";
  md.basepp = id.lexeme ++ "(" ++ fps.basepp ++ ")[]";
  md.errors := fps.errors;

  md.dimensions = 0;
  md.method_id = id;
  md.formal_params = fps;
}

abstract production method_declarator_array
md::Method_Declarator ::= md1::Method_Declarator {
  md.pp = md1.pp ++ "[]";
  md.basepp = md1.basepp ++ "[]";
  md.errors := md1.errors;

  md.dimensions = md1.dimensions + 1;
  md.method_id = md1.method_id;
  md.formal_params = md1.formal_params;
}

abstract production final_formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
  fp.pp = "final " ++ t.pp ++ " " ++ vid.pp ;
  fp.basepp = "final " ++ t.basepp ++ " " ++ vid.basepp ;
  forwards to formal_param (t, vid);
}

abstract production throws_none
th::Throws ::= {
  th.pp = "";
  th.basepp = "";
  th.errors := [];
}

abstract production throws
th::Throws ::= ctl::TypeNames {
  th.pp = " throws " ++ ctl.pp ++ " ";
  th.basepp = " throws " ++ ctl.basepp ++ " ";
  th.errors := ctl.errors;
}
