grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

import  edu:umn:cs:melt:ableJ14:terminals;

nonterminal Constructor_Invocation with errors, env, type_env, enclosingType, pp, basepp, pp_indent;

abstract production class_constructor
cdcl::Class_Member_Dcl ::= mods::Modifiers id::Id_t fps::Formal_Params thr::Throws cb::Block {
  cdcl.pp = mods.pp ++ id.lexeme ++ "(" ++ fps.pp ++ ")" ++ thr.pp ++ " " ++ cb.pp;
  cdcl.basepp = mods.basepp ++ id.lexeme ++ "(" ++ fps.basepp ++ ")" ++ thr.basepp ++ " " ++ cb.basepp;
  cdcl.field_defs = [];
  cdcl.method_defs = [];
  cdcl.constructor_defs = [ envItem (	id.lexeme, 
					fully_qualified_name_none (), 
					constructorDcl (id.lexeme, 
							mods.modlist, 
							fps.resolvedTypeReps)) ];
  cdcl.inner_type_defs = [];
  cdcl.errors := my_errors ++ mods.errors ++ fps.errors ++ thr.errors ++ cb.errors;

  local attribute my_errors :: [ Error ];
  my_errors = if id.lexeme != cdcl.enclosingType.classtyperep.name
		then [ mkError (id.line, "Constructor " ++ id.lexeme ++ " must match class name " ++ cdcl.enclosingType.classtyperep.name)]
		else [];

  cb.env = appendNewScope (fps.defs, cdcl.env);
  cb.my_return_type = voidTypeRep ();
}

function getDefaultConstructor
[ EnvItem ] ::= id::Id_t {
  return [ envItem (id.lexeme, fully_qualified_name_none (), constructorDcl (id.lexeme, [], [] )) ];
}

abstract production stmt_constructor_invocation
stmt::Stmt ::= inv::Constructor_Invocation {
  stmt.pp = inv.pp;
  stmt.basepp = inv.basepp;
  stmt.defs = [];
  stmt.type_defs = [];
  stmt.errors := inv.errors;
}

abstract production this_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
  inv.pp = "this (" ++ args.pp ++ ");";
  inv.basepp = "this (" ++ args.basepp ++ ");";
  inv.errors := args.errors;
}

abstract production super_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
  inv.pp = "super (" ++ args.pp ++ ");";
  inv.basepp = "super (" ++ args.basepp ++ ");";
  inv.errors := args.errors;
}

abstract production this_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
  inv.pp = expr.pp ++ ".this (" ++ args.pp ++ ");";
  inv.basepp = expr.basepp ++ ".this (" ++ args.basepp ++ ");";
  inv.errors := expr.errors ++ args.errors;
}

abstract production this_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
  inv.pp = t.pp ++ ".this (" ++ args.pp ++ ");";
  inv.basepp = t.basepp ++ ".this (" ++ args.basepp ++ ");";
  inv.errors := args.errors;
}

abstract production super_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
  inv.pp = expr.pp ++ ".super (" ++ args.pp ++ ");";
  inv.basepp = expr.basepp ++ ".super (" ++ args.basepp ++ ");";
  inv.errors := expr.errors ++ args.errors;
}

abstract production super_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
  inv.pp = t.pp ++ ".super (" ++ args.pp ++ ");";
  inv.basepp = t.basepp ++ ".super (" ++ args.basepp ++ ");";
  inv.errors := args.errors;
}
