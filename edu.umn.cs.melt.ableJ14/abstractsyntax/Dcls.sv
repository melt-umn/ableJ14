grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;
import  edu:umn:cs:melt:ableJ14:terminals;

nonterminal Local_Var_Dcl with     enclosingType, pp, pp_indent, basepp, env, errors, type_env, defs;
nonterminal Field_Dcl with         enclosingType, pp, pp_indent, basepp, env, errors, type_env, field_defs;
nonterminal Var_Declarators with   enclosingType, pp, pp_indent, basepp, env, errors, type_env, dcl_list;
nonterminal Var_Declarator with    enclosingType, pp, pp_indent, basepp, env, errors, type_env, name, dimensions;
nonterminal Var_Declarator_Id with enclosingType, pp, pp_indent, basepp, env, errors, type_env, name, dimensions;
nonterminal Var_Init with          enclosingType, pp, pp_indent, basepp, env, errors, type_env, typerep;
nonterminal Var_Inits with         enclosingType, pp, pp_indent, basepp, env, errors, type_env;
nonterminal Array_Init with        enclosingType, pp, pp_indent, basepp, env, errors, type_env;

synthesized attribute dcl_list :: [ Var_Declarator ];
synthesized attribute name :: String;
synthesized attribute dimensions :: Integer;

autocopy attribute requiredTypeInh :: TypeRep;
attribute requiredTypeInh occurs on Var_Declarators, Var_Declarator, Var_Init, Var_Inits, Array_Init;

abstract production stmt_dcl
stmt::Stmt ::= vdcl::Local_Var_Dcl {
  stmt.pp = vdcl.pp ++ ";";
  stmt.basepp = vdcl.basepp ++ ";";
  stmt.defs = vdcl.defs;
  stmt.type_defs = [];
  stmt.errors := vdcl.errors;
}

abstract production local_var_dcl
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
  vdcl.pp = dcltype.pp ++ " " ++ dcls.pp;

  forwards to makeIndividualDcl (dcltype, dcls.dcl_list);
}

abstract production local_var_dcl_final
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
  vdcl.pp = "final " ++ dcltype.pp ++ " " ++ dcls.pp;

  forwards to local_var_dcl (dcltype, dcls);
}

abstract production field_dcl
f::Field_Dcl ::= mods::Modifiers t::Type dcls::Var_Declarators {
  f.pp = mods.pp ++ t.pp ++ " " ++ dcls.pp ++ ";";

  forwards to makeIndividualFieldDcl (mods, t, dcls.dcl_list);
}

abstract production var_declarator
vdcl::Var_Declarator ::= v::Var_Declarator_Id {
  vdcl.pp = v.pp;
  vdcl.basepp = v.basepp;
  vdcl.errors := v.errors;
  vdcl.name = v.name;
  vdcl.dimensions = v.dimensions;
}

abstract production var_declarator_init
vdcl::Var_Declarator ::= v::Var_Declarator_Id init::Var_Init {
  vdcl.pp = v.pp ++ " = " ++ init.pp;
  vdcl.basepp = v.basepp ++ " = " ++ init.basepp;
  vdcl.errors := v.errors ++ init.errors;
  vdcl.name = v.name;
  vdcl.dimensions = v.dimensions;
}

abstract production var_declarator_id
vdcl::Var_Declarator_Id ::= id::Id_t {
  vdcl.pp = id.lexeme;
  vdcl.basepp = id.lexeme;
  vdcl.name = id.lexeme;
  vdcl.errors := [];
  vdcl.dimensions = 0;
}

abstract production var_declarator_array
vdcl::Var_Declarator_Id ::= v::Var_Declarator_Id {
  vdcl.pp = v.pp ++ "[] ";
  vdcl.basepp = v.basepp ++ "[] ";
  vdcl.name = v.name;
  vdcl.errors := v.errors;
  vdcl.dimensions = v.dimensions + 1;
}

abstract production var_init_expr
init::Var_Init ::= e::Expr {
  init.pp = e.pp;

  forwards to var_init_expr_copy (copy (e, init.requiredTypeInh, -1));
}

abstract production var_init_expr_copy
init::Var_Init ::= e::Expr {
  init.pp = e.pp;
  init.basepp = e.basepp;
  init.errors := e.errors;
  init.typerep = e.typerep;
}

abstract production var_init_array
init::Var_Init ::= e::Array_Init {
  init.pp = e.pp;
  init.basepp = e.basepp;
  init.errors := e.errors;
  init.typerep = unknownTypeRep ();
}

function makeIndividualDcl
Local_Var_Dcl ::= dcltype::Type dcls::[Var_Declarator] {
  return if null (dcls) 
	 then error ("E1: Var Declarator List empty.\n") -- will not occur
         else (if length (dcls) == 1 
	 	then local_var_dcl_one (dcltype, head (dcls))
	 	else local_var_dcl_seq (local_var_dcl_one (dcltype, head (dcls)), makeIndividualDcl (dcltype, tail (dcls)))
              );
}

-- this is needed so that the declarators are not split into separate lines in the basepp
-- in the For Init.
synthesized attribute restbasepp :: String occurs on Local_Var_Dcl, Field_Dcl;

abstract production local_var_dcl_seq
vdcl::Local_Var_Dcl ::= vdcl1::Local_Var_Dcl vdcl2::Local_Var_Dcl {
  vdcl.pp = vdcl1.pp ++ ";\n" ++ space (vdcl2.pp_indent) ++ vdcl2.pp;
  vdcl.basepp = vdcl1.basepp ++ ", " ++ vdcl2.restbasepp;
  vdcl.restbasepp = vdcl1.restbasepp ++ ", " ++ vdcl2.restbasepp;

  vdcl.defs = vdcl1.defs ++ vdcl2.defs;
  vdcl.errors := vdcl1.errors ++ vdcl2.errors;

  vdcl2.env = appendWithinScope (vdcl1.defs, vdcl.env);
}

abstract production local_var_dcl_one
vdcl::Local_Var_Dcl ::= dcltype::Type dcl::Var_Declarator {
  vdcl.pp = dcltype.pp ++ " " ++ dcl.pp;
  vdcl.basepp = dcltype.basepp ++ " " ++ dcl.basepp;
  vdcl.restbasepp = dcl.basepp;

  vdcl.defs = [ envItem (       dcl.name, 
				fully_qualified_name_none (),
				localDcl (dcl.name, typeWithDeclarator))];
					  -- not resolved, since we are not recording this in the defs file

     -- ToDo  [ variableNameBinding ( dcl.name, dcltype.
  vdcl.errors := dcltype.errors ++ dcl.errors;
  dcl.requiredTypeInh = typeWithDeclarator;

  local attribute typeWithDeclarator :: TypeRep;
  typeWithDeclarator = getTypeWithDeclarator (dcltype.typerep, dcl.dimensions);

  dcl.env = appendWithinScope (vdcl.defs, vdcl.env);
}

function makeIndividualFieldDcl
Field_Dcl ::= mods::Modifiers dcltype::Type dcls::[Var_Declarator] {
  return if null (dcls) 
	 then error ("E2: Var Declarator List empty.\n") -- will not occur
         else (if length (dcls) == 1 
		 then field_dcl_one (mods, dcltype, head (dcls))
		 else field_dcl_seq (field_dcl_one (mods, dcltype, head (dcls)), makeIndividualFieldDcl (mods, dcltype, tail (dcls)))
              );
}

abstract production field_dcl_seq
fdcl::Field_Dcl ::= fdcl1::Field_Dcl fdcl2::Field_Dcl {
  fdcl.pp = fdcl1.pp ++ ";\n" ++ space (fdcl2.pp_indent) ++ fdcl2.pp;
  fdcl.basepp = fdcl1.basepp ++ ";\n" ++ space (fdcl2.pp_indent) ++ fdcl2.basepp;
  fdcl.restbasepp = fdcl1.restbasepp ++ ", " ++ fdcl2.restbasepp;

  fdcl.field_defs = fdcl1.field_defs ++ fdcl2.field_defs;
  fdcl.errors := fdcl1.errors ++ fdcl2.errors;
}

abstract production field_dcl_one
f::Field_Dcl ::= mods::Modifiers t::Type dcl::Var_Declarator {
  f.pp = mods.pp ++ t.pp ++ " " ++ dcl.pp ++ ";";
  f.basepp = mods.basepp ++ t.basepp ++ " " ++ dcl.basepp ++ ";";
  f.restbasepp = dcl.basepp;

  f.field_defs = [  envItem (   dcl.name, 
				fully_qualified_name_none (),
				fieldDcl (dcl.name, 
					  mods.modlist,
					  getTypeWithDeclarator (t.resolvedTypeRep, dcl.dimensions))) ] ;
  f.errors := mods.errors ++ t.errors ++ dcl.errors;

  dcl.requiredTypeInh = typeWithDeclarator;

  local attribute typeWithDeclarator :: TypeRep;
  typeWithDeclarator = getTypeWithDeclarator (t.typerep, dcl.dimensions);
}

abstract production var_declarators_one
vdcls::Var_Declarators ::= vdcl::Var_Declarator {
  vdcls.pp = vdcl.pp;
  vdcls.basepp = vdcl.basepp;
  vdcls.errors := vdcl.errors;
  vdcls.dcl_list = [ vdcl ];
}

abstract production var_declarators_snoc
vdcls::Var_Declarators ::= vdcls1::Var_Declarators vdcl::Var_Declarator {
  vdcls.pp = vdcls1.pp ++ ", " ++ vdcl.pp;
  vdcls.basepp = vdcls1.basepp ++ ", " ++ vdcl.basepp;
  vdcls.errors := vdcls1.errors ++ vdcl.errors;
  vdcls.dcl_list = vdcls1.dcl_list ++ [ vdcl ];
}

abstract production array_init
ai::Array_Init ::= vs::Var_Inits {
  ai.pp = "{" ++ vs.pp ++ ", }";
  ai.basepp = "{" ++ vs.basepp ++ ", }";
  ai.errors := vs.errors;

  vs.requiredTypeInh = oneLessDimension (ai.requiredTypeInh);
}

abstract production array_init_no_comma
ai::Array_Init ::= vs::Var_Inits {
  ai.pp = "{" ++ vs.pp ++ "}";
  ai.basepp = "{" ++ vs.basepp ++ "}";
  ai.errors := vs.errors;

  vs.requiredTypeInh = oneLessDimension (ai.requiredTypeInh);
}

abstract production array_init_no_var_inits
ai::Array_Init ::= {
  ai.pp = "{, }";
  ai.basepp = "{, }";
  ai.errors := [];
}

abstract production array_init_empty
ai::Array_Init ::= {
  ai.pp = "{}";
  ai.basepp = "{}";
  ai.errors := [];
}

abstract production var_inits_one
vis::Var_Inits ::= vi::Var_Init {
  vis.pp = vi.pp;
  vis.basepp = vi.basepp;
  vis.errors := vi.errors;
}

abstract production var_inits_snoc
vis::Var_Inits ::= vis1::Var_Inits vi::Var_Init {
  vis.pp = vis1.pp ++ ", " ++ vi.pp;
  vis.basepp = vis1.basepp ++ ", " ++ vi.basepp;
  vis.errors := vis1.errors ++ vi.errors;
}

function getTypeWithDeclarator
TypeRep ::= t::TypeRep ds::Integer {
  return case t'' of
		arrayTypeRep (tr, dims) -> arrayTypeRep (tr, dims + ds) |
		tr -> if ds == 0
			then tr''
			else arrayTypeRep (tr, ds)
	 end;
}

function oneLessDimension
TypeRep ::= t::TypeRep {
  return case t'' of
		arrayTypeRep (tr, dims) -> if dims == 0 then tr'' else arrayTypeRep (tr, dims - 1) |
		tr -> errorTypeRep ([ mkError (-1, "Cannot reduce dimension of non-array type " ++ t.eqName) ])
	 end;
}

