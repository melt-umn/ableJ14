grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;
import edu:umn:cs:melt:ableJ14:terminals;

nonterminal Class_Dcl with                          errors, pp, basepp, pp_indent, type_env, type_defs, classtyperep;
nonterminal Class_Body with        enclosingType, errors, pp, basepp, pp_indent, type_env, inner_type_defs, field_defs, method_defs, constructor_defs;
nonterminal Class_Member_Dcls with enclosingType, errors, pp, basepp, pp_indent, type_env, inner_type_defs, field_defs, method_defs, constructor_defs, env;
nonterminal Class_Member_Dcl with  enclosingType, errors, pp, basepp, pp_indent, type_env, inner_type_defs, field_defs, method_defs, constructor_defs, env;

synthesized attribute field_defs :: [ EnvItem ];
synthesized attribute method_defs :: [ EnvItem ];
synthesized attribute constructor_defs :: [ EnvItem ];
synthesized attribute inner_type_defs :: [ EnvItem ];

autocopy attribute enclosingType :: TypeRep;

abstract production type_class_dcl
td::Type_Dcl ::= cdcl::Class_Dcl {
  td.pp = cdcl.pp;
  td.basepp = cdcl.basepp; 
  td.type_defs = cdcl.type_defs;
  td.errors := cdcl.errors;
}

abstract production class_dcl_seq
cdcl::Class_Dcl ::= cdcl1::Class_Dcl cdcl2::Class_Dcl {
 cdcl.pp = cdcl1.pp ++ "\n\n" ++ space(cdcl2.pp_indent) ++ cdcl2.pp;
 cdcl.basepp = cdcl1.basepp ++ "\n\n" ++ cdcl2.basepp;
 cdcl.type_defs = cdcl1.type_defs ++ cdcl2.type_defs;
 cdcl.errors := cdcl1.errors ++ cdcl2.errors;
}

abstract production class_dcl_none
cdcl::Class_Dcl ::= {
 cdcl.pp = "";
 cdcl.basepp = "";
 cdcl.type_defs = [];
 cdcl.errors := [];
}

abstract production class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
  cdcl.pp = mods.pp ++ "class " ++ cname.lexeme ++ " extends " ++ parent.pp ++ 
		(case inters'' of type_names_none () -> "" | _ -> " implements " ++ inters.pp end) ++ " " ++ cb.pp;
  cdcl.basepp = mods.basepp ++ "class " ++ cname.lexeme ++ " extends " ++ parent.basepp ++ 
		(case inters'' of type_names_none () -> "" | _ -> " implements " ++ inters.basepp end) ++ " " ++ cb.basepp;

  cdcl.errors := mods.errors ++ parentErrors ++ inters.errors ++ cb.errors ;

  cdcl.type_defs = [ envItem (	fqn.qualifiedName,
				fqn, 
				classDcl (fqn,
					  classTypeRepDefs ( if cname.lexeme == "Object"
								then object_class_type_rep_defs (mods.modlist, inters.resolvedNames,
											cb.field_defs, cb.method_defs, cb.constructor_defs, cb.inner_type_defs)
								else class_type_rep_defs (cname.lexeme, fqn.qualifiedName, mods.modlist, parent.resolvedPackageOrTypeName,
												inters.resolvedNames,
											cb.field_defs, cb.method_defs, cb.constructor_defs, cb.inner_type_defs)))) ];

  local attribute fqn :: FullyQualifiedName;
  fqn = getQualifiedFQN (cdcl.qualifiersSoFar, cname.lexeme);

  local attribute thisClassType :: TypeRep;
  thisClassType = retrieveClass2 (fqn, cdcl.type_env, cdcl.file_name, cname.line);

  cdcl.classtyperep = thisClassType.classtyperep;

  local attribute parentErrors :: [ Error ];
  parentErrors =       case parent.disambiguatedName of
                	disambiguated_type_name (tr) -> case tr of
								classTypeRep (ctr) -> [ :: Error ] |
								_ -> [ mkError (cname.line, parent.pp ++ " is not of class type") ] 
							end |
	                disambiguated_error_name (errs) -> errs |
        	        _ -> error ("Internal compiler error in production class_dcl " ++ cname.lexeme)
		       end;

  cb.enclosingType = thisClassType;
  cb.type_env = thisClassType.classtyperep.innerTypes ++ cdcl.type_env;

  cb.qualifiersSoFar = fqn;
}

synthesized attribute resolvedNames :: [ FullyQualifiedName ] occurs on TypeNames;

aspect production type_names_none
ns::TypeNames ::= {
 ns.resolvedNames = [];
}

aspect production type_names_one
ns::TypeNames ::= n::TypeName {
 ns.resolvedNames = [ case n.resolvedPackageOrTypeName of
					fully_qualified_name_none () -> n.fullyQualifiedName |
					fully_qualified_name_unknown () -> n.fullyQualifiedName |
					_ -> n.resolvedPackageOrTypeName
				     end ];
}

aspect production type_names_snoc
ns::TypeNames ::= ns1::TypeNames n::TypeName {
 ns.resolvedNames = ns1.resolvedNames ++
				   [ case n.resolvedPackageOrTypeName of
					fully_qualified_name_none () -> n.fullyQualifiedName |
					fully_qualified_name_unknown () -> n.fullyQualifiedName |
					_ -> n.resolvedPackageOrTypeName
				     end ];
}

abstract production class_body
cb::Class_Body ::= dcls::Class_Member_Dcls {
  cb.pp = "{" ++ space(dcls.pp_indent) ++ dcls.pp ++ "\n" ++ space(cb.pp_indent) ++ "}";
  cb.basepp = "{" ++ space(dcls.pp_indent) ++ dcls.basepp ++ "\n" ++ space(cb.pp_indent) ++ "}";
  cb.field_defs = dcls.field_defs;
  cb.method_defs = dcls.method_defs;
  cb.constructor_defs = dcls.constructor_defs;
  cb.inner_type_defs = dcls.inner_type_defs;
  cb.errors := dcls.errors;

  dcls.env = [] ; 
  dcls.pp_indent = cb.pp_indent + 3;
}

abstract production class_member_dcls_none
cdcls::Class_Member_Dcls ::= {
  cdcls.pp = "";
  cdcls.basepp = "";
  cdcls.field_defs = [];
  cdcls.method_defs = [];
  cdcls.constructor_defs = [];
  cdcls.inner_type_defs = [];
  cdcls.errors := [];
}

abstract production class_member_dcls_snoc
cdcls::Class_Member_Dcls ::= cdcls1::Class_Member_Dcls cdcl::Class_Member_Dcl {
  cdcls.pp = cdcls1.pp ++ "\n" ++ space(cdcl.pp_indent) ++ cdcl.pp;
  cdcls.basepp = cdcls1.basepp ++ "\n" ++ space(cdcl.pp_indent) ++ cdcl.basepp;
  cdcls.field_defs = cdcls1.field_defs ++ cdcl.field_defs;
  cdcls.method_defs = cdcls1.method_defs ++ cdcl.method_defs;
  cdcls.constructor_defs = cdcls1.constructor_defs ++ cdcl.constructor_defs;
  cdcls.inner_type_defs = cdcls1.inner_type_defs ++ cdcl.inner_type_defs;
  cdcls.errors := cdcls1.errors ++ cdcl.errors;
}

abstract production class_member_dcls_one
cdcls::Class_Member_Dcls ::= cdcl::Class_Member_Dcl {
 forwards to  class_member_dcls_snoc( class_member_dcls_none() ,cdcl) ;
}

abstract production class_member_dcl_seq
cdcl::Class_Member_Dcl ::= cdcl1::Class_Member_Dcl cdcl2::Class_Member_Dcl {
  cdcl.pp = cdcl1.pp ++ "\n" ++ space(cdcl.pp_indent) ++ cdcl2.pp;
  cdcl.basepp = cdcl1.basepp ++ "\n" ++ space(cdcl.pp_indent) ++ cdcl2.basepp;
  cdcl.field_defs = cdcl1.field_defs ++ cdcl2.field_defs;
  cdcl.method_defs = cdcl1.method_defs ++ cdcl2.method_defs;
  cdcl.constructor_defs = cdcl1.constructor_defs ++ cdcl2.constructor_defs;
  cdcl.inner_type_defs = cdcl1.inner_type_defs ++ cdcl2.inner_type_defs;
  cdcl.errors := cdcl1.errors ++ cdcl2.errors;
}

abstract production class_member_empty
cdcl::Class_Member_Dcl ::= {
  cdcl.pp = ";";
  cdcl.basepp = ";";
  cdcl.field_defs = [];
  cdcl.method_defs = [];
  cdcl.constructor_defs = [];
  cdcl.inner_type_defs = [];
  cdcl.errors := [];
}

abstract production class_field
cdcl::Class_Member_Dcl ::= f::Field_Dcl {
  cdcl.pp = f.pp;
  cdcl.basepp = f.basepp;
  cdcl.field_defs = f.field_defs;
  cdcl.method_defs = [];
  cdcl.constructor_defs = [];
  cdcl.inner_type_defs = [];
  cdcl.errors := f.errors;
}

abstract production inner_class
cdcl::Class_Member_Dcl ::= cd::Class_Dcl {
  cdcl.pp = cd.pp;
  cdcl.basepp = cd.basepp;
  cdcl.field_defs = [];
  cdcl.method_defs = [];
  cdcl.constructor_defs = [];
  cdcl.inner_type_defs = cd.type_defs;
  cdcl.errors := cd.errors;
}

abstract production class_method
cdcl::Class_Member_Dcl ::= mdcl::Method_Dcl {
  cdcl.pp = mdcl.pp;
  cdcl.basepp = mdcl.basepp;
  cdcl.field_defs = [];
  cdcl.method_defs = mdcl.method_defs;
  cdcl.constructor_defs = [];
  cdcl.inner_type_defs = [];
  cdcl.errors := mdcl.errors;
}

abstract production inner_interface
cdcl::Class_Member_Dcl ::= id::Interface_Dcl {
  cdcl.pp = id.pp;
  cdcl.basepp = id.basepp;
  cdcl.field_defs = [];
  cdcl.method_defs = [];
  cdcl.constructor_defs = [];
  cdcl.inner_type_defs = id.type_defs;
  cdcl.errors := id.errors;
}

abstract production class_block
cdcl::Class_Member_Dcl ::= b::Block {
  cdcl.pp = b.pp;
  cdcl.basepp = b.basepp;
  cdcl.field_defs = [];
  cdcl.method_defs = [];
  cdcl.constructor_defs = [];
  cdcl.inner_type_defs = [];
  cdcl.errors := b.errors;
}

abstract production class_static_initializer
cdcl::Class_Member_Dcl ::= b::Block {
  cdcl.pp = "static " ++ b.pp;
  cdcl.basepp = "static " ++ b.basepp;
  cdcl.field_defs = [];
  cdcl.method_defs = [];
  cdcl.constructor_defs = [];
  cdcl.inner_type_defs = [];
  cdcl.errors := b.errors;
}

