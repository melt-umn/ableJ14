grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;
import edu:umn:cs:melt:ableJ14:terminals;

nonterminal Interface_Dcl with              pp, basepp, pp_indent, errors, type_defs, type_env;
nonterminal Interface_Member_Dcls with env, pp, basepp, pp_indent, errors, type_env, enclosingType, field_defs, method_defs, inner_type_defs;
nonterminal Interface_Member_Dcl with  env, pp, basepp, pp_indent, errors, type_env, enclosingType, field_defs, method_defs, inner_type_defs;

abstract production type_interface_dcl
td::Type_Dcl ::= idcl::Interface_Dcl {
  td.pp = idcl.pp;
  td.basepp = idcl.basepp;
  td.type_defs = idcl.type_defs;
  td.errors := idcl.errors;
}

abstract production interface_dcl
idcl::Interface_Dcl ::= mods::Modifiers iname::Id_t inters::TypeNames dcls::Interface_Member_Dcls {
  idcl.pp = mods.pp ++ "interface " ++ iname.lexeme ++ (case inters of type_names_none () -> "" | _ -> " extends " ++ inters.pp end) ++ 
			" {" ++ space(dcls.pp_indent) ++ dcls.pp ++ "\n" ++ space(idcl.pp_indent) ++ "}";
  idcl.basepp = mods.basepp ++ "interface " ++ iname.lexeme ++ (case inters of type_names_none () -> "" | _ -> " extends " ++ inters.basepp end) ++ 
			" {" ++ space(dcls.pp_indent) ++ dcls.basepp ++ "\n" ++ space(idcl.pp_indent) ++ "}";

  dcls.pp_indent = idcl.pp_indent + 3;
  dcls.env = [];

  local attribute thisInterfaceType :: TypeRep;
  thisInterfaceType = retrieveInterface (fqn, idcl.type_env);

  local attribute fqn :: FullyQualifiedName;
  fqn = getQualifiedFQN (idcl.qualifiersSoFar, iname.lexeme);

  idcl.type_defs = [ envItem (	fqn.qualifiedName,
				fqn,
				interfaceDcl (	fqn,
						interfaceTypeRepDefs (interface_type_rep_defs (iname.lexeme, fqn.qualifiedName, mods.modlist, 
									inters.resolvedNames, dcls.field_defs, dcls.method_defs, dcls.inner_type_defs)))) ];

  idcl.errors := mods.errors ++ inters.errors ++ dcls.errors ;

  dcls.qualifiersSoFar = fqn;
  dcls.enclosingType = thisInterfaceType;
  dcls.type_env = thisInterfaceType.interfacetyperep.innerTypes ++ idcl.type_env;
}

abstract production interface_member_dcls_none
idcls::Interface_Member_Dcls ::= {
  idcls.pp = "";
  idcls.basepp = "";
  idcls.errors := [];
  idcls.field_defs = [];
  idcls.method_defs = [];
  idcls.inner_type_defs = [];
}

abstract production interface_member_dcls_snoc
idcls::Interface_Member_Dcls ::= idcls1::Interface_Member_Dcls idcl::Interface_Member_Dcl {
  idcls.pp = idcls1.pp ++ "\n" ++ space(idcl.pp_indent) ++ idcl.pp;
  idcls.basepp = idcls1.basepp ++ "\n" ++ space(idcl.pp_indent) ++ idcl.basepp;
  idcls.errors := idcls1.errors ++ idcl.errors;
  idcls.field_defs = idcls1.field_defs ++ idcl.field_defs;
  idcls.method_defs = idcls1.method_defs ++ idcl.method_defs;
  idcls.inner_type_defs = idcls1.inner_type_defs ++ idcl.inner_type_defs;
}

abstract production interface_field
idcl::Interface_Member_Dcl ::= fd::Field_Dcl {
  idcl.pp = fd.pp;
  idcl.basepp = fd.basepp;
  idcl.errors := fd.errors;
  idcl.field_defs = fd.field_defs;
  idcl.method_defs = [];
  idcl.inner_type_defs = [];
}

abstract production interface_method
idcl::Interface_Member_Dcl ::= mh::Method_Header {
  idcl.pp = mh.pp ++ ";";
  idcl.basepp = mh.basepp ++ ";";
  idcl.errors := mh.errors;
  idcl.field_defs = [];
  idcl.method_defs = mh.method_defs;
  idcl.inner_type_defs = [];
}

abstract production interface_empty
idcl::Interface_Member_Dcl ::= {
  idcl.pp = ";";
  idcl.basepp = ";";
  idcl.errors := [];
  idcl.field_defs = [];
  idcl.method_defs = [];
  idcl.inner_type_defs = [];
}

abstract production interface_inner_class
idcl::Interface_Member_Dcl ::= cd::Class_Dcl {
  idcl.pp = cd.pp;
  idcl.basepp = cd.basepp;
  idcl.field_defs = [];
  idcl.method_defs = [];
  idcl.inner_type_defs = cd.type_defs;
  idcl.errors := cd.errors;
}

abstract production interface_inner_interface
idcl::Interface_Member_Dcl ::= id::Interface_Dcl {
  idcl.pp = id.pp;
  idcl.basepp = id.basepp;
  idcl.errors := id.errors;
  idcl.field_defs = [];
  idcl.method_defs = [];
  idcl.inner_type_defs = id.type_defs;
}
