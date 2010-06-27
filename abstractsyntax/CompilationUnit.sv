grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
exports edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:terminals;

nonterminal Root        with errors, pp, basepp, allerrors, type_env, type_defs;
nonterminal Package_Dcl with errors, pp, basepp;
nonterminal Import_Dcls with errors, pp, basepp, pp_indent, type_env;
nonterminal Import_Dcl  with errors, pp, basepp, pp_indent, type_env;
nonterminal Type_Dcls   with errors, pp, basepp, pp_indent, type_env, type_defs;
nonterminal Type_Dcl    with errors, pp, basepp, pp_indent, type_env, type_defs, name;
nonterminal Package     with errors, pp, basepp;

synthesized attribute allerrors :: String;

abstract production compilation_unit
r::Root ::= pd::Package_Dcl ids::Import_Dcls tds::Type_Dcls {
  r.pp = ids.pp ++ "\n" ++  space(tds.pp_indent) ++ tds.pp ++ "\n\n";
  r.basepp = pd.basepp ++ "\n\n" ++  space(ids.pp_indent) ++ ids.basepp ++ "\n\n" ++  space(tds.pp_indent) ++ tds.basepp ++ "\n\n" ;
             -- ++ "/* Extra Stuff \n\n" ++ extra_strings ++ "\n\n\n ..... */\n\n"  ;

  r.errors := pd.errors ++ ids.errors ++ tds.errors;
  r.allerrors = printErrors (r.errors, r.file_name);
  r.type_defs = tds.type_defs;

  ids.pp_indent = 0;
  tds.pp_indent = 0;

  production attribute extra_strings :: String with ++ ;
  extra_strings := "" ;
}

abstract production dummy_compilation_unit
r::Root ::= { 
  r.pp = "dummy compilation unit ................" ;
}

abstract production package_dcl
p::Package_Dcl ::= n::PackageName {
  p.pp = "package " ++ n.pp ++ ";\n\n";
  p.basepp = "package " ++ n.basepp ++ ";";
  p.errors := [];
}

abstract production package_dcl_none
p::Package_Dcl ::= {
  p.pp = "";
  p.basepp = "";
  p.errors := [];
}

abstract production import_dcls_none
idcls::Import_Dcls ::= {
  idcls.pp = "";
  idcls.basepp = "";
  idcls.errors := [];
}

abstract production import_dcls_snoc
idcls::Import_Dcls ::= idcls1::Import_Dcls idcl::Import_Dcl {
  idcls.pp = idcls1.pp ++ "\n" ++ idcl.pp;
  idcls.basepp = idcls1.basepp ++ "\n" ++ space(idcl.pp_indent) ++ idcl.basepp;
  idcls.errors := idcls1.errors ++ idcl.errors;
}

abstract production import_dcl
idcl::Import_Dcl ::= t::Import_t n::TypeName {
  idcl.pp = "import " ++ n.pp ++ ";";
  idcl.basepp = "import " ++ n.basepp ++ ";";
  idcl.errors := [];
}

abstract production import_dcl_on_demand
idcl::Import_Dcl ::= n::PackageOrTypeName {
  idcl.pp = "import " ++ n.pp ++ ".* ;";
  idcl.basepp = "import " ++ n.basepp ++ ".* ;";
  idcl.errors := [];
}

abstract production type_dcls_none
tdcls::Type_Dcls ::= {
  tdcls.pp = "";
  tdcls.basepp = "";
  tdcls.errors := [];
  tdcls.type_defs = [];
}

abstract production type_dcls_snoc
tdcls::Type_Dcls ::= tdcls1::Type_Dcls tdcl::Type_Dcl {
  tdcls.pp = tdcls1.pp ++ "\n\n" ++ space(tdcl.pp_indent) ++ tdcl.pp;
  tdcls.basepp = tdcls1.basepp ++ "\n\n" ++ space(tdcl.pp_indent) ++ tdcl.basepp;
  tdcls.errors := tdcls1.errors ++ tdcl.errors;
  tdcls.type_defs = tdcls1.type_defs ++ tdcl.type_defs;
}

abstract production type_dcl_empty
tdcl::Type_Dcl ::= {
  tdcl.pp = ";";
  tdcl.basepp = ";";
  tdcl.errors := [];
  tdcl.type_defs = [];
}

abstract production package_id
pack::Package ::= id::Id_t {
  pack.pp = id.lexeme;
  pack.basepp = id.lexeme;
  pack.errors := [];
}

abstract production package_snoc
pack1::Package ::= pack2::Package id::Id_t {
  pack1.pp = pack2.pp ++ "." ++ id.lexeme;
  pack1.basepp = pack2.basepp ++ "." ++ id.lexeme;
  pack1.errors := pack2.errors;
}
