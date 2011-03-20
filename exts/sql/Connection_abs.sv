grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:abstractsyntax hiding fieldDcl;
import edu:umn:cs:melt:ableJ14:terminals;


nonterminal TableDcl with pp;
nonterminal TableDcls with pp;
nonterminal FieldDcls with pp;
nonterminal FieldDcl with pp;

synthesized attribute table_dcls :: [Decorated TableBinding] 
  occurs on ConnectionDclRep, TableDcls, TableDcl;

synthesized attribute field_dcls :: [Decorated FieldBinding] 
  occurs on FieldDcls, FieldDcl;

nonterminal FieldBinding with field_name, field_type;

synthesized attribute field_name :: String;
synthesized attribute field_type :: Decorated SQLTypeRep;

nonterminal TableBinding with table_name, table_type;

synthesized attribute table_name :: String;
synthesized attribute table_type :: [Decorated FieldBinding];
synthesized attribute url :: String occurs on ConnectionDclRep;

nonterminal ConnectionDclRep with name, fullyQualifiedName, unparse, typerep;
synthesized attribute connection_rep :: ConnectionDclRep occurs on DclRep;

abstract production dcl_rep_connection
di::DclRep ::= ci::ConnectionDclRep {
  
  di.name = ci.name;
  di.unparse = "dcl_rep_connection (" ++ ci.unparse ++ ")";
  di.connection_rep = ci;
  di.is_type = true;
  di.typerep = ci.typerep;

  forwards to dcl_rep_error ();
}

abstract production connection_dcl_rep
ci::ConnectionDclRep ::= n::String fqn::FullyQualifiedName u::String 
                         td::[Decorated TableBinding] tr::TypeRep {
  ci.name = n;
  ci.fullyQualifiedName = fqn;
  ci.url = u;
  ci.table_dcls = td;
  ci.typerep = connectionTypeRep (n, fqn, u, td, tr);
}

function connectionDcl
DclRep ::= n::String fqn::FullyQualifiedName u::String 
           td::[Decorated TableBinding] tr::TypeRep {
  return dcl_rep_connection (connection_dcl_rep (n, fqn, u, td, tr));
}

abstract production connectionTypeRep
top::TypeRep ::= n::String fqn::FullyQualifiedName u::String 
                 td::[Decorated TableBinding] tr::TypeRep {
  forwards to tr'';
}

abstract production type_connection_dcl
s::Type_Dcl ::= c::Id_t loc::Stringconst_t ts::TableDcls {
  local attribute fqn :: FullyQualifiedName;
  fqn = getQualifiedFQN (s.thisPackage, c.lexeme);

  s.pp = "connection " ++ c.lexeme ++ " with " ++ loc.lexeme ++ ts.pp ++ " ;";
  s.type_defs = [ envItem (c.lexeme, fqn, connectionDcl (c.lexeme, fqn, loc.lexeme, ts.table_dcls, tr)) ];
  s.localTypes = [ c.lexeme ];

  local attribute tr :: TypeRep;
  tr = retrieveTypeRep ("java.sql.Connection", s.type_env);

  s.neededFullyQualifiedTypes = [ getQualifiedFQN (getQualifiedFQN (getSimpleFQN ("java"), "sql"), "Connection") ];

  forwards to type_dcl_empty();
}

abstract production class_member_connection_dcl
s::Class_Member_Dcl ::= c::Id_t loc::Stringconst_t ts::TableDcls {
  local attribute fqn :: FullyQualifiedName;
  fqn = getQualifiedFQN (s.qualifiersSoFar, c.lexeme);

  s.pp = "connection " ++ c.lexeme ++ " with " ++ loc.lexeme ++ ts.pp ++ " ;";
  s.inner_type_defs = [ envItem (c.lexeme, fqn, connectionDcl (c.lexeme, fqn, loc.lexeme, ts.table_dcls, tr)) ];
  s.localTypes = [ c.lexeme ];

  local attribute tr :: TypeRep;
  tr = retrieveTypeRep ("java.sql.Connection", s.type_env);

  s.neededFullyQualifiedTypes = [ getQualifiedFQN (getQualifiedFQN (getSimpleFQN ("java"), "sql"), "Connection") ];

  forwards to class_member_empty();
}

abstract production statement_connection_dcl
s::Stmt ::= c::Id_t loc::Stringconst_t ts::TableDcls
{   
  local attribute fqn :: FullyQualifiedName;
  fqn = getQualifiedFQN (s.qualifiersSoFar, c.lexeme);

  s.pp = "connection " ++ c.lexeme ++ " with " ++ loc.lexeme ++ ts.pp ++ " ;";
  s.type_defs = [ envItem (c.lexeme, fqn, connectionDcl (c.lexeme, fqn, loc.lexeme, ts.table_dcls, tr)) ];
  s.localTypes = [ c.lexeme ];

  local attribute tr :: TypeRep;
  tr = retrieveTypeRep ("java.sql.Connection", s.type_env);

  s.neededFullyQualifiedTypes = [ getQualifiedFQN (getQualifiedFQN (getSimpleFQN ("java"), "sql"), "Connection") ];

  forwards to empty_stmt();
}

aspect production convert_env_item
top::ConvertedEnvItem ::= old::EnvItem environment::[ ScopeEnv ] {

 convertedEnvItems <-
	case new (old) of
		  envItem (	name_, 
				_, 
				dcl_rep_connection (connection_dcl_rep (_, fqn, loc, tableDcls, tr)))
		->  
		[  envItem (	name_, 
				fqn, 
				dcl_rep_connection (connection_dcl_rep (name_, fqn, loc, tableDcls, tr))) ]
		|
		_
		->
		[ ]
	end;
}

aspect production disambiguated_name_type
t::Reference_Type ::= n_pp::String n::TypeRep {
  transforms_to <- case n of
			connectionTypeRep (_, _, _, _, _) -> [ connection_ReferenceType (n_pp, n) ] |
			_ -> [ ]
		   end;
}

abstract production connection_ReferenceType
t::Reference_Type ::= n_pp::String n::TypeRep {
  t.pp = n_pp;
  t.basepp = "java.sql.Connection";
  t.errors := [];
  t.typerep = n;
}

abstract production tableDcls_one
ts::TableDcls ::= t::TableDcl
{ 
  ts.pp = t.pp ; 
  ts.table_dcls = t.table_dcls;
}

abstract production tableDcls_cons
ts::TableDcls ::= t::TableDcl  tstail::TableDcls
{ 
  ts.pp = t.pp ++ ",\n        " ++ tstail.pp ; 
  ts.table_dcls = t.table_dcls ++ tstail.table_dcls;
}

abstract production tableDcl
t::TableDcl ::= tname::SQL_Id_t fs::FieldDcls 
{
  t.pp = "table " ++ tname.lexeme ++ " [ " ++ fs.pp ++ " ] " ;    
  t.table_dcls = [decorate tableBinding(tname.lexeme, fs.field_dcls) with {}];
}

abstract production fieldDcls_one
fs::FieldDcls ::= f::FieldDcl
{ 
  fs.pp = f.pp ; 
  fs.field_dcls = f.field_dcls;
}

abstract production fieldDcls_cons
fs::FieldDcls ::= f::FieldDcl fstail::FieldDcls
{ 
  fs.pp = f.pp ++ ", " ++ fstail.pp ; 
  fs.field_dcls = f.field_dcls ++ fstail.field_dcls;
}

abstract production fieldDcl_SQL
f::FieldDcl ::=  t::SQL_Type fname::SQL_Id_t
{ 
  f.pp = fname.lexeme ++ " " ++ t.pp ; 
  f.field_dcls = [decorate fieldBinding(fname.lexeme, t.sqlTypeRep) with {}];
}

abstract production fieldBinding
top::FieldBinding ::= n::String t::Decorated SQLTypeRep{
  top.field_name = n;
  top.field_type = t;
}

abstract production tableBinding
top::TableBinding ::= n::String t::[Decorated FieldBinding]{
  top.table_name = n;
  top.table_type = t;
}


abstract production register
top::Stmt ::= s::String {
  top.pp = "register driver " ++ s ++ ";";
  forwards to stmt_stmt_expr (method_call (qualified_method_name (simple_ambiguous_name (terminal (Id_t, "Class")), terminal (Id_t, "forName")), exprs_one (string_const("\"" ++  s ++ "\""))));
}
