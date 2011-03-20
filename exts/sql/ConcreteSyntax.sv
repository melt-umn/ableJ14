grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax;

----------------------------------------------------------------------
-- Concrete syntax specifications for the SQL extension. 
--
-- This file contains concrete syntax specifications for 
-- the "connection" construct, 
-- the "register", 
-- the "establish" construct, and
-- the "using" constuct.
--
-- The concrete syntax for SQL is found in the SQL grammar "SQL".
----------------------------------------------------------------------

nonterminal TableDcl_c;
nonterminal TableDcls_c;
nonterminal FieldDcls_c;
nonterminal FieldDcl_c;

terminal Conn_t      'connection' lexer classes { sql, sql_kwd } , 
                                  dominates { Id_t } ;
terminal With_t      'with'       lexer classes { sql, sql_kwd } ;
terminal Table_t     'table'      lexer classes { sql, sql_kwd } ;
terminal Register_t  'register'   lexer classes { sql, sql_kwd } , 
                                  dominates { Id_t } ;
terminal Driver_t    'driver'     lexer classes { sql, sql_kwd } ;
terminal Establish_t 'establish'  lexer classes { sql, sql_kwd } , 
                                  dominates { Id_t } ;
terminal Using_t     'using'      lexer classes { sql, sql_kwd } , 
                                  dominates { Id_t } ;
terminal Query_t     'query'      lexer classes { sql, sql_kwd } ;


synthesized attribute ast_FieldDcl  :: FieldDcl  occurs on FieldDcl_c ;
synthesized attribute ast_FieldDcls :: FieldDcls occurs on FieldDcls_c ;

synthesized attribute ast_TableDcl  :: TableDcl  occurs on TableDcl_c ;
synthesized attribute ast_TableDcls :: TableDcls occurs on TableDcls_c ;


-- connection
--------------------------------------------------
concrete production type_connection_dcl_c
s::typeDefinition ::= 'connection' c::Id_t loc::Stringconst_t 'with' ts::TableDcls_c ';'
{   
  s.ast_Type_Dcl = type_connection_dcl(c, loc, ts.ast_TableDcls);
}

concrete production class_member_connection_dcl_c
s::classMemberDefinition ::= 'connection' c::Id_t loc::Stringconst_t 'with' ts::TableDcls_c ';'
{   
  s.ast_Class_Member_Dcl = class_member_connection_dcl(c, loc, ts.ast_TableDcls);
}

concrete production statement_connection_dcl_c
s::statement ::= 'connection' c::Id_t loc::Stringconst_t 'with' ts::TableDcls_c ';'
{   
  s.ast_Stmt = statement_connection_dcl(c, loc, ts.ast_TableDcls);
}

concrete production tableDcls_one_C
ts::TableDcls_c ::= t::TableDcl_c
{
  ts.ast_TableDcls = tableDcls_one(t.ast_TableDcl);
}

concrete production tableDcls_cons_C
ts::TableDcls_c ::= t::TableDcl_c ',' tstail::TableDcls_c
{
  ts.ast_TableDcls = tableDcls_cons(t.ast_TableDcl, tstail.ast_TableDcls);
}

concrete production tableDcl_C
t::TableDcl_c ::= 'table' tname::SQL_Id_t '[' fs::FieldDcls_c ']'
{
  t.ast_TableDcl = tableDcl(tname, fs.ast_FieldDcls);
}

concrete production fieldDcls_one_c
fs::FieldDcls_c ::= f::FieldDcl_c
{
  fs.ast_FieldDcls = fieldDcls_one(f.ast_FieldDcl);
}

concrete production fieldDcls_cons_c
fs::FieldDcls_c ::= f::FieldDcl_c ',' fstail::FieldDcls_c
{
  fs.ast_FieldDcls = fieldDcls_cons(f.ast_FieldDcl, fstail.ast_FieldDcls);
}

concrete production fieldDcl_c
f::FieldDcl_c ::=  t::SQL_Type_c fname::SQL_Id_t
{
  f.ast_FieldDcl = fieldDcl_SQL(t.ast_SQL_Type, fname);
}


-- register
--------------------------------------------------
concrete production register_c
top::statement ::= 'register' 'driver' s::Stringconst_t ';'
{
 top.ast_Stmt = register (removeQuotes2 (s.lexeme));
}


-- establish
--------------------------------------------------
concrete production establish_c
e::statement ::= 'establish'  c::Id_t ';'
{
  e.ast_Stmt = establish_a(c) ;
} 



-- using - query
--------------------------------------------------
concrete production using_query
e::primaryExpression ::= 'using'  c::Id_t 'query' '{' s::SQL_Stmt_c '}'
{
  e.ast_Expr = a_using_query(c, s.ast_SQL_Stmt);
}

function removeQuotes2
String ::= s::String {
  return substring (1, length (s) - 1, s);
}
