grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:abstractsyntax ;

nonterminal SQL_Type with sqlTypeRep;




-- Abstract Syntax --

attribute pp occurs on SQL_Type;

abstract production sql_type_varchar
t::SQL_Type ::= n::VARCHAR_t
{
  t.pp = n.lexeme;
  t.sqlTypeRep = sqlString();
}

abstract production sql_type_integer
t::SQL_Type ::= n::INTEGER_t
{
  t.pp = n.lexeme;
  t.sqlTypeRep = sqlInteger();
}


abstract production sql_type_float
t::SQL_Type ::= n::FLOAT_t
{
  t.pp = n.lexeme;
  t.sqlTypeRep = sqlFloat();
}

abstract production sql_type_boolean
t::SQL_Type ::= n::BOOLEAN_t
{
  t.pp = n.lexeme;
  t.sqlTypeRep = sqlBoolean();
}



