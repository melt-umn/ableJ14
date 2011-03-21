grammar edu:umn:cs:melt:ableJ14:exts:sql ;
-- export edu:umn:cs:melt:ableJ14:exts:sql:SQL ;


import edu:umn:cs:melt:ableJ14:abstractsyntax ;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals ;



nonterminal SQL_Stmt with pp;
nonterminal Select_Qualifier with pp;
nonterminal Table_List with pp;
nonterminal Where_Clause with pp ;


autocopy attribute tableDefs :: [Decorated TableBinding] ;
attribute tableDefs occurs on SQL_Stmt, Table_List, SQL_Expr_List, SQL_Expr, Where_Clause;
attribute env occurs on SQL_Stmt, Table_List, SQL_Expr_List, SQL_Expr, Where_Clause;


abstract production select_stmt
s::SQL_Stmt ::=  quals::Select_Qualifier 
                 con_select_list::SQL_Expr_List 
                 tables::Table_List
                 where::Where_Clause                  
{
  s.pp = "SELECT " ++ quals.pp ++ " " ++ con_select_list.pp ++ 
         " FROM " ++ tables.pp ++ " " ++ where.pp ;

  con_select_list.tableDefs = tables.selectTables;

  s.sqlErrors = quals.sqlErrors ++ con_select_list.sqlErrors ++ 
                tables.sqlErrors ++ where.sqlErrors;

  
  s.baseJava
    = if   where.emptyWhere 
      then p(sc("select"), 
            p(quals.baseJava, 
             p(con_select_list.baseJava, 
              p(sc("from"), tables.baseJava))))
      else p(sc("select"), 
            p(quals.baseJava, 
             p(con_select_list.baseJava, 
              p(sc("from"), 
               p(tables.baseJava, 
                p(sc("where"), where.baseJava))))));
}


-- Select Qualifier --
abstract production all_qualifier
top::Select_Qualifier ::= 
{ top.pp = " ALL " ; 
  top.sqlErrors = [];
  top.baseJava = sc("all");
}

abstract production distinct_qualifier
top::Select_Qualifier ::= 
{ top.pp = " DISTINCT " ;
  top.sqlErrors = [];
  top.baseJava = sc("distinct");
}

abstract production empty_qualifier
top::Select_Qualifier ::=
{ top.pp = "" ;
  top.sqlErrors = [];
  top.baseJava = sc("");
}

-- Select List --
abstract production select_all
top::SQL_Expr_List ::= 
{
  top.pp = " * ";
  top.errors := [];
  top.sqlErrors = [];
  top.baseJava = sc("*");
}


-- Table List --
abstract production table_list_one
top::Table_List ::= t::SQL_Id_t
{
  top.pp = t.lexeme ;
  local attribute tb :: [Decorated TableBinding];
  tb = findTable(t.lexeme, top.tableDefs);

  local attribute e1 :: [Error];
  e1 = if null(tb) then [mkError(t.line, "Table '" ++ t.lexeme ++ "' cannot be found.")] else [];

  top.sqlErrors = e1;
  top.selectTables = tb;
  top.baseJava = sc(t.lexeme);
}

abstract production table_list_cons
top::Table_List ::= t::SQL_Id_t  ttail::Table_List
{
  top.pp = t.lexeme ++ "," ++ ttail.pp ;
  local attribute tb :: [Decorated TableBinding];
  tb = findTable(t.lexeme, top.tableDefs);

  local attribute e1 :: [Error];
  e1 = if null(tb) then [mkError(t.line, "Table '" ++ t.lexeme ++ "' cannot be found.")] else [];

  top.sqlErrors = e1 ++ ttail.sqlErrors;
  top.selectTables = tb ++ ttail.selectTables;
  top.baseJava = p(sc(t.lexeme ++ ","), ttail.baseJava);
}



-- Where Clause
synthesized attribute emptyWhere :: Boolean occurs on Where_Clause;

abstract production where_clause
top::Where_Clause ::= w::WHERE_t conditions::SQL_Expr
{
  top.pp = "WHERE " ++ conditions.pp ;
  top.emptyWhere = false;
  top.sqlErrors = (if conditions.sqlTypeRep.isBoolean then [] else [mkError(w.line, "'where' clause must be of type boolean")]) ++ conditions.sqlErrors;
  top.baseJava = conditions.baseJava;
}

abstract production empty_where
top::Where_Clause ::=
{ 
  top.emptyWhere = true;
  top.sqlErrors = [];
  top.baseJava = sc("");
}



synthesized attribute baseJava :: Expr;
attribute baseJava occurs on SQL_Stmt, Select_Qualifier, SQL_Expr_List, Table_List, Where_Clause, SQL_Expr;

synthesized attribute sqlErrors :: [Error];
attribute sqlErrors occurs on SQL_Stmt, Select_Qualifier, SQL_Expr_List, Table_List, Where_Clause, SQL_Expr;

synthesized attribute selectTables :: [Decorated TableBinding] occurs on Table_List;

abstract production p
e::Expr ::= e1::Expr e2::Expr {
  forwards to plus(e1, terminal(Plus_t, "+"), e2);
}


function sc
Expr ::= s::String  {
  return string_const("\"" ++  s ++ " \"");
}

function findFieldHelp
[Decorated FieldBinding] ::= f::String fb::[Decorated FieldBinding]{
  return if null(fb) then [] else (if f == head(fb).field_name then [head(fb)] else []) ++ findFieldHelp(f, tail(fb));
}

function findField
[Decorated FieldBinding] ::= f::String tb::[Decorated TableBinding]{
  return if null(tb) then [] else findFieldHelp(f, head(tb).table_type) ++ findField(f, tail(tb));
}

function findTable
[Decorated TableBinding] ::= t::String tb::[Decorated TableBinding]{
  return if null(tb) then [] else (if t == head(tb).table_name then [head(tb)] else []) ++ findTable(t, tail(tb));
}
