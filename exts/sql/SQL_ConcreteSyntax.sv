grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:terminals;

----------------------------------------------------------------------
-- SQL Concrete Syntax Specifications
----------------------------------------------------------------------
-- This file contains the concrete syntax specifications for SQL -
-- both SQL statements (directly below), SQL expressiosn (further
-- below), and SQL types (at the bottom).
----------------------------------------------------------------------


----------------------------------------------------------------------
-- SQL Statements
----------------------------------------------------------------------

terminal SELECT_t   /(SELECT)|(select)/       lexer classes { sql, sql_kwd } ;
terminal WHERE_t    /(WHERE)|(where)/         lexer classes { sql, sql_kwd } ;
terminal FROM_t     /(FROM)|(from)/           lexer classes { sql, sql_kwd } ;
terminal ALL_t      /(ALL)|(all)/             lexer classes { sql, sql_kwd } ;
terminal DISTINCT_t /(DISTINCT)|(distinct)/   lexer classes { sql, sql_kwd } ;

nonterminal SQL_Stmt_c, Select_Qualifier_c, Select_List_c, Select_SubList_c, Table_List_c, 
            Where_Clause_c, Order_By_Clause_c ;

synthesized attribute ast_SQL_Stmt :: SQL_Stmt occurs on SQL_Stmt_c ;
synthesized attribute ast_Select_Qualifier :: Select_Qualifier  occurs on Select_Qualifier_c ;
synthesized attribute ast_SQL_Expr_List :: SQL_Expr_List occurs on Select_List_c, Select_SubList_c ;
synthesized attribute ast_Table_List :: Table_List occurs on Table_List_c ;
synthesized attribute ast_Where_Clause :: Where_Clause occurs on Where_Clause_c ;


concrete production select_stmt_c
s::SQL_Stmt_c ::= skwd::SELECT_t  quals::Select_Qualifier_c  con_select_list::Select_List_c 
                  f::FROM_t  tables::Table_List_c  where::Where_Clause_c 
                  -- orderby::OrderByClause_c
{ s.ast_SQL_Stmt = select_stmt ( quals.ast_Select_Qualifier, con_select_list.ast_SQL_Expr_List, 
                                 tables.ast_Table_List, where.ast_Where_Clause) ; -- , orderby.ast_OrderByClause ) ;
}


-- Select Qualifier --
concrete production all_qualifier_c
top::Select_Qualifier_c ::= all_term::ALL_t
{ top.ast_Select_Qualifier = all_qualifier();
}

concrete production distinct_qualifier_c
top::Select_Qualifier_c ::= distinct_term::DISTINCT_t
{ top.ast_Select_Qualifier = distinct_qualifier();
}

concrete production empty_qualifier_c
top::Select_Qualifier_c ::=
{ top.ast_Select_Qualifier = empty_qualifier();
}

-- Select List --
concrete production select_all_c
top::Select_List_c ::= kleene_star::Mul_t
{ top.ast_SQL_Expr_List = select_all();
}

concrete production select_sublist_c
top::Select_List_c ::= subList::Select_SubList_c
{ top.ast_SQL_Expr_List = subList.ast_SQL_Expr_List;
}

concrete production select_sublist_list_c
top::Select_SubList_c ::= body::Select_SubList_c ',' tail_l::SQL_Expr_c
{ top.ast_SQL_Expr_List = sql_exprs_snoc(body.ast_SQL_Expr_List, tail_l.ast_SQL_Expr);
}

concrete production just_select_sublist_c
top::Select_SubList_c ::= sub_list::SQL_Expr_c
{ top.ast_SQL_Expr_List = sql_exprs_one(sub_list.ast_SQL_Expr);
}

-- Table List --
concrete production table_list_one_c
top::Table_List_c ::= t::SQL_Id_t
{ top.ast_Table_List = table_list_one(t) ;
}

concrete production table_list_cons_c
top::Table_List_c ::= t::SQL_Id_t ',' ttail::Table_List_c
{ top.ast_Table_List = table_list_cons(t, ttail.ast_Table_List) ;
}

-- Where Clause
concrete production where_clause_c
top::Where_Clause_c ::= w::WHERE_t conditions::SQL_Expr_c
{ top.ast_Where_Clause = where_clause(w, conditions.ast_SQL_Expr);
}

concrete production empty_where_c
top::Where_Clause_c ::=
{ top.ast_Where_Clause = empty_where();
}

----------------------------------------------------------------------
-- SQL Expressions
----------------------------------------------------------------------

terminal SQL_Or_t    /[oO][rR]/      lexer classes { sql_kwd }, precedence = 5, association = left;
terminal SQL_And_t   /[aA][nN][dD]/  lexer classes { sql_kwd }, precedence = 5, association = left;
terminal SQL_Not_t   /[nN][oO][tT]/  lexer classes { sql_kwd }, precedence = 15, association = right;

terminal SQL_DOT_t     '.'  ;
--terminal SQL_LT_t      '<'  ;
--terminal SQL_LTEQ_t    '<=' ;
--terminal SQL_GT_t      '>'  ;
--terminal SQL_GTEQ_t    '>=' ;
terminal SQL_NEQ_t     '<>' ;
--terminal SQL_EQ_t      '=' ;
--terminal SQL_Plus_t    '+' ;
--terminal SQL_Dash_t    '-' ;
--terminal SQL_Mul_t     '*' ;
--terminal SQL_Divide_t  '/' ;
--terminal Comma_t       ',' ;

terminal String_Const_t    /[\"]([^\"\\]|[\\][btnfr\"\'\\]|[\\][0-7]|[\\][0-7][0-7]|[\\][0-3][0-7][0-7]|[\\][u]+[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)*[\"]/ ;
terminal Int_Const_t       /([0-9]+|0[xX][0-9a-fA-F]+)[lL]?/ ;
terminal Float_Const_t     /((([0-9]+[\.][0-9]*|[\.][0-9]+)([eE][\-\+]?[0-9]+)?|[0-9]+[eE][\-\+]?[0-9]+)[fFdD]?)|([0-9]+([eE][\-\+]?[0-9]+)?[fFdD])/  ;
--terminal LP '(' ; 
--terminal RP ')' ;


nonterminal SQL_Expr_c, SQL_Boole_Expr_c, SQL_Boole_Term_c, SQL_Boole_Factor_c, SQL_Rel_Expr_c, 
            SQL_Arith_Expr_c, SQL_Arith_Term_c, SQL_Arith_Factor_c, SQL_Primary_c ;

synthesized attribute ast_SQL_Expr :: SQL_Expr occurs on SQL_Expr_c, SQL_Boole_Expr_c, SQL_Boole_Term_c, SQL_Boole_Factor_c, 
                            SQL_Rel_Expr_c, SQL_Arith_Expr_c, SQL_Arith_Term_c, SQL_Arith_Factor_c, SQL_Primary_c ;

concrete production sql_expr_bool_c
top::SQL_Expr_c ::= e::SQL_Boole_Expr_c
{
  top.ast_SQL_Expr = e.ast_SQL_Expr;
}

concrete production sql_exp_or_c
top::SQL_Boole_Expr_c ::=  e::SQL_Boole_Expr_c  o::SQL_Or_t  t::SQL_Boole_Term_c
{
  top.ast_SQL_Expr = sql_expr_or(e.ast_SQL_Expr,o,t.ast_SQL_Expr);
}

concrete production search_cond_leaf_c
top::SQL_Boole_Expr_c ::= b_term::SQL_Boole_Term_c
{
  top.ast_SQL_Expr = b_term.ast_SQL_Expr;
}

concrete production bool_term_tree_c
top::SQL_Boole_Term_c ::= b_term::SQL_Boole_Term_c and_op::SQL_And_t b_factor::SQL_Boole_Factor_c
{ 
  top.ast_SQL_Expr = bool_term_tree(b_term.ast_SQL_Expr, and_op, b_factor.ast_SQL_Expr);
}

concrete production bool_term_leaf_c
top::SQL_Boole_Term_c ::= b_factor::SQL_Boole_Factor_c
{
  top.ast_SQL_Expr = b_factor.ast_SQL_Expr;
}

concrete production bool_factor_negated_c
top::SQL_Boole_Factor_c ::= not_op::SQL_Not_t b_prime::SQL_Rel_Expr_c
{
  top.ast_SQL_Expr = bool_factor_negated(not_op, b_prime.ast_SQL_Expr);
}

concrete production bool_factor_reg_c
top::SQL_Boole_Factor_c ::= b_prime::SQL_Rel_Expr_c
{
  top.ast_SQL_Expr = b_prime.ast_SQL_Expr;
}

concrete production comparison_less_c
top::SQL_Rel_Expr_c ::= lhs::SQL_Rel_Expr_c less_than::Lt_t rhs::SQL_Arith_Expr_c
{
  top.ast_SQL_Expr = comparison_less(lhs.ast_SQL_Expr, less_than, rhs.ast_SQL_Expr);
}

concrete production comparison_less_eq_c
top::SQL_Rel_Expr_c ::= lhs::SQL_Rel_Expr_c less_eq::LtEq_t rhs::SQL_Arith_Expr_c
{
  top.ast_SQL_Expr = comparison_less_eq(lhs.ast_SQL_Expr, less_eq, rhs.ast_SQL_Expr);
}

concrete production comparison_greater_c
top::SQL_Rel_Expr_c ::= lhs::SQL_Rel_Expr_c greater_than::Gt_t rhs::SQL_Arith_Expr_c
{
  top.ast_SQL_Expr = comparison_greater(lhs.ast_SQL_Expr, greater_than, rhs.ast_SQL_Expr);
}

concrete production comparison_greater_eq_c
top::SQL_Rel_Expr_c ::= lhs::SQL_Rel_Expr_c greater_than_eq::GtEq_t rhs::SQL_Arith_Expr_c
{ 
  top.ast_SQL_Expr = comparison_greater_eq(lhs.ast_SQL_Expr, greater_than_eq, rhs.ast_SQL_Expr);
}

concrete production comparison_not_eq_c
top::SQL_Rel_Expr_c ::= lhs::SQL_Rel_Expr_c op::SQL_NEQ_t rhs::SQL_Arith_Expr_c
{
  top.ast_SQL_Expr = comparison_not_eq(lhs.ast_SQL_Expr, op, rhs.ast_SQL_Expr);
}

concrete production comparison_equal_c
top::SQL_Rel_Expr_c ::= lhs::SQL_Rel_Expr_c equal::Eq_t rhs::SQL_Arith_Expr_c
{
  top.ast_SQL_Expr = comparison_equal(lhs.ast_SQL_Expr, equal, rhs.ast_SQL_Expr);
}

concrete production decorate_comp_compexpr_attr_C
top::SQL_Rel_Expr_c ::= the_expr::SQL_Arith_Expr_c
{
  top.ast_SQL_Expr = the_expr.ast_SQL_Expr;
}

----------------------------
-- Arithmetic Expressions --
----------------------------
concrete production sql_add_c
top::SQL_Arith_Expr_c ::= l::SQL_Arith_Expr_c o::Plus_t r::SQL_Arith_Term_c
{
  top.ast_SQL_Expr = sql_add(l.ast_SQL_Expr,o, r.ast_SQL_Expr);
}

concrete production sql_sub_c
top::SQL_Arith_Expr_c ::= l::SQL_Arith_Expr_c o::Minus_t r::SQL_Arith_Term_c
{
  top.ast_SQL_Expr = sql_sub(l.ast_SQL_Expr, o, r.ast_SQL_Expr);
}

concrete production sql_expr_term_c
top::SQL_Arith_Expr_c ::= r::SQL_Arith_Term_c
{
  top.ast_SQL_Expr = r.ast_SQL_Expr;
}

concrete production sql_multiply_c
top::SQL_Arith_Term_c ::= l::SQL_Arith_Term_c o::Mul_t r::SQL_Arith_Factor_c
{
  top.ast_SQL_Expr = sql_multiply(l.ast_SQL_Expr, o, r.ast_SQL_Expr);
}

concrete production sql_divide_c
top::SQL_Arith_Term_c ::= l::SQL_Arith_Term_c o::Div_t r::SQL_Arith_Factor_c
{
  top.ast_SQL_Expr = sql_divide(l.ast_SQL_Expr, o, r.ast_SQL_Expr);
}

concrete production sql_term_factor_c
top::SQL_Arith_Term_c ::= l::SQL_Arith_Factor_c
{
  top.ast_SQL_Expr = l.ast_SQL_Expr;
}

concrete production sql_negate_c
top::SQL_Arith_Factor_c ::= o::Minus_t e::SQL_Primary_c
{
  top.ast_SQL_Expr = sql_negate(o, e.ast_SQL_Expr);
}

concrete production sql_positive_c
top::SQL_Arith_Factor_c ::= o::Plus_t e::SQL_Primary_c
{
  top.ast_SQL_Expr = sql_positive(o, e.ast_SQL_Expr);
}

concrete production sql_primary_c
top::SQL_Arith_Factor_c ::= e::SQL_Primary_c
{
  top.ast_SQL_Expr = e.ast_SQL_Expr;
}

concrete production column_expr_c
top::SQL_Primary_c ::= c::SQL_Id_t
{
  top.ast_SQL_Expr = column_expr(c);
}

concrete production column_expr2_c
top::SQL_Primary_c ::= c1::SQL_Id_t d::SQL_DOT_t c2::SQL_Id_t
{
  top.ast_SQL_Expr = column_expr2(c1, d, c2);
}

concrete production literal_primary_c
top::SQL_Primary_c ::= literal_arg::String_Const_t
{
  top.ast_SQL_Expr = literal_const(literal_arg);
}

concrete production unsigned_int_primary_C
top::SQL_Primary_c ::= uint::Int_Const_t
{
  top.ast_SQL_Expr = unsigned_int_literal(uint);
}

concrete production float_primary_C
top::SQL_Primary_c ::= f::Float_Const_t
{
  top.ast_SQL_Expr = float_literal(f);
}

concrete production sub_expr_C
top::SQL_Primary_c ::= '(' e::SQL_Expr_c ')' 
{
  top.ast_SQL_Expr = sub_expr(e.ast_SQL_Expr);
}







----------------------------------------------------------------------
-- SQL Types
----------------------------------------------------------------------

nonterminal SQL_Type_c ;

terminal VARCHAR_t 'VARCHAR'  lexer classes { sql_kwd }  ;
terminal INTEGER_t 'INTEGER'  lexer classes { sql_kwd }  ;
terminal FLOAT_t   'FLOAT'    lexer classes { sql_kwd }  ;
terminal BOOLEAN_t 'BOOLEAN'  lexer classes { sql_kwd }  ;

synthesized attribute ast_SQL_Type :: SQL_Type occurs on SQL_Type_c;


concrete production sql_type_varchar_c
t::SQL_Type_c ::= n::VARCHAR_t
{
  t.ast_SQL_Type =  sql_type_varchar(n);
}

concrete production sql_type_integer_c
t::SQL_Type_c ::= n::INTEGER_t
{
  t.ast_SQL_Type =  sql_type_integer(n);
}

concrete production sql_type_float_c
t::SQL_Type_c ::= n::FLOAT_t
{
  t.ast_SQL_Type =  sql_type_float(n);
}

concrete production sql_type_boolean_c
t::SQL_Type_c ::= n::BOOLEAN_t
{
  t.ast_SQL_Type =  sql_type_boolean(n);
}

