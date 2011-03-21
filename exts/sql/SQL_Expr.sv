grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:abstractsyntax ;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals ;

synthesized attribute sqlTypeRep :: Decorated SQLTypeRep;
nonterminal SQL_Expr with pp, errors, sqlTypeRep;
nonterminal SQL_Expr_List with pp, errors, sqlTypeRep;


abstract production sql_exprs_one
top::SQL_Expr_List ::= select::SQL_Expr
{
  top.pp = select.pp;
  top.sqlErrors = select.sqlErrors;
  top.baseJava = select.baseJava;
--  top.errors := select.errors;
}

abstract production sql_exprs_snoc
top::SQL_Expr_List ::= body_el::SQL_Expr_List tail_el::SQL_Expr
{
  top.pp = body_el.pp ++ " , " ++ tail_el.pp;
--  top.errors := body_el.errors ++ tail_el.errors;
  top.sqlErrors = body_el.sqlErrors ++ tail_el.sqlErrors;
  top.baseJava = p(body_el.baseJava, p(sc(","), tail_el.baseJava));
}

abstract production sql_expr_or
top::SQL_Expr ::=  e::SQL_Expr o::SQL_Or_t t::SQL_Expr
{
  top.pp = e.pp ++ " " ++ o.lexeme ++ " " ++ t.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if e.sqlTypeRep.isBoolean && t.sqlTypeRep.isBoolean then [] else [mkError(o.line, "Operands to 'or' must be of type boolean.")])
			 ++ e.sqlErrors ++ t.sqlErrors;
  top.baseJava =  p(e.baseJava, p(sc("and"), t.baseJava));
}

abstract production bool_term_tree
top::SQL_Expr ::= b_term::SQL_Expr op::SQL_And_t b_factor::SQL_Expr
{ 
  top.pp = b_term.pp ++ " " ++ op.lexeme ++ " " ++ b_factor.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if b_term.sqlTypeRep.isBoolean && b_factor.sqlTypeRep.isBoolean then [] else [mkError(op.line, "Operands to 'and' must be of type boolean.")])
			 ++ b_term.sqlErrors ++ b_factor.sqlErrors;
  top.baseJava =  p(b_term.baseJava, p(sc("and"), b_factor.baseJava));
}


abstract production bool_factor_negated
top::SQL_Expr ::= op::SQL_Not_t b_prime::SQL_Expr
{
  top.pp = op.lexeme ++ " " ++ b_prime.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if b_prime.sqlTypeRep.isBoolean then [] else [mkError(op.line, "Operands to ! must be of type boolean.")])
			 ++ b_prime.sqlErrors;

  top.baseJava = p(sc("not"), b_prime.baseJava);
}



abstract production comparison_less
top::SQL_Expr ::= lhs::SQL_Expr op::Lt_t rhs::SQL_Expr
{ 
  top.pp = lhs.pp ++ " " ++ op.lexeme ++ " " ++ rhs.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if (lhs.sqlTypeRep.isInteger || lhs.sqlTypeRep.isFloat) && (rhs.sqlTypeRep.isInteger || rhs.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to < must be numeric.")])
			 ++ lhs.sqlErrors ++ rhs.sqlErrors;

  top.baseJava =  p(lhs.baseJava, p(sc("<"), rhs.baseJava));
}

abstract production comparison_less_eq
top::SQL_Expr ::= lhs::SQL_Expr op::LtEq_t rhs::SQL_Expr
{ 
  top.pp = lhs.pp ++ " " ++ op.lexeme ++ " " ++ rhs.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if (lhs.sqlTypeRep.isInteger || lhs.sqlTypeRep.isFloat) && (rhs.sqlTypeRep.isInteger || rhs.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to <= must be numeric.")])
			 ++ lhs.sqlErrors ++ rhs.sqlErrors;

  top.baseJava =  p(lhs.baseJava, p(sc("<="), rhs.baseJava));
}

abstract production comparison_greater
top::SQL_Expr ::= lhs::SQL_Expr op::Gt_t rhs::SQL_Expr
{ 
  top.pp = lhs.pp ++ " " ++ op.lexeme ++ " " ++ rhs.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if (lhs.sqlTypeRep.isInteger || lhs.sqlTypeRep.isFloat) && (rhs.sqlTypeRep.isInteger || rhs.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to > must be numeric.")])
			 ++ lhs.sqlErrors ++ rhs.sqlErrors;

  top.baseJava =  p(lhs.baseJava, p(sc(">"), rhs.baseJava));
}

abstract production comparison_greater_eq
top::SQL_Expr ::= lhs::SQL_Expr op::GtEq_t rhs::SQL_Expr
{ 
  top.pp = lhs.pp ++ " " ++ op.lexeme ++ " " ++ rhs.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if (lhs.sqlTypeRep.isInteger || lhs.sqlTypeRep.isFloat) && (rhs.sqlTypeRep.isInteger || rhs.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to >= must be numeric.")])
			 ++ lhs.sqlErrors ++ rhs.sqlErrors;
  top.baseJava =  p(lhs.baseJava, p(sc(">="), rhs.baseJava));
}

abstract production comparison_not_eq
top::SQL_Expr ::= lhs::SQL_Expr op::SQL_NEQ_t rhs::SQL_Expr
{ 
  top.pp = lhs.pp ++ " " ++ op.lexeme ++ " " ++ rhs.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if 	(lhs.sqlTypeRep.isInteger && rhs.sqlTypeRep.isInteger) || 
			(lhs.sqlTypeRep.isFloat && rhs.sqlTypeRep.isFloat) || 
			(lhs.sqlTypeRep.isString && rhs.sqlTypeRep.isString) || 
			(lhs.sqlTypeRep.isBoolean && rhs.sqlTypeRep.isBoolean) 

		   then [] else [mkError(op.line, "Operands to <> must be of the same type.")])

			 ++ lhs.sqlErrors ++ rhs.sqlErrors;

  top.baseJava =  p(lhs.baseJava, p(sc("<>"), rhs.baseJava));
}

abstract production comparison_equal
top::SQL_Expr ::= lhs::SQL_Expr op::Eq_t rhs::SQL_Expr
{ 
  top.pp = lhs.pp ++ " " ++ op.lexeme ++ " " ++ rhs.pp ;
  top.sqlTypeRep = sqlBoolean();
  top.sqlErrors = (if 	(lhs.sqlTypeRep.isInteger && rhs.sqlTypeRep.isInteger) || 
			(lhs.sqlTypeRep.isFloat && rhs.sqlTypeRep.isFloat) || 
			(lhs.sqlTypeRep.isString && rhs.sqlTypeRep.isString) || 
			(lhs.sqlTypeRep.isBoolean && rhs.sqlTypeRep.isBoolean) 

		   then [] else [mkError(op.line, "Operands to = must be of the same type.")])

			 ++ lhs.sqlErrors ++ rhs.sqlErrors;

  top.baseJava =  p(lhs.baseJava, p(sc("="), rhs.baseJava));
}

abstract production sql_add
top::SQL_Expr ::= l::SQL_Expr op::Plus_t r::SQL_Expr
{
  top.pp = l.pp ++ " " ++ op.lexeme ++ " " ++ r.pp ;
  top.sqlTypeRep = if l.sqlTypeRep.isFloat || r.sqlTypeRep.isFloat then sqlFloat() else sqlInteger();
  top.sqlErrors = (if (l.sqlTypeRep.isInteger || l.sqlTypeRep.isFloat) && (r.sqlTypeRep.isInteger || r.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to + must be numeric.")])  ++ l.sqlErrors ++ r.sqlErrors;
  top.baseJava =  p(l.baseJava, p(sc("+"), r.baseJava));
}

abstract production sql_sub
top::SQL_Expr ::= l::SQL_Expr op::Minus_t r::SQL_Expr
{
  top.pp = l.pp ++ " " ++ op.lexeme ++ " " ++ r.pp ;
  top.sqlTypeRep = if l.sqlTypeRep.isFloat || r.sqlTypeRep.isFloat then sqlFloat() else sqlInteger();
  top.sqlErrors = (if (l.sqlTypeRep.isInteger || l.sqlTypeRep.isFloat) && (r.sqlTypeRep.isInteger || r.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to - must be numeric.")])  ++ l.sqlErrors ++ r.sqlErrors;
  top.baseJava =  p(l.baseJava, p(sc("-"), r.baseJava));
}

abstract production sql_multiply
top::SQL_Expr ::= l::SQL_Expr op::Mul_t r::SQL_Expr
{
  top.pp = l.pp ++ " " ++ op.lexeme ++ " " ++ r.pp ;
  top.sqlTypeRep = if l.sqlTypeRep.isFloat || r.sqlTypeRep.isFloat then sqlFloat() else sqlInteger();
  top.sqlErrors = (if (l.sqlTypeRep.isInteger || l.sqlTypeRep.isFloat) && (r.sqlTypeRep.isInteger || r.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to * must be numeric.")])  ++ l.sqlErrors ++ r.sqlErrors;
  top.baseJava =  p(l.baseJava, p(sc("*"), r.baseJava));
}

abstract production sql_divide
top::SQL_Expr ::= l::SQL_Expr op::Div_t r::SQL_Expr
{
  top.pp = l.pp ++ " " ++ op.lexeme ++ " " ++ r.pp ;
  top.sqlTypeRep = if l.sqlTypeRep.isFloat || r.sqlTypeRep.isFloat then sqlFloat() else sqlInteger();
  top.sqlErrors = (if (l.sqlTypeRep.isInteger || l.sqlTypeRep.isFloat) && (r.sqlTypeRep.isInteger || r.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to / must be numeric.")])  ++ l.sqlErrors ++ r.sqlErrors;

  top.baseJava =  p(l.baseJava, p(sc("/"), r.baseJava));
}

abstract production sql_negate
top::SQL_Expr ::= op::Minus_t e::SQL_Expr
{
  top.pp = op.lexeme ++ " " ++ e.pp ;
  top.sqlTypeRep = e.sqlTypeRep;
  top.sqlErrors = (if (e.sqlTypeRep.isInteger || e.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to negate must be numeric.")])  ++ e.sqlErrors;

  top.baseJava = p(sc("-"), e.baseJava);
}

abstract production sql_positive
top::SQL_Expr ::= op::Plus_t e::SQL_Expr
{
  top.pp = op.lexeme ++ " " ++ e.pp ;
  top.sqlTypeRep = e.sqlTypeRep;
  top.sqlErrors = (if (e.sqlTypeRep.isInteger || e.sqlTypeRep.isFloat) then [] else [mkError(op.line, "Operands to positive must be numeric.")])  ++ e.sqlErrors;

  top.baseJava = p(sc("+"), e.baseJava);
}

abstract production column_expr
top::SQL_Expr ::= c::SQL_Id_t
{
  top.pp = c.lexeme  ;
  local attribute fb :: [Decorated FieldBinding];
  fb = findField(c.lexeme, top.tableDefs);

  top.sqlErrors = if !null(fb) 
			then []
		  else if length (javaSearchResult) == 1
			then []
		  else if null (javaSearchResult)
			then [mkError(c.line, "Identifier '" ++ c.lexeme ++ "' not found")]
		  else [mkError (c.line, "Multiple matches for identifier '" ++ c.lexeme) ]; 

  -- can't forward to anything that uses the environment
  top.baseJava = if null (fb) && length (javaSearchResult) == 1
			then expr_lhs (bound_expr_id (terminal (Id_t, c.lexeme), head (javaSearchResult).dclrep, javaTypeRep))
			else sc (c.lexeme);

  local attribute javaSearchResult :: [ DclInfo ];
  javaSearchResult = lookupNameOneScope (c.lexeme, top.env);

  local attribute javaTypeRep :: TypeRep;
  javaTypeRep = if null (javaSearchResult)
		then unknownTypeRep ()
		else if length (javaSearchResult) > 1
		then unknownTypeRep ()
		else head (javaSearchResult).dclrep.typerep;

  top.sqlTypeRep = if !null(fb) then head(fb).field_type else getSQLTypeRep (javaTypeRep);

}


abstract production column_expr2
top::SQL_Expr ::= c1::SQL_Id_t d::SQL_DOT_t c2::SQL_Id_t
{
  top.pp = c1.lexeme ++ d.lexeme ++ c2.lexeme  ;
  local attribute tb :: [Decorated TableBinding];
  tb = findTable(c1.lexeme, top.tableDefs);

  local attribute fb :: [Decorated FieldBinding];
  fb = findFieldHelp(c2.lexeme, head(tb).table_type);

  top.sqlErrors = if null(tb) 
  		  then [mkError(c1.line, "Table '" ++ c1.lexeme ++ "' not found")]
		  else if null(tb) 
		       then [mkError(c2.line, "Field '" ++ c2.lexeme ++ "' not found in table '" ++ c1.lexeme ++ "'")]
		       else [];

  -- can't forward to anything that uses the environment
  top.baseJava = sc (c1.lexeme ++ "." ++ c2.lexeme);

  top.sqlTypeRep = if !null(tb) && !null(fb) then head(fb).field_type else sqlDefault();

}

abstract production literal_const
top::SQL_Expr ::= literal_arg::String_Const_t
{
  top.pp = literal_arg.lexeme ;
  top.sqlTypeRep = sqlString();
  top.sqlErrors =[];
  top.baseJava = sc("'" ++ substring(1, length(literal_arg.lexeme)-1, literal_arg.lexeme) ++ "'");
}

abstract production unsigned_int_literal
top::SQL_Expr ::= uint::Int_Const_t
{
  top.pp = uint.lexeme ;
  top.sqlTypeRep = sqlInteger();
  top.sqlErrors =[];
  top.baseJava = sc(uint.lexeme);
}

abstract production float_literal
top::SQL_Expr ::= f::Float_Const_t
{
  top.pp = f.lexeme ;
  top.sqlTypeRep = sqlFloat();
  top.sqlErrors =[];
  top.baseJava = sc(f.lexeme);
}

abstract production sub_expr
top::SQL_Expr ::=  e::SQL_Expr 
{
  top.pp = e.pp ;
  top.sqlTypeRep = e.sqlTypeRep;
  top.sqlErrors = e.sqlErrors;
  top.baseJava = p(sc(")"), p(e.baseJava, sc(")")));
}
