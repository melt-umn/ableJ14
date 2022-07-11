grammar edu:umn:cs:melt:ableJ14:exts:tables ;

import edu:umn:cs:melt:ableJ14:terminals ;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:concretesyntax hiding parse ;

import edu:umn:cs:melt:ableJ14:host:exts:parameterized_expr_block hiding parse;


-- Forward
--      c4 = table (
--          c1 : T F ,
--          c2 : F * ,
--          c3 : T T );
-- to
-- 
--  ({ (boolean t1 = c1, boolean t2 = c2, boolean t3 = c3)
--     boolean result ;
--     result = ((c1 && ((! c2) && c3)) || ((! c1) && (true && c3)));
--     return result ;   )}


-- Abstract Syntax --

abstract production table
t::Expr ::= trows::TableRows 
{
 t.pp = "table (\n" ++ trows.pp ++ " )" ;
 t.typerep = booleanTypeRep ();
 t.errors := trows.errors ;

 forwards to exprBlock ( trows.trow_dcls, body, primitive_type (boolean_type ()), ret_expr ) ;

 --  ({ (boolean t1 = c1, boolean t2 = c2, boolean t3 = c3)
 --     boolean result ;
 --     result = ((c1 && ((! c2) && c3)) || ((! c1) && (true && c3)));
 --     return result ;   )}
 -- Note the ({ ... }) syntax indicates a statement block embedded in an expression. It
 -- translates to the definition and use of an anonymous class.

 local attribute ret_expr :: Expr ;
 ret_expr = getExpr ("result") ;

 local attribute result_expr :: Expr ;
 result_expr = if !null(trows.errors)
               then true_const()
               else disjunction(mapConjunction(transpose ( trows.ft_exprss ))) ;

 local attribute body :: Stmt ;
 body = stmt_seq ( stmt_dcl (local_var_dcl( primitive_type(boolean_type()) , 
                            var_declarators_one (var_declarator(var_declarator_id(terminal(Id_t,"result")))))) ,
                   stmt_seq (
                      stmt_stmt_expr (assign ( getLHS ("result"), terminal (Eq_t, "="), result_expr ) ) ,
                      return_expr ( terminal (Return_t, "return"), getExpr ("result"))
                            )
                 ) ; 

 trows.pp_indent = 10 ;
 t.neededImportedSingleTypes = trows.neededImportedSingleTypes;
 t.neededCurrentPackageTypes = trows.neededCurrentPackageTypes;
 t.neededImportedOnDemandTypes = trows.neededImportedOnDemandTypes;
 t.neededFullyQualifiedTypes = trows.neededFullyQualifiedTypes;
}

-- Table Rows --

nonterminal TableRows with pp, pp_indent, ft_exprss, rlen, errors, env, trow_dcls, enclosingType, type_env, line_no,
				neededImportedSingleTypes, neededCurrentPackageTypes, neededImportedOnDemandTypes, neededFullyQualifiedTypes; --is_static
synthesized attribute ft_exprss :: [[Expr]] ;
synthesized attribute rlen :: Integer ;
synthesized attribute trow_dcls :: ExprBlockParams ;

abstract production tableRowSnoc
trows::TableRows ::=  trowstail::TableRows trow::TableRow
{
 trows.pp = trowstail.pp ++ "\n" ++ trow.pp ;
 trows.rlen = trow.rlen ;
 trows.errors := trowstail.errors ++ trow.errors ++         
  if trow.rlen == trowstail.rlen then []
  else [mkError (trow.line_no, "The number of T,F,* entries in table row \n    \"" ++ trow.pp ++ 
                               "\"\n    must be the same as the preceding rows")] ;
 trows.line_no = trowstail.line_no ;

 trows.ft_exprss = trowstail.ft_exprss ++ [trow.ft_exprs] ;
 trows.trow_dcls = exprBlockParam_snoc (trowstail.trow_dcls, trow.trow_dcl) ;

 trows.neededImportedSingleTypes = trowstail.neededImportedSingleTypes ++ trow.neededImportedSingleTypes;
 trows.neededCurrentPackageTypes = trowstail.neededCurrentPackageTypes ++ trow.neededCurrentPackageTypes;
 trows.neededImportedOnDemandTypes = trowstail.neededImportedOnDemandTypes ++ trow.neededImportedOnDemandTypes;
 trows.neededFullyQualifiedTypes = trowstail.neededFullyQualifiedTypes ++ trow.neededFullyQualifiedTypes;
}

abstract production tableRowOne
trows::TableRows ::= trow::TableRow
{
 trows.pp = trow.pp ;
 trows.errors := trow.errors ;
 trows.rlen = trow.rlen ; 
 trows.ft_exprss = [ trow.ft_exprs ] ;
 trows.line_no = trow.line_no ;
 trows.trow_dcls = exprBlockParam_one (trow.trow_dcl) ;

 trows.neededImportedSingleTypes = trow.neededImportedSingleTypes;
 trows.neededCurrentPackageTypes = trow.neededCurrentPackageTypes;
 trows.neededImportedOnDemandTypes = trow.neededImportedOnDemandTypes;
 trows.neededFullyQualifiedTypes = trow.neededFullyQualifiedTypes;
}

-- Table Row
nonterminal TableRow with pp, pp_indent, ft_exprs, errors, rlen, env, trow_dcl, enclosingType, type_env, line_no,
			neededImportedSingleTypes, neededCurrentPackageTypes, neededImportedOnDemandTypes, neededFullyQualifiedTypes; -- is_static
synthesized attribute ft_exprs :: [ Expr ] ;
synthesized attribute trow_dcl :: ExprBlockParam ;

abstract production tableRow
trow::TableRow ::= e::Expr tvl::TruthValueList_c
{
 trow.pp = space(trow.pp_indent) ++ e.pp ++ " : " ++ tvl.pp  ;
 -- raise error if e is not compatible with boolean -- LK
 trow.errors := e.errors ;
 trow.rlen = tvl.rlen ;
 trow.line_no = tvl.line_no ;

 trow.ft_exprs = tvl.ft_exprs ;
 tvl.row_temp_expr = idExpr (temp_name);

 trow.trow_dcl = exprBlockParam ( primitive_type(boolean_type()), 
                                  var_declarator_id(terminal(Id_t,temp_name)) ,
                                  var_init_expr(e) ) ;                                  

 local attribute temp_name :: String ;
 temp_name = "temp" ++  toString(genInt()) ;

 trow.neededImportedSingleTypes = e.neededImportedSingleTypes;
 trow.neededCurrentPackageTypes = e.neededCurrentPackageTypes;
 trow.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes;
 trow.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes;
}

abstract production idExpr
e::Expr ::= n::String 
{ forwards to getExpr (n); }

-- Truth Value List
-------------------

attribute row_temp_expr, pp, ft_exprs, rlen, line_no occurs on TruthValueList_c;

inherited attribute row_temp_expr :: Expr ;
-- the temporary identifier holding the value of the expression
--  in the table row.  It is passed down to the TF* value to begin 
-- construction of the base-Java expression.

aspect production tvlistCons
tvl::TruthValueList_c ::=  tv::TruthValue_c  tvltail::TruthValueList_c
{
 tvl.pp = tv.pp ++ " " ++ tvltail.pp ;
 tvl.rlen = 1 + tvltail.rlen ;
 tvl.ft_exprs = cons (tv.ft_expr, tvltail.ft_exprs ) ;

 tv.row_temp_expr = tvl.row_temp_expr ;
 tvltail.row_temp_expr = tvl.row_temp_expr ;
 tvl.line_no = tv.line_no ;
}

aspect production tvlistOne
tvl::TruthValueList_c ::=  tv::TruthValue_c
{
 tvl.pp = tv.pp ;
 tvl.rlen = 1 ;
 tvl.ft_exprs = [ tv.ft_expr ] ;
 tv.row_temp_expr = tvl.row_temp_expr ;
 tvl.line_no = tv.line_no ;
}
 --forwards to tvlistCons(tv, tvlistNil() );

-- Truth Values
---------------

attribute row_temp_expr, ft_expr, pp, line_no occurs on TruthValue_c ;
synthesized attribute ft_expr :: Expr ;

aspect production tvTrue
tv::TruthValue_c ::= truetv::TrueTV_t
{
 tv.pp = "T" ;
 tv.ft_expr = tv.row_temp_expr ;
 tv.line_no = truetv.line ;
}

aspect production tvFalse
tv::TruthValue_c ::= falsetv::FalseTV_t
{
 tv.pp = "F" ;
 tv.ft_expr = notOp (tv.row_temp_expr) ;
 tv.line_no = falsetv.line ;
}

aspect production tvStar
tv::TruthValue_c ::= startv::Mul_t
{ 
 tv.pp = "*" ;
 tv.ft_expr = true_const () ;
 tv.line_no = startv.line ;
}

-- table helper functions
-------------------------

-- foldl1 (or_or, [[Expr]])  :: Expr
function disjunction Expr ::= es::[Expr]
{ return if length(es) == 1  then head(es)
         else or_or ( head(es), disjunction ( tail(es) ) ) ;
}


function mapConjunction [Expr] ::= ess::[[Expr]]
{ return if null(ess)  then [ ] 
         else cons ( conjunction ( head(ess) ),
                     mapConjunction ( tail(ess) ) ) ;
}

-- foldl1 (and_p, [[Expr]])  :: Expr
function conjunction Expr ::= es::[Expr]
{ return if length(es) == 1    then head(es)
         else and_and ( head(es), conjunction ( tail(es) ) ) ;
}

-- map ( disjunction, [[Expr]] ) :: [Expr]  
function mapOr [Expr] ::= ess::[[Expr]]    -- not used anywhere
{ return if null(ess)   then [ ]
         else cons ( disjunction ( head(ess) ), 
                     mapOr ( tail(ess) ) ) ;
}



function transpose
[[Expr]] ::= matrix::[[Expr]]
{
 return if length(matrix) == 1 -- matrix.length_Expr_List == 1
        then mapWrap_Expr ( row )
        else mapCons_Expr ( row , 
               transpose ( tail(matrix)) -- matrix.tail_Expr_List )
              ) ;

 local attribute row :: [Expr] ;
 row = head(matrix) ; -- matrix.head_Expr_List ;
}



function mapCons_Expr
[[Expr]] ::= row::[Expr] matrix::[[Expr]]
{
 return if null(row)
         then [ ]
         else cons (
                 cons ( head(row) , head(matrix) ) ,
                 mapCons_Expr ( tail(row) , tail(matrix) ) ) ;
}


function mapWrap_Expr
[[Expr]] ::= l::[Expr]
{
 return  if null(l)
         then [ ] 
         else cons (
                 cons ( head(l), [ ] ),
                 mapWrap_Expr ( tail(l) ) ) ;
}

