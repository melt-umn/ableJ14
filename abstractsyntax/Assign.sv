grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

import  edu:umn:cs:melt:ableJ14:terminals;

-- So now we're trying to separate out the copy dispatches
-- from the assign and method call productions.
-- 
-- assign (l, r) => assign_copy (l, copy (r))
-- 
-- The copy production checks to see if a particular
-- expression can be copied into a context that requires a particular
-- type. It does pretty much what the assign productions do now.
-- 
-- It first checks to see if there are any "preempting" conversions
-- (say int -> Integer, or the complex cloning production) that pre-empt
-- the (default) subtype checks. The pre-empting special conversions
-- are stored in a production collection attribute.
-- 
-- If there aren't any special conversions, a subtype check is performed.
-- If this works, the corresponding subtype -> supertype conversion
-- is performed.

abstract production assign
a::Stmt_Expr ::= lhs::LHS t::Eq_t expr::Expr {
  a.pp = lhs.pp ++ " = " ++ expr.pp;

  forwards to assign_copy (lhs, t, copy (expr, lhs.typerep, t.line));
}

abstract production assign_copy
a::Stmt_Expr ::= lhs::LHS t::Eq_t expr::Expr {
  a.pp = lhs.pp ++ " = " ++ expr.pp;
  a.basepp = lhs.basepp ++ " = " ++ expr.basepp;
  a.errors := lhs.errors ++ expr.errors;
  a.typerep = lhs.typerep ;
}

synthesized attribute copyWorked :: Boolean;
attribute copyWorked occurs on Expr;

abstract production copy
top::Expr ::= expr::Expr requiredType::TypeRep lineNo::Integer {
  top.pp = expr.pp;
  -- top.errors := my_errors ++ expr.errors;

  production attribute copy_dispatches :: [ Expr ] with ++;
  copy_dispatches := [];

  local attribute f :: Expr;
  f = if length (copy_dispatches) == 1
      		then head (copy_dispatches)
      else if length (copy_dispatches) > 1
      		then erroneous_Expr (top, all_errors)
      else if res.isSubType 
           	then convert.convertedExpr
           	else erroneous_Expr (top, all_errors);
  forwards to f'';

  top.copyWorked = length (copy_dispatches) == 1 || res.isSubType;

  production attribute my_errors :: [Error] ; -- ToDo: at "with ++" here?
  my_errors = if length (copy_dispatches) == 1
               then [ ]
               else if length (copy_dispatches) > 1 
               then mk_dispatch_internal_errors_Expr (requiredType, expr.typerep, copy_dispatches, lineNo)
               else if requiredType.isUnknownType || expr.typerep.isUnknownType 
               then [ ]
               else if ! res.isSubType 
               then mk_dispatch_incompatible_errors_Expr (requiredType, expr.typerep, copy_dispatches, lineNo)
               else [ ] ;

  local attribute all_errors :: [Error] ;
  all_errors =  my_errors ++ expr.errors;

  -- for checking subtype and the conversion of expr to lhs is expr is subtype of lhs
  production attribute res :: SubTypeRes;
  res = subtype_check (expr.typerep, requiredType);

  production attribute convert :: ConvertBy;
  convert = res.convertBy;
  convert.toConvertExpr = expr;
}

---------------------------------------------------------
-- Dispatch Error Functions for Expr nonterminals --
---------------------------------------------------------

function mk_dispatch_incompatible_errors_Expr
[Error] ::= t1::TypeRep t2::TypeRep dispatches::[Expr] loc::Integer { 
 return
  [ mkError (loc,  "Error: Types " ++ t1.eqName ++ " and " ++ t2.eqName ++ " not compatible"
            ) ] ;
}

function mk_dispatch_internal_errors_Expr
[Error] ::= t1::TypeRep t2::TypeRep dispatches::[Expr] loc::Integer { 
 return
  [ mkError (loc,  "Internal Compiler Error:\n" ++ 
                   " Types " ++ t1.eqName ++ " and " ++ t2.eqName ++ 
                   " compatible in multiple ways" ++
                   "\n First production of first: " ++ head(dispatches).patProdName ++ "\n"  ++
                   "\n Second production of second: " ++ head(tail(dispatches)).patProdName ++ "\n" 
            ) ] ;
}
