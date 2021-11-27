grammar edu:umn:cs:melt:ableJ14:exts:rlp;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:concretesyntax;


-- Consider the following example use of randomized linear perturbation:
--   float x_in, y_in, z_in ;
--   rlp<float> x = x_in ;
--   rlp<float> y = y_in ;
--   rlp<float> z = z_in ;
--   int r ;
--   r = sign (z - x * y ) ;

-- The types of x, y, and z are RLP (rlpTypeRep(float)) and are
-- parameterized by the type used to represent the value.

-- The first step in processing is to translate RLP variables to
-- their symbolic representations:
--    x  -->   x  +_s   e_x  *_s  e
-- Here, x of type RLP is replaced by a expression of type Symbolic in
-- which x is added to its "perturbation value".  It is a random
-- value, e_x, associated with the variable x multiplied by a symbolic
-- infinitely small value e.  This perturbation value is symbolic and
-- cannot be computed.   The operators +_s and *_s are addition and 
-- multiplication of symbolic expressions.  These are implemented by the
-- production addSym and substractSym.

-- The original RLP expression under sign computes the symbolic version
-- of the expression.  For z - x * y, this is
--  (z  +_s   e_z  *_s  e)  -_s  (x  +_s   e_x  *_s  e)  *_s   (y  +_s   e_y  *_s  e)

-- On the symbolic version, we gather like-coefficients to create the 
-- expression: 
--        e^0 *_s (z - x * y)                      
--   +_s  e^1 *_s (e_z - (x * e_y + y * e_x))
--   +_s  e^2 *_s (0 - ( (x * 0) + (e_x * e_y) + (0 * y) )

-- We can then test each coefficient to see if one is not zero.  The sign of the
-- the first non-zero coefficient is the sign of the expression.

-- If all are zero, which is very unlikely, then the data degeneracy
-- has not been removed. 

-- Thus, the above example translate, via forwarding, to:
--   float x_in, y_in, z_in ;
--   float x = x_in ;
--   float e_x = random();
--   float y = y_in ;
--   float e_y = random();
--   float z = z_in ;
--   float e_z = random();
--   int r ;
--   r = sign_h ( (z - (x * y)) );
--   if ((r == 0))
--     {
--      r = sign_h ( (e_z - ((x * e_y) + (e_x * y))) );
--      if ((r == 0))
--        {
--         r = sign_h ( (0.0 - ((x * 0.0) + ((e_x * e_y) + (0.0 * y)))) );
--         if ((r == 0))
--            r = 0;
--         else
--            throw PertrubationError ;    -- ToDo: generate this
--        }
--     }
--

-- Declarations of RLP-typed variables          -- 
--------------------------------------------------
terminal RLP_t 'rlp' dominates {Id_t};

concrete production rlp_local_var_dcl_c
s::BlockStatement ::= 'rlp' '<' pt::Type_NT '>' dcl::VariableDeclarator ';' {
 s.ast_Stmt = rlp_local_var_dcl (pt.ast_Type, dcl.ast_Var_Declarator);
}

abstract production rlp_local_var_dcl
s::Stmt ::= pt::Type  dcl::Var_Declarator
{
 s.pp = "rlp<" ++ pt.pp ++ "> " ++ dcl.pp ++ ";" ;
 s.defs = [ envItem (dcl.name, getSimpleFQN ("NoFQN"), localDcl (dcl.name, rlpTypeRep(pt.typerep) )) ];
 s.errors := [];
 -- ToDo - collect proper errors above

 s.neededFullyQualifiedTypes = pt.neededFullyQualifiedTypes ++ dcl.neededFullyQualifiedTypes ++ [ getQualifiedFQN (getQualifiedFQN ( getSimpleFQN ("java"), "lang"), "Math") ];

 forwards to    
  -- "pt dcl ;"
  -- "pt e_x ;"
  stmt_seq (
   stmt_dcl (local_var_dcl(pt, var_declarators_one (dcl) ) ) ,
   stmt_dcl (local_var_dcl(pt, var_declarators_one ( var_declarator_init (
                   var_declarator_id (terminal(Id_t,"e_" ++ dcl.name)),
                   var_init_expr ( -- AST for "java.lang.Math.random()"
                                   ast_java_lang_Math_random_method_call )
                  ) ) ) )
            ) ;

 -- AST for "java.lang.Math.random()"
 local attribute ast_java_lang_Math_random_method_call::Expr ;
 ast_java_lang_Math_random_method_call 
  = expr_stmt_expr (
      method_call ( qualified_method_name ( 
                      qualified_ambiguous_name ( 
                        qualified_ambiguous_name ( 
                          simple_ambiguous_name ( terminal(Id_t,"java") ) ,
                          terminal(Id_t,"lang") 
                        ) ,
                        terminal(Id_t,"Math") 
                      ) , 
                      terminal(Id_t,"random") 
                    ) 
                  , exprs_none () 
                  ) ) ;

-- previous: expr_stmt_expr (method_call ( simple_method_name ( terminal(Id_t,"rnd")), exprs_none() ) )
}


-- Assignment of 'sign' expression              --
--------------------------------------------------
terminal Sign_t 'sign' dominates {Id_t} ;
concrete production sign_c
s::BlockStatement ::= lhs::Expression '=' s_kwd::Sign_t '(' expr::Expression ')' ';' {
  s.ast_Stmt = case lhs.ast_Expr of
		expr_lhs (l) -> signRLP ( l, expr.ast_Expr ) |
		_ -> error ("Syntax error " ++ toString(s_kwd.line) ++  ": sign assignment to non-LHS expression.")
	       end;
}


abstract production signRLP
s::Stmt ::= lhs::LHS e::Expr
{
 s.pp = lhs.pp ++ " = sign_rpl ( " ++ e.pp ++ " );\n" ;
 s.defs = [];

 -- compute the symbolic version of the expression e in the 
 -- attribute symTree and forward to the symbolic sign production
 -- with this tree as the expression.

 s.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes ++ 
		[ getQualifiedFQN (getQualifiedFQN (getQualifiedFQN ( getQualifiedFQN (getQualifiedFQN ( getQualifiedFQN (getQualifiedFQN ( 
			getSimpleFQN ("edu"), "umn"), "cs"), "melt"), "ableJ14"), "exts"), "rlp"), "Sign") ];
 forwards to if   null(lhs.errors ++ e.errors)
             then signSym (lhs,e.symTree)  
             else erroneous_Stmt ( s, lhs.errors ++ e.errors ) ;
}


synthesized attribute symTree :: Expr ;
attribute symTree occurs on Expr,LHS ;

-- Multiply Overloading --
aspect production mulOp
e::Expr ::= e1::Expr t::Mul_t e2::Expr 
{
 dispatches <-
    if   e1.typerep.isPeturbedType && e2.typerep.isPeturbedType
    then [ multiplyRLP(e1,e2) ]
    else [ ] ;

 -- if e1 and e2 are RLP types the overload multiply (mul)
 -- for mulitplication of RLP types - using multiplyRLP
 -- production.
}

abstract production multiplyRLP
e::Expr ::= l::Expr r::Expr
{
 e.pp = "(" ++ l.pp ++ " *p " ++ r.pp ++ ")" ;
 e.typerep = l.typerep ;
 e.errors := l.errors ++ r.errors ;
 e.symTree = multiplySym(l.symTree,r.symTree) ;
}

-- Addition Overloading --
aspect production plus
e::Expr ::= e1::Expr t::Plus_t e2::Expr {
 dispatches <-
    if   e1.typerep.isPeturbedType && e2.typerep.isPeturbedType
    then [ addRLP(e1,e2) ]
    else [ ] ;
}

abstract production addRLP
e::Expr ::= l::Expr r::Expr
{
 e.pp = "(" ++ l.pp ++ " +p " ++ r.pp ++ ")" ;
 e.typerep = l.typerep ;
 e.errors := l.errors ++ r.errors ;
 e.symTree = addSym(l.symTree,r.symTree) ;
}

-- Subtraction Overloading --
aspect production minus
e::Expr ::= e1::Expr t::Minus_t e2::Expr {
 dispatches <- 
    if   e1.typerep.isPeturbedType && e2.typerep.isPeturbedType
    then [ subtractRLP(e1,e2) ]
    else [ ] ;
}

abstract production subtractRLP
e::Expr ::= l::Expr r::Expr
{
 e.pp = "(" ++ l.pp ++ " -p " ++ r.pp ++ ")" ;
 e.typerep = l.typerep ;
 e.errors := l.errors ++ r.errors ;
 e.symTree = subtractSym(l.symTree,r.symTree) ;
}


aspect production expr_lhs
expr::Expr ::= lhs::LHS {
  expr.symTree = lhs.symTree;
}


-- Variable references
-- The idRefTrans productions builds the translation 
-- 'id +_s e_id *_s e' for identifier id.

aspect production  bound_id
e::LHS ::= id::Id_t dr::DclRep
{
 e.symTree =
    if   ! dr.is_local 
    then erroneous_Expr (
           expr_lhs(e), 
           [ mkError (id.line, "Identifier " ++ id.lexeme ++
                              " must be a local variable.") ] )
    else 
    if   dr.local_rep.typerep.isPeturbedType
    then idRefTrans(id, dr)
    else erroneous_Expr (
           expr_lhs(e), 
           [ mkError (id.line, "Identifier " ++ id.lexeme ++ 
                               " must be of type RLP to be used in sign.") ] ) ;
}

aspect production erroneous_Expr
expr::Expr ::= err_tree::Expr errs::[Error] 
{
 expr.symTree = erroneous_Expr(err_tree,errs);
}
