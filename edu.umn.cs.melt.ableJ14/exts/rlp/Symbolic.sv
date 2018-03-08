grammar edu:umn:cs:melt:ableJ14:exts:rlp;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:concretesyntax;

-- The number of the highest coefficient of e in 
-- the symbolic expression represented by coefs
synthesized attribute ncoefs :: Integer ;
attribute ncoefs occurs on Expr; 

-- Represents the symbolic expression
synthesized attribute coefs :: [ Expr ] ;
attribute coefs occurs on Expr ; 


abstract production signHost
s::Stmt ::= lhs::LHS e::Expr
{
 s.pp = lhs.pp ++ " = sign_h ( " ++ e.pp ++ " );\n" ;
 s.errors := lhs.errors ++ e.errors ;

-- s.defs = [];
-- s.type_defs = [];

 -- forwards to  "lhs = edu.umn.cs.melt.ableJ14.exts.cg.Sign.signMethod( e ) ; "
 forwards to stmt_stmt_expr(sign_assign) ;

 local attribute sign_assign :: Stmt_Expr ;
 sign_assign
  = assign ( lhs , 
             terminal (Eq_t,"=") ,
             expr_stmt_expr( 
               method_call ( qualified_method_name ( edu__Sign_name,  terminal(Id_t,"signMethod") ) ,
                             exprs_one( e ) ) ) ) ;

 local attribute edu__Sign_name :: AmbiguousName ;
 edu__Sign_name = mk_ambig_name( ["edu","umn","cs","melt","ableJ14","exts","rlp","Sign"] ) ;

 s.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes ++ 
		[ getQualifiedFQN (getQualifiedFQN (getQualifiedFQN ( getQualifiedFQN (getQualifiedFQN ( getQualifiedFQN (getQualifiedFQN ( 
			getSimpleFQN ("edu"), "umn"), "cs"), "melt"), "ableJ14"), "exts"), "rlp"), "Sign") ];
}



abstract production signSym
s::Stmt ::= lhs::LHS e::Expr
{
 s.pp = lhs.pp ++ " = sign_s ( " ++ e.pp ++ " );\n" ;
 s.errors := [];
 s.defs = [];

 local attribute rest :: Stmt ;
 rest = mkRest (tail(e.coefs), e.ncoefs, lhs,  int_const("0") ) ; 

 forwards to stmt_block ( block ( stmt_seq ( signHost(lhs, head(e.coefs)), rest ) ) ) ;
 s.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes ++ 
		[ getQualifiedFQN (getQualifiedFQN (getQualifiedFQN ( getQualifiedFQN (getQualifiedFQN ( getQualifiedFQN (getQualifiedFQN ( 
			getSimpleFQN ("edu"), "umn"), "cs"), "melt"), "ableJ14"), "exts"), "rlp"), "Sign") ];
}

function mkRest
Stmt ::= es::[Expr] sz::Integer lhs::LHS zero::Expr 
{
 return if   sz==0
        then if_then(terminal (If_t, "if"),  
                     equalInt(expr_lhs(lhs),zero) , 
                     stmt_stmt_expr(assign(lhs, terminal (Eq_t, "="), int_const("0"))) )
        else if_then(terminal (If_t, "if"),   equalInt(expr_lhs(lhs),zero) ,
                      blk (stmt_seq ( signHost(lhs,head(es)) ,
                                      mkRest ( tail(es), sz-1, lhs, zero ) ) ) )  ;
}


-- ToDo: use proper Java equals in above stuff, not our version below
abstract production equalInt
e::Expr ::= l::Expr r::Expr
{
 e.pp = "(" ++ l.pp ++ " == " ++ r.pp ++ ")" ;
 e.typerep = booleanTypeRep() ;
 e.basepp = "(" ++ l.basepp ++ " == " ++ r.basepp ++ ")" ;
 e.errors := [ ] ;
}


abstract production addSym
e::Expr ::= l::Expr r::Expr
{
 e.pp = "(" ++ l.pp ++ " +s " ++ r.pp ++ ")" ;
 e.typerep = l.typerep ;
 e.errors := [] ;
 e.ncoefs = maxI (l.ncoefs, r.ncoefs) ;

 e.coefs = addThing ( plus, -- float_plus, --
                      l.coefs, r.coefs ) ;
}




abstract production subtractSym
e::Expr ::= l::Expr r::Expr
{
 e.pp = "(" ++ l.pp ++ " -s " ++ r.pp ++ ")" ;
 e.typerep = l.typerep ;
 e.errors := [];
 e.ncoefs = maxI (l.ncoefs, r.ncoefs) ;
 e.coefs = subThing ( -- l.type.compType.subtractProd,
                      minus, --- ToDo --------- was -      float_minus,
                      l.coefs, r.coefs ) ;
}

function addThing
[Expr] ::= p::(Expr ::= Expr Plus_t Expr) ls::[Expr] rs::[Expr]
{
 return [ p ( head(ls), terminal(Plus_t,"+"), head(rs) ) ] ++ addThing ( p, tail(ls), tail(rs) ) ;
 -- we rely on laziness here, knowing that we only take the ones we need from
 -- the created list.
}

function subThing
[Expr] ::= p::(Expr ::= Expr Minus_t Expr) ls::[Expr] rs::[Expr]
{
 return [ p ( head(ls), terminal(Minus_t,"-"), head(rs) ) ] ++ subThing ( p, tail(ls), tail(rs) ) ;
 -- we rely on laziness here, knowing that we only take the ones we need from
 -- the created list.
}


abstract production multiplySym
e::Expr ::= l::Expr r::Expr
{
 e.pp = "(" ++ l.pp ++ " *s " ++ r.pp ++ ")" ;
 e.typerep = l.typerep ;
 e.errors := [] ;
 e.ncoefs = l.ncoefs + r.ncoefs ;
 e.coefs = mulThing ( plus, mul, -- float_plus, float_mul,
                      l.coefs, r.coefs ) ;
-- l.type.compType.addProd, l.type.compType.multiplyProd, 
-- we really should be able to just use the dispatcher..
}

function mulThing
[Expr] ::= addp::(Expr ::= Expr Plus_t Expr) mulp::(Expr ::= Expr Mul_t Expr) ls::[Expr] rs::[Expr] 
{
 return addLists(addp,ess) ;

 local attribute ess :: [[Expr]] ;
 ess = do_cs(mulp, ls, rs) ;

}

function addLists 
[Expr] ::= addp::(Expr ::= Expr Plus_t Expr)  ess::[[Expr]] 
{
 return --if leng(ess)==0 then [::Expr] else 
        head(head(ess)) :: add2(addp, tail(head(ess)), addLists(addp,tail(ess))) ;
}

function add2
[Expr] ::= addp::(Expr ::= Expr Plus_t Expr) l1::[Expr] l2::[Expr]
{
 return --if leng(l1)==0 then l2 else
        addp(head(l1), terminal(Plus_t,"+"), head(l2)) :: add2(addp,tail(l1),tail(l2)) ;
}

function do_cs
[[Expr]] ::=  mp::(Expr ::= Expr Mul_t Expr) cs::[Expr] ds::[Expr]
{
 return --if leng(cs)==0 then [[::Expr]] else 
        do_c_to_ds(mp,head(cs),ds) :: do_cs(mp,tail(cs),ds) ;
}

function do_c_to_ds
[Expr] ::= mp::(Expr ::= Expr Mul_t Expr) ce::Expr ds::[Expr]
{
 return --if leng(ds)==0 then [::Expr] else
        (mp(ce,  terminal(Mul_t,"+"), head(ds))) :: do_c_to_ds (mp,ce,tail(ds)) ;
}



abstract production idRefTrans
e::Expr ::= i::Id_t ct::DclRep
{
 e.pp = i.lexeme ;
 e.typerep = symTypeRep (ct.local_rep.typerep.componentTypeRep);
 e.ncoefs = 1 ;
 e.coefs = [ expr_lhs(bound_id(i, 
                               localDcl(i.lexeme,ct.local_rep.typerep.componentTypeRep))) ,
             expr_lhs(bound_id(terminal(Id_t,"e_"++i.lexeme),
                               localDcl("e_"++i.lexeme,ct.local_rep.typerep.componentTypeRep)))
           ] 
           ++ zeros
          ;

 local attribute zeros :: [Expr] ;
 zeros = double_const("0.0") 
         :: zeros ;
}



function expI Integer ::= x::Integer n::Integer
{
 return  if n == 0 then 1 else x * expI (x, n-1) ;
}

function maxI Integer ::= a::Integer b::Integer
{
 return  if a > b then a else b ;
}



abstract production blk
b::Stmt::= s::Stmt {
 forwards to stmt_block ( block ( s) ) ; }

