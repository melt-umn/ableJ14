grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals;

-- Productions and mechanisms for operator overloading for
--  "-"  subtraction 
--  "*"  multiplication
--  "/"  division
--  "%"  mod

--------------------------------------------------------------------------
----- SUBTRACT
--------------------------------------------------------------------------

abstract production minus
e::Expr ::= e1::Expr t::Minus_t e2::Expr 
{
 e.pp = "(" ++ e1.pp ++ " - " ++ e2.pp ++ ")";

 -- compute minimal common super type of e1 and e2
 production attribute min_common_super_types :: [ TypeRep ] ;
 min_common_super_types = minimal_common_super_types ( e1.typerep, e2.typerep );

 -- Various types will examine single_min_common_super_type to determine if they
 -- should "claim" this instance of minus.   If they do, they will add the tree
 -- to which this production should forward into the dispatches list.
 -- See some examples below for how built in Java types do this.
 production attribute dispatches :: [ Expr ] with ++ ;
 dispatches := [ ] ; 

 -- if there is just 1 tree that this minus should transform/forward to, the do so,
 -- otherwise, forward 
 forwards to if   length (dispatches) == 1
             then head (dispatches)
             else   
             if   ! null(e1.errors ++ e2.errors) 
             then erroneous_Expr (e, e1.errors ++ e2.errors)
             else
             if   length(dispatches) > 1
             then erroneous_Expr (e, [ mkError (t.line,  "Internal Compiler Error:\n" ++ 
                     " Types " ++ e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ 
                     " compatible on subtraction (-) in multiple ways.") ] )
             else erroneous_Expr (e, [ mkError (t.line,  "Incompatible types (" ++ 
                     e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ ") on subtraction.") ] ) ; 
}


aspect production minus
e::Expr ::= e1::Expr t::Minus_t e2::Expr 
{  dispatches <-      -- int - int --> int
                 if   elem_of_TypeRep ( intTypeRep(), min_common_super_types )
                 then [ typed_java_minus ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- byte - byte --> int
                 if   elem_of_TypeRep ( byteTypeRep(), min_common_super_types )
                 then [ typed_java_minus ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- char - char --> int
                 if   elem_of_TypeRep ( charTypeRep(), min_common_super_types )
                 then [ typed_java_minus ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- short - short --> int
                 if   elem_of_TypeRep ( shortTypeRep(), min_common_super_types )
                 then [ typed_java_minus ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- long - long --> long
                 if   elem_of_TypeRep ( longTypeRep(), min_common_super_types )
                 then [ typed_java_minus ( convertTo(e1, e1.typerep, longTypeRep()),
                                          convertTo(e2, e2.typerep, longTypeRep()), 
                                          longTypeRep() ) ] 
                 else
                      -- float - float --> float
                 if   elem_of_TypeRep ( floatTypeRep(), min_common_super_types )
                 then [ typed_java_minus ( convertTo(e1, e1.typerep, floatTypeRep()),
                                          convertTo(e2, e2.typerep, floatTypeRep()), 
                                          floatTypeRep() ) ] 
                 else
                      -- double - double --> double
                 if   elem_of_TypeRep ( doubleTypeRep(), min_common_super_types )
                 then [ typed_java_minus ( convertTo(e1, e1.typerep, doubleTypeRep()),
                                          convertTo(e2, e2.typerep, doubleTypeRep()), 
                                          doubleTypeRep() ) ] 
                 else [ ] ;
}

abstract production typed_java_minus
e::Expr ::= e1::Expr e2::Expr t::TypeRep 
{
 e.pp = "(" ++ e1.pp ++ " - " ++ e2.pp ++ ")";
 e.basepp = "(" ++ e1.basepp ++ " - " ++ e2.basepp ++ ")";
 e.errors := e1.errors ++ e2.errors;
 e.typerep = t;

 e.neededImportedSingleTypes   = e1.neededImportedSingleTypes   ++ e2.neededImportedSingleTypes ;
 e.neededCurrentPackageTypes   = e1.neededCurrentPackageTypes   ++ e2.neededCurrentPackageTypes ;
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes ;
 e.neededFullyQualifiedTypes   = e1.neededFullyQualifiedTypes   ++ e2.neededFullyQualifiedTypes ; 
}

--------------------------------------------------------------------------
----- MUL
--------------------------------------------------------------------------

abstract production mul
e::Expr ::= e1::Expr t::Mul_t e2::Expr 
{
 e.pp = "(" ++ e1.pp ++ " * " ++ e2.pp ++ ")";

 -- compute minimal common super type of e1 and e2
 production attribute min_common_super_types :: [ TypeRep ] ;
 min_common_super_types = minimal_common_super_types ( e1.typerep, e2.typerep );

 -- Various types will examine single_min_common_super_type to determine if they
 -- should "claim" this instance of multiply.   If they do, they will add the tree
 -- to which this production should forward into the dispatches list.
 -- See some examples below for how built in Java types do this.
 production attribute dispatches :: [ Expr ] with ++ ;
 dispatches := [ ] ; 

 -- if there is just 1 tree that this multiply should transform/forward to, the do so,
 -- otherwise, forward 
 forwards to if   length (dispatches) == 1
             then head (dispatches)
             else   
             if   ! null(e1.errors ++ e2.errors) 
             then erroneous_Expr (e, e1.errors ++ e2.errors)
             else
             if   length(dispatches) > 1
             then erroneous_Expr (e, [ mkError (t.line,  "Internal Compiler Error:\n" ++ 
                     " Types " ++ e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ 
                     " compatible on multiplication (*) in multiple ways.") ] )
             else erroneous_Expr (e, [ mkError (t.line,  "Incompatible types (" ++ 
                     e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ ") on multiplication.") ] ) ; 
}


aspect production mul
e::Expr ::= e1::Expr t::Mul_t e2::Expr 
{  dispatches <-      -- int * int --> int
                 if   elem_of_TypeRep ( intTypeRep(), min_common_super_types )
                 then [ typed_java_mult ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- byte * byte --> int
                 if   elem_of_TypeRep ( byteTypeRep(), min_common_super_types )
                 then [ typed_java_mult ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- char * char --> int
                 if   elem_of_TypeRep ( charTypeRep(), min_common_super_types )
                 then [ typed_java_mult ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- short * short --> int
                 if   elem_of_TypeRep ( shortTypeRep(), min_common_super_types )
                 then [ typed_java_mult ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- long * long --> long
                 if   elem_of_TypeRep ( longTypeRep(), min_common_super_types )
                 then [ typed_java_mult ( convertTo(e1, e1.typerep, longTypeRep()),
                                          convertTo(e2, e2.typerep, longTypeRep()), 
                                          longTypeRep() ) ] 
                 else
                      -- float * float --> float
                 if   elem_of_TypeRep ( floatTypeRep(), min_common_super_types )
                 then [ typed_java_mult ( convertTo(e1, e1.typerep, floatTypeRep()),
                                          convertTo(e2, e2.typerep, floatTypeRep()), 
                                          floatTypeRep() ) ] 
                 else
                      -- double * double --> double
                 if   elem_of_TypeRep ( doubleTypeRep(), min_common_super_types )
                 then [ typed_java_mult ( convertTo(e1, e1.typerep, doubleTypeRep()),
                                          convertTo(e2, e2.typerep, doubleTypeRep()), 
                                          doubleTypeRep() ) ] 
                 else [ ] ;
}

abstract production typed_java_mult
e::Expr ::= e1::Expr e2::Expr t::TypeRep 
{
 e.pp = "(" ++ e1.pp ++ " * " ++ e2.pp ++ ")";
 e.basepp = "(" ++ e1.basepp ++ " * " ++ e2.basepp ++ ")";
 e.errors := e1.errors ++ e2.errors;
 e.typerep = t;

 e.neededImportedSingleTypes   = e1.neededImportedSingleTypes   ++ e2.neededImportedSingleTypes ;
 e.neededCurrentPackageTypes   = e1.neededCurrentPackageTypes   ++ e2.neededCurrentPackageTypes ;
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes ;
 e.neededFullyQualifiedTypes   = e1.neededFullyQualifiedTypes   ++ e2.neededFullyQualifiedTypes ; 
}


--------------------------------------------------------------------------
----- DIVISION
--------------------------------------------------------------------------
abstract production div
e::Expr ::= e1::Expr t::Div_t e2::Expr 
{
 e.pp = "(" ++ e1.pp ++ " / " ++ e2.pp ++ ")";

 -- compute minimal common super type of e1 and e2
 production attribute min_common_super_types :: [ TypeRep ] ;
 min_common_super_types = minimal_common_super_types ( e1.typerep, e2.typerep );

 -- Various types will examine single_min_common_super_type to determine if they
 -- should "claim" this instance of division.   If they do, they will add the tree
 -- to which this production should forward into the dispatches list.
 -- See some examples below for how built in Java types do this.
 production attribute dispatches :: [ Expr ] with ++ ;
 dispatches := [ ] ; 

 -- if there is just 1 tree that this division should transform/forward to, the do so,
 -- otherwise, forward 
 forwards to if   length (dispatches) == 1
             then head (dispatches)
             else   
             if   ! null(e1.errors ++ e2.errors) 
             then erroneous_Expr (e, e1.errors ++ e2.errors)
             else
             if   length(dispatches) > 1
             then erroneous_Expr (e, [ mkError (t.line,  "Internal Compiler Error:\n" ++ 
                     " Types " ++ e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ 
                     " compatible on division (/) in multiple ways.") ] )
             else erroneous_Expr (e, [ mkError (t.line,  "Incompatible types (" ++ 
                     e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ ") on division.") ] ) ; 
}

aspect production div
e::Expr ::= e1::Expr t::Div_t e2::Expr 
{  dispatches <-      -- int / int --> int
                 if   elem_of_TypeRep ( intTypeRep(), min_common_super_types )
                 then [ typed_java_div ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- byte / byte --> int
                 if   elem_of_TypeRep ( byteTypeRep(), min_common_super_types )
                 then [ typed_java_div ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- char / char --> int
                 if   elem_of_TypeRep ( charTypeRep(), min_common_super_types )
                 then [ typed_java_div ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- short / short --> int
                 if   elem_of_TypeRep ( shortTypeRep(), min_common_super_types )
                 then [ typed_java_div ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- long / long --> long
                 if   elem_of_TypeRep ( longTypeRep(), min_common_super_types )
                 then [ typed_java_div ( convertTo(e1, e1.typerep, longTypeRep()),
                                          convertTo(e2, e2.typerep, longTypeRep()), 
                                          longTypeRep() ) ] 
                 else
                      -- float / float --> float
                 if   elem_of_TypeRep ( floatTypeRep(), min_common_super_types )
                 then [ typed_java_div ( convertTo(e1, e1.typerep, floatTypeRep()),
                                          convertTo(e2, e2.typerep, floatTypeRep()), 
                                          floatTypeRep() ) ] 
                 else
                      -- double / double --> double
                 if   elem_of_TypeRep ( doubleTypeRep(), min_common_super_types )
                 then [ typed_java_div ( convertTo(e1, e1.typerep, doubleTypeRep()),
                                          convertTo(e2, e2.typerep, doubleTypeRep()), 
                                          doubleTypeRep() ) ] 
                 else [ ] ;
}

abstract production typed_java_div
e::Expr ::= e1::Expr e2::Expr t::TypeRep 
{
 e.pp = "(" ++ e1.pp ++ " / " ++ e2.pp ++ ")";
 e.basepp = "(" ++ e1.basepp ++ " / " ++ e2.basepp ++ ")";
 e.errors := e1.errors ++ e2.errors;
 e.typerep = t;

 e.neededImportedSingleTypes   = e1.neededImportedSingleTypes   ++ e2.neededImportedSingleTypes ;
 e.neededCurrentPackageTypes   = e1.neededCurrentPackageTypes   ++ e2.neededCurrentPackageTypes ;
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes ;
 e.neededFullyQualifiedTypes   = e1.neededFullyQualifiedTypes   ++ e2.neededFullyQualifiedTypes ; 
}

--------------------------------------------------------------------------
----- MOD
--------------------------------------------------------------------------

abstract production mod
e::Expr ::= e1::Expr t::Mod_t e2::Expr {
  e.pp = "(" ++ e1.pp ++ " % " ++ e2.pp ++ ")";

 -- compute minimal common super type of e1 and e2
 production attribute min_common_super_types :: [ TypeRep ] ;
 min_common_super_types = minimal_common_super_types ( e1.typerep, e2.typerep );

 -- Various types will examine single_min_common_super_type to determine if they
 -- should "claim" this instance of mod.   If they do, they will add the tree
 -- to which this production should forward into the dispatches list.
 -- See some examples below for how built in Java types do this.
 production attribute dispatches :: [ Expr ] with ++ ;
 dispatches := [ ] ; 

 -- if there is just 1 tree that this mod should transform/forward to, the do so,
 -- otherwise, forward 
 forwards to if   length (dispatches) == 1
             then head (dispatches)
             else   
             if   ! null(e1.errors ++ e2.errors) 
             then erroneous_Expr (e, e1.errors ++ e2.errors)
             else
             if   length(dispatches) > 1
             then erroneous_Expr (e, [ mkError (t.line,  "Internal Compiler Error:\n" ++ 
                     " Types " ++ e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ 
                     " compatible on mod (%) in multiple ways.") ] )
             else erroneous_Expr (e, [ mkError (t.line,  "Incompatible types (" ++ 
                     e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ ") on mod (%).") ] ) ; 
}

aspect production mod
e::Expr ::= e1::Expr t::Mod_t e2::Expr 
{  dispatches <-      -- int % int --> int
                 if   elem_of_TypeRep ( intTypeRep(), min_common_super_types )
                 then [ typed_java_mod ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- byte % byte --> int
                 if   elem_of_TypeRep ( byteTypeRep(), min_common_super_types )
                 then [ typed_java_mod ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- char % char --> int
                 if   elem_of_TypeRep ( charTypeRep(), min_common_super_types )
                 then [ typed_java_mod ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- short % short --> int
                 if   elem_of_TypeRep ( shortTypeRep(), min_common_super_types )
                 then [ typed_java_mod ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- long % long --> long
                 if   elem_of_TypeRep ( longTypeRep(), min_common_super_types )
                 then [ typed_java_mod ( convertTo(e1, e1.typerep, longTypeRep()),
                                          convertTo(e2, e2.typerep, longTypeRep()), 
                                          longTypeRep() ) ] 
                 else
                      -- float % float --> float
                 if   elem_of_TypeRep ( floatTypeRep(), min_common_super_types )
                 then [ typed_java_mod ( convertTo(e1, e1.typerep, floatTypeRep()),
                                          convertTo(e2, e2.typerep, floatTypeRep()), 
                                          floatTypeRep() ) ] 
                 else
                      -- double % double --> double
                 if   elem_of_TypeRep ( doubleTypeRep(), min_common_super_types )
                 then [ typed_java_mod ( convertTo(e1, e1.typerep, doubleTypeRep()),
                                          convertTo(e2, e2.typerep, doubleTypeRep()), 
                                          doubleTypeRep() ) ] 
                 else [ ] ;
}

abstract production typed_java_mod
e::Expr ::= e1::Expr e2::Expr t::TypeRep 
{
 e.pp = "(" ++ e1.pp ++ " % " ++ e2.pp ++ ")";
 e.basepp = "(" ++ e1.basepp ++ " % " ++ e2.basepp ++ ")";
 e.errors := e1.errors ++ e2.errors;
 e.typerep = t;

 e.neededImportedSingleTypes   = e1.neededImportedSingleTypes   ++ e2.neededImportedSingleTypes ;
 e.neededCurrentPackageTypes   = e1.neededCurrentPackageTypes   ++ e2.neededCurrentPackageTypes ;
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes ;
 e.neededFullyQualifiedTypes   = e1.neededFullyQualifiedTypes   ++ e2.neededFullyQualifiedTypes ; 
}
