grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals;

synthesized attribute plus_dispatches :: [ Expr ] with ++ ;
attribute plus_dispatches occurs on Expr ;

abstract production plus
e::Expr ::= e1::Expr t::Plus_t e2::Expr 
{
 e.pp = "(" ++ e1.pp ++ " + " ++ e2.pp ++ ")";

 -- if   e1 or e2 is a java.lang.String, 
 -- then forward to string concatentation
 -- else forward to the tree which overloads + 
 --      this will be stored in the 'dispatches' attribute

 -- compute minimal common super type of e1 and e2
 production attribute min_common_super_types :: [ TypeRep ] ;
 min_common_super_types = minimal_common_super_types ( e1.typerep, e2.typerep );

 -- Various types will examine single_min_common_super_type to determine if they
 -- should "claim" this instance of plus.   If they do, they will add the tree
 -- to which this production should forward into the dispatches list.
 -- See some examples below for how built in Java types do this.
 production attribute dispatches :: [ Expr ] with ++ ;
 dispatches := [ ] ; -- e.plus_dispatches ;

 -- if there is just 1 tree that this plus should transform/forward to, the do so,
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
                     " compatible on addition (+) in multiple ways.") ] )
             else erroneous_Expr (e, [ mkError (t.line,  "Incompatible types (" ++ 
                     e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ ") on addition.") ] ) ; 

}



--------------------------------------------------------------------------
----- PLUS
--------------------------------------------------------------------------
aspect production plus
e::Expr ::= e1::Expr t::Plus_t e2::Expr 
{  dispatches <-      -- int + int --> int
                 if   elem_of_TypeRep ( intTypeRep(), min_common_super_types )
                 then [ typed_java_plus ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- byte + byte --> int
                 if   elem_of_TypeRep ( byteTypeRep(), min_common_super_types )
                 then [ typed_java_plus ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- char + char --> int
                 if   elem_of_TypeRep ( charTypeRep(), min_common_super_types )
                 then [ typed_java_plus ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- short + short --> int
                 if   elem_of_TypeRep ( shortTypeRep(), min_common_super_types )
                 then [ typed_java_plus ( convertTo(e1, e1.typerep, intTypeRep()),
                                          convertTo(e2, e2.typerep, intTypeRep()), 
                                          intTypeRep() ) ] 
                 else
                      -- long + long --> long
                 if   elem_of_TypeRep ( longTypeRep(), min_common_super_types )
                 then [ typed_java_plus ( convertTo(e1, e1.typerep, longTypeRep()),
                                          convertTo(e2, e2.typerep, longTypeRep()), 
                                          longTypeRep() ) ] 
                 else
                      -- float + float --> float
                 if   elem_of_TypeRep ( floatTypeRep(), min_common_super_types )
                 then [ typed_java_plus ( convertTo(e1, e1.typerep, floatTypeRep()),
                                          convertTo(e2, e2.typerep, floatTypeRep()), 
                                          floatTypeRep() ) ] 
                 else
                      -- double + double --> double
                 if   elem_of_TypeRep ( doubleTypeRep(), min_common_super_types )
                 then [ typed_java_plus ( convertTo(e1, e1.typerep, doubleTypeRep()),
                                          convertTo(e2, e2.typerep, doubleTypeRep()), 
                                          doubleTypeRep() ) ] 
                 else [ ] ;
}

abstract production typed_java_plus
e::Expr ::= e1::Expr e2::Expr t::TypeRep 
{
 e.pp = "(" ++ e1.pp ++ " + " ++ e2.pp ++ ")";
 e.basepp = "(" ++ e1.basepp ++ " + " ++ e2.basepp ++ ")";
 e.errors := e1.errors ++ e2.errors;
 e.typerep = t;

 e.neededImportedSingleTypes   = e1.neededImportedSingleTypes   ++ e2.neededImportedSingleTypes ;
 e.neededCurrentPackageTypes   = e1.neededCurrentPackageTypes   ++ e2.neededCurrentPackageTypes ;
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes ;
 e.neededFullyQualifiedTypes   = e1.neededFullyQualifiedTypes   ++ e2.neededFullyQualifiedTypes ; 
}



-- String concatentation 
--------------------------------------------------
aspect production plus
e::Expr ::= e1::Expr t::Plus_t e2::Expr 
{
 dispatches <- if   equality_check (e1.typerep, string_typerep) ||
                    equality_check (e2.typerep, string_typerep) 
               then [ string_concat ( e1, t, e2 ) ] 
               else [ ] ;

 local attribute string_typerep :: TypeRep ;
 string_typerep = retrieveTypeRep ("java.lang.String", e.type_env ) ;
}

abstract production string_concat
e::Expr ::= e1::Expr t::Plus_t e2::Expr 
{
 -- ToDo - check that the non-string expression can be converted into a String.
 e.pp = "(" ++ e1.pp ++ " + " ++ e2.pp ++ ")";
 e.typerep = retrieveTypeRep ("java.lang.String", e.type_env) ;

 production attribute dispatches :: [Expr] with ++ ;
 dispatches := [ ];

 forwards to if use_java_string_concat
             then java_string_concat(e1, t, e2) 
             else 
             if   length (dispatches) == 1
             then head (dispatches)
             else   
             if   ! null(e1.errors ++ e2.errors) 
             then erroneous_Expr (e, e1.errors ++ e2.errors)
             else
             if   length(dispatches) > 1
             then erroneous_Expr (e, [ mkError (t.line,  "Internal Compiler Error:\n" ++ 
                     " Types " ++ e1.typerep.eqName ++ " and " ++ e2.typerep.eqName ++ 
                     " compatible on string concatenation (+) in multiple ways.") ] )
             else erroneous_Expr (e, [ mkError (t.line,  "Incompatible types (" ++ 
                     e1.typerep.pp ++ " and " ++ e2.typerep.pp ++ ") on string concatenation.") ] ) ; 



 local attribute use_java_string_concat :: Boolean ;
 use_java_string_concat = ( equality_check (e1.typerep, string_typerep)
                            &&
                            works_with_string_concat(e2.typerep, object_typerep) )
                        ||
                          ( equality_check (e2.typerep, string_typerep)
                            &&
                            works_with_string_concat(e1.typerep, object_typerep) ) ;
                          
 local attribute string_typerep :: TypeRep ;
 string_typerep = retrieveTypeRep ("java.lang.String", e.type_env ) ;

 local attribute object_typerep :: TypeRep ;
 object_typerep = retrieveTypeRep ("java.lang.Object", e.type_env ) ;

 e.neededImportedSingleTypes   = e1.neededImportedSingleTypes   ++ e2.neededImportedSingleTypes ;
 e.neededCurrentPackageTypes   = e1.neededCurrentPackageTypes   ++ e2.neededCurrentPackageTypes ;
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes ;
 e.neededFullyQualifiedTypes   = e1.neededFullyQualifiedTypes   ++ e2.neededFullyQualifiedTypes ; 
}

function works_with_string_concat
Boolean ::= t::TypeRep  object_typerep::TypeRep
{ return equality_check ( t, booleanTypeRep() )
      || equality_check ( t, charTypeRep() )
      || equality_check ( t, doubleTypeRep() )
      || equality_check ( t, floatTypeRep() )
      || equality_check ( t, intTypeRep() )
      || equality_check ( t, shortTypeRep() )
      || equality_check ( t, byteTypeRep() )
      || subtype_check  ( t, object_typerep ).isSubType ;
      -- ToDo: need to check char arrays as well.
}

abstract production java_string_concat
e::Expr ::= e1::Expr t::Plus_t e2::Expr 
{
 e.pp = "(" ++ e1.pp ++ " + " ++ e2.pp ++ ")";
 e.basepp = "(" ++ e1.basepp ++ " + " ++ e2.basepp ++ ")";
 e.errors := e1.errors ++ e2.errors;
 e.typerep = retrieveTypeRep ("java.lang.String", e.type_env) ;

 e.neededImportedSingleTypes   = e1.neededImportedSingleTypes   ++ e2.neededImportedSingleTypes ;
 e.neededCurrentPackageTypes   = e1.neededCurrentPackageTypes   ++ e2.neededCurrentPackageTypes ;
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes ;
 e.neededFullyQualifiedTypes   = e1.neededFullyQualifiedTypes   ++ e2.neededFullyQualifiedTypes ; 
}




-- Things to make obsolte below ...............................................................


abstract production int_plus
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " + " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " + " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = intTypeRep();
}

abstract production float_plus
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " + " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " + " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = floatTypeRep();
}

abstract production double_plus
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " + " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " + " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = doubleTypeRep();
}

abstract production generic_plus_OBSOLETE
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " + " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " + " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = e1.typerep;
}
