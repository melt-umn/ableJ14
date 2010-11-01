grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;
import  edu:umn:cs:melt:ableJ14:terminals;

abstract production subtype_on_typeref_test
td::Type_Dcl ::=  t1::Type  t2::Type {
 td.pp = t1.pp ++ " ==> " ++ t2.pp  ++ ";\n" ;
 td.basepp = "";
 td.type_defs = [];
 td.errors := [];
}

abstract production subtype_on_typeref_expr_test
td::Type_Dcl ::= t1::Type e1::Id_t t2::Type e2::Id_t {
 td.pp = t1.pp ++ " " ++ e1.lexeme ++ " ==> " ++ t2.pp ++ " " ++ e2.lexeme  ++ ";\n" 
         ++
         (if ! stres.isSubType 
          then "Type " ++ t1.pp ++ " is not a subtype of " ++ t2.pp ++ "\n"
          else "Type " ++ t1.pp ++ " is a subtype of " ++ t2.pp ++ "\n" ++
               "Msg: " ++ stres.msg ++ "\n"
         )
         ++
         "Errors found: " ++ printErrors (stres.errors, td.file_name)
         ; 

 local attribute tr1 :: TypeRep ;
 tr1 = t1.typerep ;

 local attribute tr2 :: TypeRep ;
 tr2 = t2.typerep ;


 -- check if t1 <: t2
 local attribute stres :: SubTypeRes ;
 stres =  subtype_check (tr1, tr2) ;



 td.errors := t1.errors ++ t2.errors;
 td.basepp = td.pp;
 td.type_defs = [];

 td.neededImportedSingleTypes = t1.neededImportedSingleTypes ++ t2.neededImportedSingleTypes;
 td.neededCurrentPackageTypes = t1.neededCurrentPackageTypes ++ t2.neededCurrentPackageTypes;
 td.neededImportedOnDemandTypes = t1.neededImportedOnDemandTypes ++ t2.neededImportedOnDemandTypes;
 td.neededFullyQualifiedTypes = t1.neededFullyQualifiedTypes ++ t2.neededFullyQualifiedTypes;
 td.localTypes = [];
}


