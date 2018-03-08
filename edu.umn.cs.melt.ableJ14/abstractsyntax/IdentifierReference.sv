grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import  edu:umn:cs:melt:ableJ14:terminals;

synthesized attribute id_dispatches :: [ LHS ] with ++;
attribute id_dispatches occurs on LHS ;

-- Variable, Parameter References --
------------------------------------
abstract production lhs_name
lhs::LHS ::= en::ExprName {
 lhs.pp = en.pp;

 forwards to case en.disambiguatedName of
                  disambiguated_expr_name (l) -> l'' |
                  disambiguated_error_name (errs) -> erroneous_LHS (lhs, errs) |
                  _ -> error ("Internal compiler error in production lhs_name " ++ lhs.pp)
	     end ;
}

-- This production ensures that the declaration of this identifier
-- has a TypeRep that this is a standard "variable reference".

-- This is not really necessary since AmbiguousName.sv is
-- doing some type checking on this.  Also, the check if it is 
-- a field isn't needed since it can't be.
--
abstract production bound_id
e::LHS ::= id::Id_t dr::DclRep 
{
 local attribute e_typerep :: TypeRep ;
 e_typerep = if dr.is_param then dr.param_rep.typerep
		else if dr.is_local then dr.local_rep.typerep
		else if dr.is_field then dr.field_rep.typerep
		else error (" bound_id \"" ++ id.lexeme ++ "\" has DclRep that is not a parameter, local or field");

 forwards to if   true -- null(local_errors)
             then bound_expr_id(id, dr, e_typerep) 
             else erroneous_LHS (e, local_errors);

 production attribute local_errors :: [ Error ] with ++ ;
 local_errors := [ ] ;
}


-- This production uses type or declaration information
-- to translate to a type-specific production.
--
abstract production bound_expr_id
e::LHS ::= id::Id_t dr::DclRep tr::TypeRep {
 e.pp = id.lexeme ;
 -- ToDo - do we need this?     
 -- It would be helpful in RLP to not define it here.     e.typerep = tr ;

 production attribute id_dclrep :: DclRep ;
 id_dclrep = dr;

 production attribute transforms_to :: [ LHS ] with ++ ;
 transforms_to := [ ] ;

 forwards to  if   null(transforms_to)
              then id_JavaType (id, dr, tr)
              else if length (transforms_to) == 1
		then head (transforms_to)
		else error ("Multiple dispatches in bound_expr_id");

 local attribute e_msg :: String ;
 e_msg = "Identifier \"" ++ id.lexeme ++ "\" with type " ++ e.typerep.eqName ++ " cannot be used " ++ "as a variable reference." ;
}


-- Java 1.4 host language types --
----------------------------------

-- This production defines variable references for Java types.
abstract production id_JavaType
e::LHS ::= id::Id_t dr::DclRep tr::TypeRep  {
 e.pp = id.lexeme ++ " /*" ++ tr.pp ++ "*/";
 e.basepp = id.lexeme;
 e.typerep = tr ;
 e.errors := [];

 production attribute id_dclrep :: DclRep ;
 id_dclrep = dr;
}
