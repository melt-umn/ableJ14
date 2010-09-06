grammar edu:umn:cs:melt:ableJ14:exts:complex;

import edu:umn:cs:melt:ableJ14:host;

-- "Primitive type" version

terminal Complex_t 'complex' dominates { Id_t } ;

concrete production complex_type_c
t::referenceType ::= c::Complex_t {
 t.ast_Reference_Type = complex_type (c);
}

abstract production complex_type
t::Reference_Type ::= c::Complex_t  {
 t.pp = c.lexeme ;
 t.typerep = complexTypeRep ();
 t.resolvedTypeRep = complexTypeRep ();

 forwards to name_type (qualified_type_name (simple_package_or_type_name (terminal (Id_t, "complexpackage")), terminal (Id_t, "ComplexClass")));
}

-- "Reference type" version

terminal ComplexRef_t  'Complex' dominates { Id_t } ;

concrete production reference_complex_type_c
t::referenceType ::= c::ComplexRef_t {
 t.ast_Reference_Type = reference_complex_type (c);
}

abstract production reference_complex_type
t::Reference_Type ::= c::ComplexRef_t  {
 t.pp = c.lexeme ;
 t.typerep = reference_complexTypeRep ();
 t.resolvedTypeRep = reference_complexTypeRep ();

 forwards to name_type (qualified_type_name (simple_package_or_type_name (terminal (Id_t, "complexpackage")), terminal (Id_t, "ComplexClass")));
}

