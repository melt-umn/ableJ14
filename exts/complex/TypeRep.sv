grammar edu:umn:cs:melt:ableJ14:exts:complex;

import edu:umn:cs:melt:ableJ14:host;

-- "Primitive type" version

abstract production complexTypeRep
t::TypeRep ::= {
 t.pp = "complexTypeRep ()" ;
 t.errors := [];
 t.eqName = "complex" ;
 t.unparse = "complexTypeRep ()";
 t.superTypes_ConvertBy := [ mkToUnknownCnvt() ] ;

 forwards to primitiveTypeRep (t);
}

-- "Reference type" version

abstract production reference_complexTypeRep
t::TypeRep ::= {
 t.pp = "reference_complexTypeRep ()" ;
 t.errors := [];
 t.eqName = "Complex" ;
 t.unparse = "reference_complexTypeRep ()";
 t.superTypes_ConvertBy := [ mkToUnknownCnvt() ] ;

 forwards to referenceTypeRep (t);
}

-- defs -> env conversion

aspect production convert_type_rep
top::ConvertedTypeRep ::= old::TypeRep environment::[ ScopeEnv ] {

 convertedTypeRep <-
	case new (old) of
	  reference_complexTypeRep ()
	->
	  [ reference_complexTypeRep () ]

	|

	  complexTypeRep ()
	->
	  [ complexTypeRep () ]

	|
	  _
	->
	  [ :: TypeRep ]
	end;
}

-- specify that double is a subtype of complex

aspect production doubleTypeRep
t::TypeRep ::= {
 t.superTypes_ConvertBy <- [ mkConvertBy ("double->complex", convert_double_to_complex, complexTypeRep()) ] ; 
 t.superTypes_ConvertBy <- [ mkConvertBy ("double->Complex", convert_double_to_complex_class, reference_complexTypeRep()) ] ; 
}

abstract production convert_double_to_complex 
c::Expr ::= d::Expr {
 forwards to complex_literal (d, double_const ("0.0"));
}

abstract production convert_double_to_complex_class 
c::Expr ::= d::Expr {
 forwards to reference_complex_literal (d, double_const ("0.0"));
}
