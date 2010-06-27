grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

import  edu:umn:cs:melt:ableJ14:terminals;

nonterminal Type with           errors, pp, basepp, typerep, type_env, pp_indent;
nonterminal Primitive_Type with errors, pp, basepp, typerep;
nonterminal Reference_Type with errors, pp, basepp, typerep, type_env, name;
nonterminal Array_Type with     errors, pp, basepp, typerep, type_env;

abstract production primitive_type
t::Type ::= t1::Primitive_Type {
  t.pp = t1.pp;
  t.basepp = t1.basepp;
  t.typerep = t1.typerep;
  t.errors := t1.errors;
}

abstract production reference_type
t::Type ::= t1::Reference_Type {
  t.pp = t1.pp;
  t.basepp = t1.basepp;
  t.errors := t1.errors;
  t.typerep = t1.typerep;
}

abstract production void_type
t::Type ::= {
  t.pp = "void";
  t.basepp = "void";
  t.errors := [];
  t.typerep = voidTypeRep ();
}

abstract production type_typerep
t::Type ::= tr::TypeRep  {
  t.pp = tr.eqName ;
  t.basepp = tr.eqName ;
  t.errors := tr.errors;
  t.typerep = tr;
}

abstract production name_type
t::Reference_Type ::= n::TypeName {
  t.pp = n.pp;

  forwards to case n.disambiguatedName of
		disambiguated_type_name (tr) -> disambiguated_name_type (t.pp, tr) |
		disambiguated_error_name (errs) -> erroneous_Reference_Type (t, errs) |
		_ -> error ("Internal compiler error in production name_type " ++ t.pp)
	      end;
}

abstract production disambiguated_name_type
t::Reference_Type ::= n_pp::String n::TypeRep {
  t.pp = n_pp;
  t.typerep = n;

  production attribute transforms_to :: [ Reference_Type ] with ++;
  transforms_to := [];

  forwards to if null (transforms_to)
		then java_ReferenceType (n_pp, n)
		else if length (transforms_to) == 1
		then head (transforms_to)
		else error ("Multiple dispatches in disambiguated_name_type"); 
}

abstract production java_ReferenceType
t::Reference_Type ::= n_pp::String n::TypeRep {
  t.pp = n_pp;
  t.basepp = n_pp;
  t.errors := [];
  t.typerep = n;
}

function getTypeName
TypeName ::= s::String {
  return simple_type_name (terminal (Id_t, s));
}

function getReferenceType
Reference_Type ::= s::String {
  return name_type (getTypeName (s));
}

function getType
Type ::= s::String {
  return reference_type (getReferenceType (s));
}

abstract production char_type
t::Primitive_Type ::= {
  t.pp = "char";
  t.basepp = "char";
  t.typerep = charTypeRep();
  t.errors := [];
}

abstract production byte_type
t::Primitive_Type ::= {
  t.pp = "byte";
  t.basepp = "byte";
  t.typerep = byteTypeRep();
  t.errors := [];
}

abstract production short_type
t::Primitive_Type ::= {
  t.pp = "short";
  t.basepp = "short";
  t.typerep = shortTypeRep();
  t.errors := [];
}

abstract production int_type
t::Primitive_Type ::= {
  t.pp = "int";
  t.basepp = "int";
  t.typerep = intTypeRep();
  t.errors := [];
}

abstract production long_type
t::Primitive_Type ::= {
  t.pp = "long";
  t.basepp = "long";
  t.typerep = longTypeRep();
  t.errors := [];
}

abstract production float_type
t::Primitive_Type ::= {
  t.pp = "float";
  t.basepp = "float";
  t.typerep = floatTypeRep();
  t.errors := [];
}

abstract production double_type
t::Primitive_Type ::= {
  t.pp = "double";
  t.basepp = "double";
  t.typerep = doubleTypeRep();
  t.errors := [];
}

abstract production boolean_type
t::Primitive_Type ::= {
  t.pp = "boolean";
  t.basepp = "boolean";
  t.typerep = booleanTypeRep();
  t.errors := [];
}


abstract production array_type
t::Reference_Type ::= t1::Array_Type {
  t.pp = t1.pp;
  t.basepp = t1.basepp;
  t.errors := t1.errors;
  t.typerep = t1.typerep;
}

abstract production primitive_array
t::Array_Type ::= t1::Primitive_Type ds::Integer {
  t.pp = t1.pp ++ printDims (ds);
  t.basepp = t1.basepp ++ printDims (ds);
  t.errors := t1.errors;
  t.typerep = arrayTypeRep (t1.typerep, ds);
}

abstract production name_array
t::Array_Type ::= n::TypeName ds::Integer {
  t.pp = n.pp ++ printDims (ds);
  t.basepp = n.basepp ++ printDims (ds);
  t.errors := case n.disambiguatedName of
			disambiguated_type_name (tr) -> [ :: Error ] |
			disambiguated_error_name (errs) -> errs |
			_ -> error ("Internal compiler error 1 in production name_array " ++ t.pp)
	      end;
  t.typerep = arrayTypeRep (case n.disambiguatedName of
				disambiguated_type_name (tr) -> tr |
				disambiguated_error_name (errs) -> errorTypeRep (errs) |
				_ -> error ("Internal compiler error 2 in production name_array " ++ t.pp)
			    end, ds);
}

-- TypeNames
--------------------------------------------------------
nonterminal TypeNames ;
attribute pp, basepp, errors, type_env occurs on TypeNames ;

abstract production type_names_none
ns::TypeNames ::= {
  ns.pp = "";
  ns.basepp = "";
  ns.errors := [];
}

abstract production type_names_one
ns::TypeNames ::= n::TypeName {
  ns.pp = n.pp;
  ns.basepp = n.basepp;
  ns.errors := case n.disambiguatedName of
                disambiguated_type_name (tr) -> [ :: Error ] |
		disambiguated_error_name (errs) -> errs |
        	_ -> error ("Internal compiler error in production type_names_one " ++ n.pp)
	      end;
}

abstract production type_names_snoc
ns::TypeNames ::= ns1::TypeNames n::TypeName {
  ns.pp = ns1.pp ++ ", " ++ n.pp;
  ns.basepp = ns1.basepp ++ ", " ++ n.basepp;
  ns.errors := ns1.errors ++  case n.disambiguatedName of
                		disambiguated_type_name (tr) -> [ :: Error ] |
				disambiguated_error_name (errs) -> errs |
        			_ -> error ("Internal compiler error in production type_names_one " ++ n.pp)
			     end;
}
