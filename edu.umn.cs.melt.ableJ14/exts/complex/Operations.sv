grammar edu:umn:cs:melt:ableJ14:exts:complex;

import edu:umn:cs:melt:ableJ14:host;

-- Overloading Assignment --
----------------------------
aspect production copy
top ::= expr::Expr requiredType::TypeRep _ {
  copy_dispatches <- if expr.typerep.eqName == "complex" && requiredType.eqName == "complex"
			then [ expr_stmt_expr (resolved_method_call_copy (expr, terminal (Id_t, "clone"), exprs_none (), complexTypeRep ())) ]
			else [];
}

-- Overloading Addition --
--------------------------
aspect production plus
e ::= e1::Expr _ e2::Expr {
 dispatches <- if elem_of_TypeRep ( complexTypeRep(), min_common_super_types ) 
               then [ complex_plus    ( convertTo(e1, e1.typerep, complexTypeRep()),
                                        convertTo(e2, e2.typerep, complexTypeRep()), 
                                        complexTypeRep() ) ] 

	       else if elem_of_TypeRep ( reference_complexTypeRep(), min_common_super_types ) 
                 then [ complex_plus    ( convertTo(e1, e1.typerep, reference_complexTypeRep()),
                                          convertTo(e2, e2.typerep, reference_complexTypeRep()), 
                                          reference_complexTypeRep() ) ] 
		 else [];
}

abstract production complex_plus
e::Expr ::= l::Expr r::Expr tr::TypeRep {
  e.pp = l.pp ++ " + " ++  r.pp ;
  e.typerep = tr;
  e.errors := l.errors ++ r.errors;
  forwards to expr_stmt_expr (expr_method_call (l, terminal (Id_t, "plus"), exprs_one (r)));
}

aspect production minus
e ::= e1::Expr _ e2::Expr {
 dispatches <- if elem_of_TypeRep ( complexTypeRep(), min_common_super_types ) 
               then [ complex_minus    ( convertTo(e1, e1.typerep, complexTypeRep()),
                                        convertTo(e2, e2.typerep, complexTypeRep()), 
                                        complexTypeRep() ) ] 

	       else if elem_of_TypeRep ( reference_complexTypeRep(), min_common_super_types ) 
                 then [ complex_minus    ( convertTo(e1, e1.typerep, reference_complexTypeRep()),
                                          convertTo(e2, e2.typerep, reference_complexTypeRep()), 
                                          reference_complexTypeRep() ) ] 
		 else [];
}

abstract production complex_minus
e::Expr ::= l::Expr r::Expr tr::TypeRep {
  e.pp = l.pp ++ " - " ++  r.pp ;
  e.typerep = tr;
  e.errors := l.errors ++ r.errors;
  forwards to expr_stmt_expr (expr_method_call (l, terminal (Id_t, "minus"), exprs_one (r)));
}

aspect production mul
e ::= e1::Expr _ e2::Expr {
 dispatches <- if elem_of_TypeRep ( complexTypeRep(), min_common_super_types ) 
               then [ complex_mul    ( convertTo(e1, e1.typerep, complexTypeRep()),
                                        convertTo(e2, e2.typerep, complexTypeRep()), 
                                        complexTypeRep() ) ] 

	       else if elem_of_TypeRep ( reference_complexTypeRep(), min_common_super_types ) 
                 then [ complex_mul    ( convertTo(e1, e1.typerep, reference_complexTypeRep()),
                                          convertTo(e2, e2.typerep, reference_complexTypeRep()), 
                                          reference_complexTypeRep() ) ] 
		 else [];
}

abstract production complex_mul
e::Expr ::= l::Expr r::Expr tr::TypeRep {
  e.pp = l.pp ++ " * " ++  r.pp ;
  e.typerep = tr;
  e.errors := l.errors ++ r.errors;
  forwards to expr_stmt_expr (expr_method_call (l, terminal (Id_t, "times"), exprs_one (r)));
}
