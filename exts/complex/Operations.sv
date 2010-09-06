grammar edu:umn:cs:melt:ableJ14:exts:complex;

import edu:umn:cs:melt:ableJ14:host;

-- Overloading Assignment --
----------------------------
aspect production copy
top ::= expr requiredType _ {
  copy_dispatches <- if expr.typerep.eqName == "complex" && requiredType.eqName == "complex"
			then [ expr_stmt_expr (resolved_method_call_copy (expr, terminal (Id_t, "clone"), exprs_none (), complexTypeRep ())) ]
			else [];
}

-- Overloading Addition --
--------------------------
aspect production plus
e ::= e1 op e2 {
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
