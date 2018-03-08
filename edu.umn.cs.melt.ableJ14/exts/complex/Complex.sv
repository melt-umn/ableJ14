grammar edu:umn:cs:melt:ableJ14:exts:complex;

import edu:umn:cs:melt:ableJ14:host ;

-- "Primitive type" version

concrete production complex_literal_c
top::PrimaryExpression ::= 'complex' '(' r::Expression ',' i::Expression ')' {
  top.ast_Expr = complex_literal (r.ast_Expr, i.ast_Expr);
}

abstract production complex_literal
top::Expr ::= r::Expr i::Expr {
  top.pp = "complex (" ++ r.pp ++ ", " ++ i.pp ++ ")";
  top.typerep = complexTypeRep ();

  forwards to expr_stmt_expr (new_class (qualified_type_name (simple_package_or_type_name (terminal (Id_t, "complexpackage")), terminal (Id_t, "ComplexClass")),
					 exprs_cons (r, exprs_one (i))));
}

-- "Reference type" version

concrete production reference_complex_literal_c
top::PrimaryExpression ::= 'Complex' '(' r::Expression ',' i::Expression ')' {
  top.ast_Expr = reference_complex_literal (r.ast_Expr, i.ast_Expr);
}

abstract production reference_complex_literal
top::Expr ::= r::Expr i::Expr {
  top.pp = "Complex (" ++ r.pp ++ ", " ++ i.pp ++ ")";
  top.errors := r.errors ++ i.errors ; -- ++ check if they're both double
  top.typerep = reference_complexTypeRep ();

  forwards to expr_stmt_expr (new_class (qualified_type_name (simple_package_or_type_name (terminal (Id_t, "complexpackage")), terminal (Id_t, "ComplexClass")),
					 exprs_cons (r, exprs_one (i))));
}

