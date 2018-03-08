grammar edu:umn:cs:melt:ableJ14:exts:autoboxing;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;

-- Converting int to Integer
----------------------------
aspect production copy
top::Expr ::= expr::Expr requiredType::TypeRep _ {
 copy_dispatches 
     <- if requiredType.eqName == "java.lang.Byte"  &&
           equality_check (expr.typerep, byteTypeRep ())
  	then [ expr_stmt_expr (new_class (getTypeName ("Byte"), exprs_one (expr))) ]
        else [ ];

 copy_dispatches 
     <- if requiredType.eqName == "java.lang.Char"  &&
           equality_check (expr.typerep, charTypeRep ())
  	then [ expr_stmt_expr (new_class (getTypeName ("Char"), exprs_one (expr))) ]
        else [ ];

 copy_dispatches 
     <- if requiredType.eqName == "java.lang.Short"  &&
           equality_check (expr.typerep, shortTypeRep ())
  	then [ expr_stmt_expr (new_class (getTypeName ("Short"), exprs_one (expr))) ]
        else [ ];

 copy_dispatches 
     <- if requiredType.eqName == "java.lang.Integer"  &&
           equality_check (expr.typerep, intTypeRep ())
  	then [ expr_stmt_expr (new_class (getTypeName ("Integer"), exprs_one (expr))) ]
        else [ ];

 copy_dispatches 
     <- if requiredType.eqName == "java.lang.Long"  &&
           equality_check (expr.typerep, longTypeRep ())
  	then [ expr_stmt_expr (new_class (getTypeName ("Long"), exprs_one (expr))) ]
        else [ ];

 copy_dispatches 
     <- if requiredType.eqName == "java.lang.Float"  &&
           equality_check (expr.typerep, floatTypeRep ())
  	then [ expr_stmt_expr (new_class (getTypeName ("Float"), exprs_one (expr))) ]
        else [ ];

 copy_dispatches 
     <- if requiredType.eqName == "java.lang.Double"  &&
           equality_check (expr.typerep, doubleTypeRep ())
  	then [ expr_stmt_expr (new_class (getTypeName ("Double"), exprs_one (expr))) ]
        else [ ];

 copy_dispatches 
     <- if requiredType.eqName == "java.lang.Boolean"  &&
           equality_check (expr.typerep, booleanTypeRep ())
  	then [ expr_stmt_expr (new_class (getTypeName ("Boolean"), exprs_one (expr))) ]
        else [ ];
}
