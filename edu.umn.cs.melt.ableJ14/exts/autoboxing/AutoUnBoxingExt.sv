grammar edu:umn:cs:melt:ableJ14:exts:autoboxing;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;

-- Converting Byte to byte
--------------------------
aspect production byte_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName 
                     interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
                     constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy <- [ mkConvertBy("java/lang/Byte->byte",convert_Byte_to_byte,byteTypeRep())  ] ; 
}

abstract production convert_Byte_to_byte 
prim::Expr ::= ref::Expr {
 forwards to expr_stmt_expr (expr_method_call(ref, terminal(Id_t,"byteValue"),exprs_none())); 
}

-- Converting Char to char
--------------------------
aspect production char_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName 
                     interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
                     constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy <- [ mkConvertBy("java/lang/Char->char",convert_Char_to_char,charTypeRep())  ] ; 
}

abstract production convert_Char_to_char 
prim::Expr ::= ref::Expr {
 forwards to expr_stmt_expr (expr_method_call(ref, terminal(Id_t,"charValue"),exprs_none())); 
}

-- Converting Short to short
----------------------------
aspect production short_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName 
                     interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
                     constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy <- [ mkConvertBy("java/lang/Short->short",convert_Short_to_short,shortTypeRep())  ] ; 
}

abstract production convert_Short_to_short 
prim::Expr ::= ref::Expr {
 forwards to expr_stmt_expr (expr_method_call(ref, terminal(Id_t,"shortValue"),exprs_none())); 
}

-- Converting Integer to int
----------------------------
aspect production integer_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName 
                     interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
                     constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy <- [ mkConvertBy("java/lang/Integer->int",convert_Integer_to_int,intTypeRep())  ] ; 
}

abstract production convert_Integer_to_int 
prim::Expr ::= ref::Expr {
 forwards to expr_stmt_expr (expr_method_call(ref, terminal(Id_t,"intValue"),exprs_none())); 
}

-- Converting Long to long
--------------------------
aspect production long_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName 
                     interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
                     constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy <- [ mkConvertBy("java/lang/Long->long",convert_Long_to_long,longTypeRep())  ] ; 
}

abstract production convert_Long_to_long 
prim::Expr ::= ref::Expr {
 forwards to expr_stmt_expr (expr_method_call(ref, terminal(Id_t,"longValue"),exprs_none())); 
}

-- Converting Float to float
----------------------------
aspect production float_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName 
                     interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
                     constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy <- [ mkConvertBy("java/lang/Float->float",convert_Float_to_float,floatTypeRep()) ] ; 
}
                           
abstract production convert_Float_to_float
prim::Expr ::= ref::Expr {
 forwards to expr_stmt_expr (expr_method_call(ref, terminal(Id_t,"floatValue"),exprs_none())); 
}

-- Converting Double to double
------------------------------
aspect production double_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName 
                     interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
                     constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy <- [ mkConvertBy("java/lang/Double->double",convert_Double_to_double,doubleTypeRep()) ] ; 
}
                           
abstract production convert_Double_to_double
prim::Expr ::= ref::Expr {
 forwards to expr_stmt_expr (expr_method_call(ref, terminal(Id_t,"doubleValue"),exprs_none())); 
}

-- Converting Boolean to boolean
--------------------------------
aspect production boolean_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName 
                     interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
                     constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy <- [ mkConvertBy("java/lang/Boolean->boolean",convert_Boolean_to_boolean,booleanTypeRep())  ] ; 
}

abstract production convert_Boolean_to_boolean 
prim::Expr ::= ref::Expr {
 forwards to expr_stmt_expr (expr_method_call(ref, terminal(Id_t,"booleanValue"),exprs_none())); 
}
