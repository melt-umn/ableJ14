grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals;
--import core;

-- Error nonterminal and associated productions

nonterminal Error with file_name, line_no, message, pp, unparse;
synthesized attribute line_no :: Integer;
synthesized attribute message :: String;

autocopy attribute file_name :: String;
attribute file_name occurs on 			  	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Package_Dcl, Import_Dcls, Import_Dcl, 
							Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs;

synthesized attribute errors :: [Error] with ++;

abstract production mkError
err::Error ::= lin::Integer mess::String {
 err.line_no = lin;
 err.message = mess;
 err.pp = err.file_name ++ ":" ++ toString (err.line_no) ++ ": " ++ err.message;
 err.unparse = "mkError (" ++ toString (lin) ++ ", \"" ++ mess ++ "\")";
}

function printErrors
String ::= errs::[Error] fn::String {
 return printErrors2 (errs, fn) ++ (if null (errs)
						then ""
					else if length (errs) == 1
						then "\n1 error\n\n"
					else "\n" ++ toString (length (errs)) ++ " errors\n\n");
}

function printErrors2
String ::= errs::[Error] fn::String {
 return if null (errs) 
	then "" 
	else (decorate (head (errs)) with {file_name = fn;}).pp ++ "\n" ++ printErrors2 (tail (errs), fn);
}

function unparseErrors
String ::= se::[ Error ] {
 return "[" ++ unparseErrorsHelper (se) ++ "]";
}

function unparseErrorsHelper
String ::= se::[ Error ] {
 return if null (se)
	then ""
	else (head (se)).unparse ++ (if null (tail (se)) then "" else ",\n\n") ++ unparseErrorsHelper (tail (se));
}

abstract production erroneous_Expr
expr::Expr ::= err_tree::Expr errs::[Error] {
 expr.pp = "/* ERRONEOUS EXPRESSION */ " ++ err_tree.pp ;
 expr.basepp = error ("From erroneous_Expr on " ++ expr.pp ++ " :\n" ++ printErrors (errs, expr.file_name)); 
-- expr.basepp = "/* ERRONEOUS EXPRESSION */ " ++ err_tree.pp ;
 expr.errors := errs ;
 expr.typerep = errorTypeRep (errs);
}

abstract production erroneous_LHS
lhs::LHS ::= err_tree::LHS errs::[Error] {
 lhs.pp = "/* ERRONEOUS LHS */ " ++ err_tree.pp ;
 lhs.basepp = error ("From erroneous_LHS on " ++ lhs.pp ++ " :\n" ++ printErrors (errs, lhs.file_name)); 
-- lhs.basepp = "/* ERRONEOUS LHS */ " ++ err_tree.pp ;
 lhs.errors := errs ;
 lhs.typerep = errorTypeRep (errs);
}

abstract production erroneous_Stmt
s::Stmt ::= err_tree::Stmt errs::[Error] {
 s.pp = "/* ERRONEOUS STATEMENT */ " ++ err_tree.pp ;
 s.basepp = error ("From erroneous_Stmt on " ++ s.pp ++ " :\n" ++ printErrors (errs, s.file_name)); 
-- s.basepp = "/* ERRONEOUS STATEMENT */ " ++ err_tree.pp ;
 s.defs = [];
 s.type_defs = [];
 s.errors := errs ;
}

abstract production erroneous_Stmt_Expr
s::Stmt_Expr ::= err_tree::Stmt_Expr errs::[Error] {
 s.pp = "/* ERRONEOUS STATEMENT EXPRESSION */ " ++ err_tree.pp ;
 s.basepp = error ("From erroneous_Stmt_Expr on " ++ s.pp ++ " :\n" ++ printErrors (errs, s.file_name));
-- s.basepp = "/* ERRONEOUS STATEMENT EXPRESSION */ " ++ err_tree.pp ;
 s.typerep = errorTypeRep (errs);
 s.errors := errs ;
}

abstract production erroneous_Reference_Type
t::Reference_Type ::= err_tree::Reference_Type errs::[ Error ] {
  t.pp = "/* ERRONEOUS REFERENCE TYPE */ " ++ err_tree.pp;
  t.basepp = error ("From erroneous_Reference_Type on " ++ t.pp ++ " :\n" ++ printErrors (errs, t.file_name)); 
--  t.basepp = "/* ERRONEOUS REFERENCE TYPE */ " ++ err_tree.pp;
  t.errors := errs;
  t.typerep = errorTypeRep (errs);
}
