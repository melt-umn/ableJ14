grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals;
--import core;

-- FullyQualifiedName
-----------------------------------

nonterminal FullyQualifiedName with qualifiedFileName, qualifiedName, name, line_no, pathList, unparse;
synthesized attribute fullyQualifiedName :: FullyQualifiedName;
synthesized attribute fullyQualifiedNames::[ FullyQualifiedName  ];

attribute fullyQualifiedName occurs on Package_Dcl, PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName;
attribute fullyQualifiedNames occurs on TypeNames;

synthesized attribute qualifiedFileName :: String;
synthesized attribute pathList :: [ String ];

abstract production fully_qualified_name_none
fqn::FullyQualifiedName ::= {
    fqn.qualifiedFileName = "";
    fqn.qualifiedName = "";
    fqn.name = "";
    fqn.line_no = -1;
    fqn.pathList = [];
    fqn.unparse = "fully_qualified_name_none ()";
}

abstract production fully_qualified_name_simple
fqn::FullyQualifiedName ::= n::Id_t {
    fqn.qualifiedFileName = n.lexeme;
    fqn.qualifiedName = n.lexeme;
    fqn.name = n.lexeme;
    fqn.line_no = n.line;
    fqn.pathList = [ n.lexeme ];
    fqn.unparse = "fully_qualified_name_simple (terminal (Id_t, \"" ++ n.lexeme ++  "\"))";
}

abstract production fully_qualified_name_qualified
fqn::FullyQualifiedName ::= fqn1::FullyQualifiedName n::Id_t {
    fqn.qualifiedFileName = case fqn1 of
				fully_qualified_name_none () -> n.lexeme |
				_ -> fqn1.qualifiedFileName ++ "/" ++ n.lexeme
			    end;
    fqn.qualifiedName = case fqn1 of
				fully_qualified_name_none () -> n.lexeme |
				_ -> fqn1.qualifiedName ++ "." ++ n.lexeme
			end;
    fqn.name = n.lexeme;
    fqn.line_no = fqn1.line_no;
    fqn.pathList = fqn1.pathList ++ [ n.lexeme ];
    fqn.unparse = "fully_qualified_name_qualified (" ++ fqn1.unparse ++ ", terminal (Id_t, \"" ++ n.lexeme ++  "\"))";
}

abstract production fully_qualified_name_unknown
fqn::FullyQualifiedName ::= {
    fqn.qualifiedFileName = "";
    fqn.qualifiedName = "";
    fqn.name = "";
    fqn.line_no = -1;
    fqn.pathList = [];
    fqn.unparse = "fully_qualified_name_unknown ()";
}

function getSimpleFQN
FullyQualifiedName ::= s::String {
    return fully_qualified_name_simple (terminal (Id_t, s));
}

function getQualifiedFQN
FullyQualifiedName ::= fqn::FullyQualifiedName s::String {
    return fully_qualified_name_qualified (fqn, terminal (Id_t, s));
}

-- Fully qualified name with location

nonterminal LFQN with fullyQualifiedName, location, unparse;
synthesized attribute location :: String;

abstract production lfqn
top::LFQN ::= location_::String fullyQualifiedName_::FullyQualifiedName {
 top.location = location_;
 top.fullyQualifiedName = fullyQualifiedName_;
 top.unparse = "lfqn (\"" ++ location_ ++ "\", " ++ fullyQualifiedName_.unparse ++ ")";
}
-----------------------------------------------------------------------------

-- FullyQualifiedName Utility Functions
---------------------------------------

function printFullyQualifiedNames
String ::= fqns::[ FullyQualifiedName ] {
 return if null (fqns)
	then ""
	else ((head (fqns)).qualifiedName ++ (if null (tail (fqns)) then "" else (", " ++ printFullyQualifiedNames (tail (fqns)))));
}

function unparseFullyQualifiedNames
String ::= fqns::[ FullyQualifiedName ] {
 return "[" ++ unparseFullyQualifiedNamesHelper (fqns) ++ "]";
}

function unparseFullyQualifiedNamesHelper
String ::= fqns::[ FullyQualifiedName ] {
 return if null (fqns)
	then ""
	else ((head (fqns)).unparse ++ (if null (tail (fqns)) then "" else (", " ++ unparseFullyQualifiedNamesHelper (tail (fqns)))));
}

function unparseLFQNs
String ::= lfqns::[ LFQN ] {
 return "[" ++ unparseLFQNsHelper (lfqns) ++ "]";
}

function unparseLFQNsHelper
String ::= lfqns::[ LFQN ] {
 return if null (lfqns)
	then ""
	else ((head (lfqns)).unparse ++ (if null (tail (lfqns)) then "" else (", " ++ unparseLFQNsHelper (tail (lfqns)))));
}

function equalFullyQualifiedName
Boolean ::= fqn1::FullyQualifiedName fqn2::FullyQualifiedName {
 return (
  case fqn1 of  
     fully_qualified_name_none () ->
	(case fqn2 of
 		fully_qualified_name_none () -> true |
		_ -> false
	 end) |

     fully_qualified_name_simple (id1) -> 
	(case fqn2 of
		fully_qualified_name_simple (id2) -> id1.lexeme == id2.lexeme |
		_ -> false
	 end) |

     fully_qualified_name_qualified (qn1, id1) ->
	(case fqn2 of
		fully_qualified_name_qualified (qn2, id2) -> id1.lexeme == id2.lexeme && equalFullyQualifiedName (    (qn1),     (qn2)) |
		_ -> false
         end)
  end);
}

function equalLFQN
Boolean ::= lfqn1::LFQN lfqn2::LFQN {
 return lfqn1.location == lfqn2.location && equalFullyQualifiedName (lfqn1.fullyQualifiedName, lfqn2.fullyQualifiedName);
}

function memberFullyQualifiedName
Boolean ::= elt::FullyQualifiedName ls::[ FullyQualifiedName ] {
 return if null (ls) 
	then false 
	else (equalFullyQualifiedName (elt, head (ls)) || memberFullyQualifiedName (elt, tail (ls)));
}

function memberLFQN
Boolean ::= elt::LFQN ls::[ LFQN ] {
 return if null (ls) 
	then false 
	else (equalLFQN (elt, head (ls)) || memberLFQN (elt, tail (ls)));
}

function uniqueFullyQualifiedNames
[ FullyQualifiedName ] ::= fqns::[ FullyQualifiedName ] {
 return if null (fqns)
	then []
	else if memberFullyQualifiedName (head (fqns), tail (fqns))
		then uniqueFullyQualifiedNames (tail (fqns))
		else head (fqns) :: uniqueFullyQualifiedNames (tail (fqns));
}

function uniqueLFQNs
[ LFQN ] ::= lfqns::[ LFQN ] {
 return if null (lfqns)
	then []
	else if memberLFQN (head (lfqns), tail (lfqns))
		then uniqueLFQNs (tail (lfqns))
		else head (lfqns) :: uniqueLFQNs (tail (lfqns));
}

aspect production simple_package_name
pn::PackageName ::= id::Id_t {
    pn.fullyQualifiedName = fully_qualified_name_simple (id);
}

aspect production qualified_package_name
pn::PackageName ::= pn2::PackageName id::Id_t {
    pn.fullyQualifiedName = fully_qualified_name_qualified (pn2.fullyQualifiedName, id);
}

aspect production simple_package_or_type_name 
ptn::PackageOrTypeName ::= id::Id_t {
    ptn.fullyQualifiedName = fully_qualified_name_simple (id);
}

aspect production qualified_package_or_type_name
ptn::PackageOrTypeName ::=  pn::PackageOrTypeName id::Id_t {  
    ptn.fullyQualifiedName = fully_qualified_name_qualified (pn.fullyQualifiedName, id);
}

aspect production simple_type_name
tn::TypeName ::= id::Id_t {
    tn.fullyQualifiedName = fully_qualified_name_simple (id);
}

aspect production qualified_type_name
tn::TypeName ::= pn::PackageOrTypeName id::Id_t {  
    tn.fullyQualifiedName = fully_qualified_name_qualified (pn.fullyQualifiedName, id);
}

aspect production simple_expr_name
en::ExprName ::= id::Id_t {
    en.fullyQualifiedName = fully_qualified_name_simple (id);
}

aspect production qualified_expr_name
en::ExprName ::= an::AmbiguousName id::Id_t  { 
    en.fullyQualifiedName = fully_qualified_name_qualified (an.fullyQualifiedName, id);
}

aspect production simple_method_name
mn::MethodName ::= id::Id_t  { 
    mn.fullyQualifiedName = fully_qualified_name_simple (id);
}

aspect production qualified_method_name
mn::MethodName ::= an::AmbiguousName  id::Id_t  { 
    mn.fullyQualifiedName = fully_qualified_name_qualified (an.fullyQualifiedName, id);
}

aspect production simple_ambiguous_name
an::AmbiguousName ::= id::Id_t {
    an.fullyQualifiedName = fully_qualified_name_simple (id);
}

aspect production qualified_ambiguous_name
andi::AmbiguousName ::= an::AmbiguousName  id::Id_t  { 
    andi.fullyQualifiedName = fully_qualified_name_qualified (an.fullyQualifiedName, id);
}

aspect production type_names_none
ns::TypeNames ::= {
    ns.fullyQualifiedNames = [];
}

aspect production type_names_one
ns::TypeNames ::= n::TypeName {
    ns.fullyQualifiedNames = [ n.fullyQualifiedName ];
}

aspect production type_names_snoc
ns::TypeNames ::= ns1::TypeNames n::TypeName {
    ns.fullyQualifiedNames = ns1.fullyQualifiedNames ++ [ n.fullyQualifiedName ];
}

-- Needed Types
----------------------------------------------------

synthesized attribute neededImportedSingleTypes :: [ LFQN ];
attribute neededImportedSingleTypes occurs on   	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

synthesized attribute neededCurrentPackageTypes :: [ LFQN ];
attribute neededCurrentPackageTypes occurs on   	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

synthesized attribute neededImportedOnDemandTypes :: [ LFQN ];
attribute neededImportedOnDemandTypes occurs on 	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

synthesized attribute neededFullyQualifiedTypes :: [ FullyQualifiedName ] ;
attribute neededFullyQualifiedTypes occurs on	  	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Package_Dcl, Import_Dcls, Import_Dcl, 
							Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs;
