grammar edu:umn:cs:melt:ableJ14:concretesyntax;
import  edu:umn:cs:melt:ableJ14:terminals ;
import edu:umn:cs:melt:ableJ14:abstractsyntax ;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs ;

nonterminal Root_C ;
nonterminal compilationUnit, packageDefinition, importDefinitions, importDefinition, typeDefinitions, typeDefinition, declaration, type, arrayType, 
	declaratorBracketsOpt, declaratorBrackets, referenceType, primitiveType, nameConcrete, simpleNameConcrete, qualifiedNameConcrete, modifiersOpt, modifiers, modifier, classDefinition, superClassClause, 
	interfaceDefinition, classBlock, interfaceExtends, implementsClause, names, classMemberDefinitions, classMemberDefinition, interfaceBlock, interfaceMemberDefinitions, 
	interfaceMemberDefinition, explicitConstructorInvocation, expressionListOpt, expressionList, variableDeclarators, variableDeclarator, initializer, initializers, 
	arrayInitializer, throwsClause, parameterDeclarationList, parameterDeclarations, parameterDeclaration, blockConcrete, blockStatements, blockStatement, statement, 
	caseGroups, caseGroup, cases, aCase, switchBlock, forInit, forCond, forIter, finallyClause, handlers, handler, expression, declaratorExpressions, 
	constructorBody, statementExpression, statementExpressionList, methodDeclarator, variableDeclaratorId, primaryExpression, primaryExpressionAndArrayCreation,methodBody;

synthesized attribute ast_Root :: Root;
synthesized attribute ast_Package_Dcl :: Package_Dcl ;
synthesized attribute ast_Import_Dcls :: Import_Dcls ;
synthesized attribute ast_Import_Dcl :: Import_Dcl ;
synthesized attribute ast_Type_Dcls :: Type_Dcls ;
synthesized attribute ast_Type_Dcl :: Type_Dcl ;
synthesized attribute ast_Package ::   Package ;
synthesized attribute ast_Class_Dcl :: Class_Dcl ;
synthesized attribute ast_Interface_Dcl :: Interface_Dcl ;
synthesized attribute ast_Class_Body :: Class_Body ;
synthesized attribute ast_Class_Member_Dcls :: Class_Member_Dcls ;
synthesized attribute ast_Class_Member_Dcl :: Class_Member_Dcl ;
synthesized attribute ast_Interface_Member_Dcls :: Interface_Member_Dcls ;
synthesized attribute ast_Interface_Member_Dcl :: Interface_Member_Dcl ;
synthesized attribute ast_Constructor_Invocation :: Constructor_Invocation ;
synthesized attribute ast_Modifiers :: Modifiers ;
synthesized attribute ast_Modifier :: Modifier ;
synthesized attribute ast_PackageName :: PackageName ;
synthesized attribute ast_TypeName :: TypeName ;
synthesized attribute ast_TypeNames :: TypeNames ;
synthesized attribute ast_ExprName :: ExprName ;
synthesized attribute ast_MethodName :: MethodName ;
synthesized attribute ast_PackageOrTypeName :: PackageOrTypeName ;
synthesized attribute ast_AmbiguousName :: AmbiguousName ;

synthesized attribute ast_Type :: Type ;
synthesized attribute ast_Primitive_Type :: Primitive_Type ;
synthesized attribute ast_Reference_Type :: Reference_Type ;
synthesized attribute ast_Array_Type :: Array_Type ;

synthesized attribute ast_Method_Declarator :: Method_Declarator ;
synthesized attribute ast_Formal_Params :: Formal_Params ;
synthesized attribute ast_Formal_Param :: Formal_Param ;
synthesized attribute ast_Throws :: Throws ;
synthesized attribute ast_Var_Declarators :: Var_Declarators ; 
synthesized attribute ast_Var_Declarator :: Var_Declarator ; 
synthesized attribute ast_Var_Declarator_Id :: Var_Declarator_Id ; 

synthesized attribute ast_Var_Init :: Var_Init ;
synthesized attribute ast_Array_Init :: Array_Init ;
synthesized attribute ast_Var_Inits :: Var_Inits ;
synthesized attribute ast_Dim_Exprs :: Dim_Exprs ;

synthesized attribute ast_Block :: Block ;
synthesized attribute ast_Stmt :: Stmt ;
synthesized attribute ast_Local_Var_Dcl :: Local_Var_Dcl ;

synthesized attribute ast_Expr :: Expr ;
synthesized attribute ast_Stmt_Expr :: Stmt_Expr ;
synthesized attribute ast_LHS :: LHS ;

synthesized attribute ast_Exprs :: Exprs ;
synthesized attribute ast_Stmt_Exprs :: Stmt_Exprs ;

synthesized attribute ast_For_Test :: For_Test ;
synthesized attribute ast_For_Init :: For_Init ;
synthesized attribute ast_For_Update :: For_Update ;

synthesized attribute ast_Switch_Block :: Switch_Block ;
synthesized attribute ast_Switch_Groups :: Switch_Groups ;
synthesized attribute ast_Switch_Group :: Switch_Group ;
synthesized attribute ast_Switch_Labels :: Switch_Labels ;
synthesized attribute ast_Switch_Label :: Switch_Label ;

synthesized attribute ast_Catches :: Catches ;
synthesized attribute ast_Catch :: Catch ;


attribute ast_Type occurs on type ;
attribute ast_Primitive_Type occurs on primitiveType;
attribute ast_Reference_Type occurs on referenceType;
attribute ast_Array_Type occurs on arrayType;

attribute ast_PackageName occurs on nameConcrete,simpleNameConcrete,qualifiedNameConcrete;
attribute ast_TypeName occurs on nameConcrete,simpleNameConcrete,qualifiedNameConcrete, superClassClause;
attribute ast_TypeNames occurs on interfaceExtends, implementsClause, names;
attribute ast_ExprName occurs on nameConcrete,simpleNameConcrete,qualifiedNameConcrete;
attribute ast_MethodName occurs on nameConcrete,simpleNameConcrete,qualifiedNameConcrete; 
attribute ast_PackageOrTypeName occurs on nameConcrete,simpleNameConcrete,qualifiedNameConcrete;
attribute ast_AmbiguousName occurs on nameConcrete,simpleNameConcrete,qualifiedNameConcrete;
attribute ast_Root occurs on Root_C, compilationUnit;
attribute ast_Package_Dcl occurs on packageDefinition;
attribute ast_Import_Dcls occurs on importDefinitions;
attribute ast_Import_Dcl occurs on importDefinition;
attribute ast_Type_Dcls occurs on typeDefinitions;
attribute ast_Type_Dcl occurs on typeDefinition;
attribute ast_Class_Dcl occurs on classDefinition;
attribute ast_Interface_Dcl occurs on interfaceDefinition;
attribute ast_Modifiers occurs on modifiersOpt, modifiers;
attribute ast_Modifier  occurs on modifier;
attribute ast_Class_Body occurs on classBlock;
attribute ast_Class_Member_Dcls occurs on  classMemberDefinitions;
attribute ast_Interface_Member_Dcls occurs on  interfaceBlock, interfaceMemberDefinitions;
attribute ast_Class_Member_Dcl occurs on classMemberDefinition;
attribute ast_Interface_Member_Dcl occurs on interfaceMemberDefinition;
attribute ast_Constructor_Invocation occurs on explicitConstructorInvocation;

attribute ast_Method_Declarator occurs on methodDeclarator ;
attribute ast_Formal_Params occurs on parameterDeclarationList, parameterDeclarations;
attribute ast_Formal_Param occurs on parameterDeclaration ;
attribute ast_Throws occurs on throwsClause;
attribute ast_Var_Declarator_Id occurs on variableDeclaratorId ;

attribute ast_Var_Init occurs on initializer;
attribute ast_Array_Init occurs on arrayInitializer;
attribute ast_Var_Inits occurs on initializers;
attribute ast_Dim_Exprs occurs on declaratorExpressions;

attribute ast_Block occurs on blockConcrete, finallyClause, constructorBody;
attribute ast_Stmt occurs on blockStatements, blockStatement, statement;

attribute ast_Local_Var_Dcl occurs on declaration;
attribute ast_Var_Declarator occurs on variableDeclarator;
attribute ast_Var_Declarators occurs on variableDeclarators;

attribute ast_Stmt_Expr occurs on statementExpression, expression, primaryExpression, primaryExpressionAndArrayCreation; -- todo
attribute ast_Expr occurs on expression, primaryExpression, primaryExpressionAndArrayCreation;
attribute ast_LHS occurs on expression;

attribute ast_Stmt_Exprs occurs on statementExpressionList;
attribute ast_Exprs occurs on expressionListOpt, expressionList;

attribute ast_For_Init occurs on forInit;
attribute ast_For_Test occurs on forCond;
attribute ast_For_Update occurs on forIter;

attribute ast_Switch_Block occurs on switchBlock;
attribute ast_Switch_Groups occurs on caseGroups;
attribute ast_Switch_Group occurs on caseGroup;
attribute ast_Switch_Labels occurs on cases;
attribute ast_Switch_Label occurs on aCase;

attribute ast_Catches occurs on handlers;
attribute ast_Catch occurs on handler;

synthesized attribute dims :: Integer ;
attribute dims occurs on declaratorBracketsOpt, declaratorBrackets;

synthesized attribute isStmtExpr :: Boolean;
attribute isStmtExpr occurs on expression, primaryExpression, primaryExpressionAndArrayCreation;

-- Productions

synthesized attribute canparse :: String occurs on Root_C ;
-- Adding aspect on root production
aspect production root_c
top::Root_C 
	::=	cu::compilationUnit { 
 top.canparse = "success" ;
} 

concrete production root_c
top::Root_C
	::=	cu::compilationUnit { 
 top.ast_Root = cu.ast_Root ;
}

-- Compilation Unit: In Java, this is a single file.  This is the start
--   rule for this parser
concrete production compilationUnit_c 
top::compilationUnit
	::=	-- A compilation unit starts with an optional package definition
		pd::packageDefinition

		-- Next we have a series of zero or more import statements
		ids::importDefinitions

		-- Wrapping things up with any number of class or interface
		--    definitions
		tds::typeDefinitions {
 top.ast_Root = compilation_unit (pd.ast_Package_Dcl, ids.ast_Import_Dcls, tds.ast_Type_Dcls );   
}


-- Package statement: "package" followed by a package name
concrete production packageDefinition_c 
top::packageDefinition
	::=	'package' n::nameConcrete ';' { 
 top.ast_Package_Dcl = package_dcl (n.ast_PackageName);
}

concrete production packageDefinitionEmpty_c 
top::packageDefinition
	::=	{
 top.ast_Package_Dcl = package_dcl_none () ;
}

concrete production importDefinitionsSnoc_c 
top::importDefinitions
	::=	many::importDefinitions one::importDefinition {
 top.ast_Import_Dcls = import_dcls_snoc (many.ast_Import_Dcls, one.ast_Import_Dcl);
}

concrete production importDefinitionsEmpty_c 
top::importDefinitions
	::=	{
 top.ast_Import_Dcls = import_dcls_none () ;
}

-- Import statement: import followed by a package or class name
concrete production importDefinition_c 
top::importDefinition
	::=	t::Import_t n::nameConcrete ';' {
 top.ast_Import_Dcl = import_dcl (t, n.ast_TypeName);
}

concrete production importDefinitionOnDemand_c 
top::importDefinition
	::=	'import' n::nameConcrete '.' '*' ';' {
 top.ast_Import_Dcl = import_dcl_on_demand (n.ast_PackageOrTypeName);
}

concrete production typeDefinitionsSnoc_c 
top::typeDefinitions
	::=	many::typeDefinitions one::typeDefinition {
 top.ast_Type_Dcls = type_dcls_snoc (many.ast_Type_Dcls, one.ast_Type_Dcl);
}

concrete production typeDefinitionsEmpty_c 
top::typeDefinitions
	::=	{
 top.ast_Type_Dcls = type_dcls_none() ;
}

-- A type definition in a file is either a class or interface definition, or nothing.
concrete production typeDefinitionClass_c 
top::typeDefinition
	::=	cd::classDefinition {
 top.ast_Type_Dcl = type_class_dcl (cd.ast_Class_Dcl);
}

concrete production typeDefinitionInterface_c 
top::typeDefinition
	::=	id::interfaceDefinition {
 top.ast_Type_Dcl  = type_interface_dcl (id.ast_Interface_Dcl);
}

concrete production typeDefinitionEmpty_c 
top::typeDefinition
	::=	';' {
 top.ast_Type_Dcl = type_dcl_empty ();
}

-- A declaration is the creation of a reference or primitive-type variable
--   Create a separate Type/Var tree for each var in the var list.
--
concrete production declaration_c 
top::declaration
	::=	t::type v::variableDeclarators {
 top.ast_Local_Var_Dcl = local_var_dcl (t.ast_Type, v.ast_Var_Declarators);
}

concrete production declarationFinal_c 
top::declaration
	::=	'final' t::type v::variableDeclarators {
 top.ast_Local_Var_Dcl = local_var_dcl_final (t.ast_Type, v.ast_Var_Declarators);
}

-- A type specification is a type name with possible brackets afterwards
--   (which would make it an array type).
concrete production typeReference_c 
top::type
	::= 	t::referenceType {
 top.ast_Type = reference_type (t.ast_Reference_Type) ;
}


concrete production typePrimitive_c 
top::type
	::= 	t::primitiveType {
 top.ast_Type = primitive_type (t.ast_Primitive_Type) ;
}

concrete production typeVoid_c 
top::type
	::= 	'void' {
 top.ast_Type = void_type ();
}

concrete production typeArray_c 
top::referenceType
	::= 	t::arrayType { 
 top.ast_Reference_Type = array_type ( t.ast_Array_Type ) ;
}

concrete production typeArrayReference_c
top::arrayType
	::=	n::nameConcrete ds::declaratorBrackets {
 top.ast_Array_Type = name_array ( n.ast_TypeName, ds.dims ) ;
}

concrete production typeArrayPrimitive_c
top::arrayType
	::=	t::primitiveType ds::declaratorBrackets {
 top.ast_Array_Type = primitive_array (t.ast_Primitive_Type, ds.dims ) ;
}

concrete production declaratorBracketsSome
top::declaratorBracketsOpt
	::= 	many::declaratorBrackets { 
 top.dims = many.dims ;
}

concrete production declaratorBracketsNone
top::declaratorBracketsOpt
	::= 	{ 
 top.dims = 0 ;
}

concrete production declaratorBracketsSnoc
top::declaratorBrackets 
	::= 	many::declaratorBrackets '[' ']' { 
 top.dims = many.dims + 1 ;
}

concrete production declaratorBracketsOne
top::declaratorBrackets 
	::= 	'[' ']' { 
 top.dims = 1 ;
}

-- A type name
concrete production nameType_c
top::referenceType
	::=	n::nameConcrete {
 top.ast_Reference_Type = name_type( n.ast_TypeName ) ;
}

-- The primitive types.
concrete production booleanType_c
top::primitiveType
	::=	'boolean' { 
 top.ast_Primitive_Type = boolean_type() ;
}

concrete production byteType_c
top::primitiveType
	::=	'byte' { 
 top.ast_Primitive_Type = byte_type() ;
}

concrete production charType_c
top::primitiveType
	::=	'char' { 
 top.ast_Primitive_Type = char_type() ;
}

concrete production shortType_c 
top::primitiveType
	::=	'short' { 
 top.ast_Primitive_Type = short_type() ;
}

concrete production intType_c
top::primitiveType
	::=	'int' { 
 top.ast_Primitive_Type = int_type() ;
}

concrete production floatType_c
top::primitiveType
	::=	'float' { 
 top.ast_Primitive_Type = float_type() ;
}

concrete production longType_c
top::primitiveType
	::=	'long' { 
 top.ast_Primitive_Type = long_type() ;
}

concrete production doubleType_c
top::primitiveType
	::=	'double' { 
 top.ast_Primitive_Type = double_type() ;
}
	
-- A (possibly-qualified) java name.  We start with the first IDENT
--   and expand its name by adding dots and following IDENTS

concrete production name_c
top::nameConcrete ::= sn::simpleNameConcrete
{
 top.ast_PackageName  = sn.ast_PackageName ;
 top.ast_TypeName  = sn.ast_TypeName ;
 top.ast_ExprName  = sn.ast_ExprName ;
 top.ast_MethodName  = sn.ast_MethodName ;
 top.ast_PackageOrTypeName  = sn.ast_PackageOrTypeName ;
 top.ast_AmbiguousName  = sn.ast_AmbiguousName ;
  
}

concrete production qname_c
top::nameConcrete ::= qn::qualifiedNameConcrete
{
 top.ast_PackageName  = qn.ast_PackageName ;
 top.ast_TypeName  = qn.ast_TypeName ;
 top.ast_ExprName  = qn.ast_ExprName ;
 top.ast_MethodName  = qn.ast_MethodName ;
 top.ast_PackageOrTypeName  = qn.ast_PackageOrTypeName ;
 top.ast_AmbiguousName  = qn.ast_AmbiguousName ;
  
}

concrete production simpleName_c 
top::simpleNameConcrete
	::=	id::Id_t {
 top.ast_PackageName = simple_package_name(id) ;
 top.ast_TypeName = simple_type_name (id) ;
 top.ast_ExprName = simple_expr_name(id) ;
 top.ast_MethodName = simple_method_name (id);
 top.ast_PackageOrTypeName = simple_package_or_type_name(id);
 top.ast_AmbiguousName = simple_ambiguous_name (id); 
}

concrete production nameQualified_c 
top::qualifiedNameConcrete
	::=	n::nameConcrete '.' id::Id_t { 
 top.ast_PackageName =  qualified_package_name ( n.ast_PackageName, id ) ;
 top.ast_TypeName = qualified_type_name ( n.ast_PackageOrTypeName, id ) ;
 top.ast_ExprName = qualified_expr_name ( n.ast_AmbiguousName, id ) ;
 top.ast_MethodName = qualified_method_name ( n.ast_AmbiguousName, id ) ;
 top.ast_PackageOrTypeName = qualified_package_or_type_name ( n.ast_PackageOrTypeName, id ) ;
 top.ast_AmbiguousName = qualified_ambiguous_name ( n.ast_AmbiguousName, id ) ;
}

concrete production modifiersSome_c 
top::modifiersOpt
	::=	many::modifiers { 
 top.ast_Modifiers = many.ast_Modifiers;
}

concrete production modifiersEmpty_c 
top::modifiersOpt
	::=	{ 
 top.ast_Modifiers = modifiers_none() ;
}

concrete production modifiersSnoc_c 
top::modifiers
	::=	many::modifiers one::modifier { 
 top.ast_Modifiers = modifiers_snoc (many.ast_Modifiers, one.ast_Modifier) ;
}

concrete production modifiersOne_c 
top::modifiers
	::=	one::modifier { 
 top.ast_Modifiers = modifiers_snoc (modifiers_none(), one.ast_Modifier) ;
}

-- modifiers for Java classes, interfaces, class/instance vars and methods
concrete production modifierPrivate_c 
top::modifier
	::=	'private' { 
 top.ast_Modifier = private();
}

concrete production modifierPublic_c 
top::modifier
	::=	'public' { 
 top.ast_Modifier = public();
}

concrete production modifierProtected_c 
top::modifier
	::=	'protected' { 
 top.ast_Modifier = protected();
}

concrete production modifierStatic_c 
top::modifier
	::=	'static' { 
 top.ast_Modifier = static_mod();
}

concrete production modifierTransient_c 
top::modifier
	::=	'transient' { 
 top.ast_Modifier = transient();
}

concrete production modifierFinal_c 
top::modifier
	::=	'final' { 
 top.ast_Modifier = final();
}

concrete production modifierAbstract_c 
top::modifier
	::=	'abstract' { 
 top.ast_Modifier = abstract_mod();
}

concrete production modifierNative_c 
top::modifier
	::=	'native' { 
 top.ast_Modifier = native();
}

concrete production modifierSynchronized_c 
top::modifier
	::=	'synchronized' { 
 top.ast_Modifier = synchronized_mod();
}

concrete production modifierVolatile_c 
top::modifier
	::=	'volatile' { 
 top.ast_Modifier = volatile();
}

concrete production modifierStrictfp_c 
top::modifier
	::=	'strictfp' { 
 top.ast_Modifier = strictfp();
}

-- Definition of a Java class
concrete production classDefinition_c 
top::classDefinition
	::=	m::modifiersOpt 
		'class' 
		id::Id_t
		-- it _might_ have a superclass...
		sc::superClassClause
		-- it might implement some interfaces...
		ic::implementsClause
		-- now parse the body of the class
		cb::classBlock { 
 top.ast_Class_Dcl = class_dcl ( m.ast_Modifiers, id, sc.ast_TypeName, ic.ast_TypeNames, cb.ast_Class_Body ) ;
}

concrete production superClassClause_c 
top::superClassClause
	::=	'extends' n::nameConcrete { 
 top.ast_TypeName = n.ast_TypeName ; 
}

concrete production superClassClauseEmpty_c 
top::superClassClause
	::=	{ 
 top.ast_TypeName = getTypeName ("Object") ;
}

-- Definition of a Java Interface
concrete production interfaceDefinition_c 
top::interfaceDefinition
	::=	m::modifiersOpt 
		'interface' 
		id::Id_t
		-- it might extend some other interfaces
		ie::interfaceExtends
		-- now parse the body of the interface (looks like a class...)
		ib::interfaceBlock {
 top.ast_Interface_Dcl = interface_dcl (m.ast_Modifiers, id, ie.ast_TypeNames, ib.ast_Interface_Member_Dcls);
}

-- This is the body of a class.  You can have fields and extra semicolons,
-- That's about it (until you see what a field is...)
concrete production classBlock_c 
top::classBlock
	::=	'{' ds::classMemberDefinitions '}' { 
 top.ast_Class_Body = class_body (ds.ast_Class_Member_Dcls) ;
}

concrete production interfaceBlock_c 
top::interfaceBlock
	::=	'{' ds::interfaceMemberDefinitions '}' {
 top.ast_Interface_Member_Dcls = ds.ast_Interface_Member_Dcls;
}

-- An interface can extend several other interfaces...
concrete production interfaceExtends_c 
top::interfaceExtends
	::=	'extends' ns::names {
 top.ast_TypeNames = ns.ast_TypeNames ;
}

concrete production interfaceExtendsEmpty_c 
top::interfaceExtends
	::=	{ 
 top.ast_TypeNames = type_names_none();
}

-- A class can implement several interfaces...
concrete production implementsClause_c 
top::implementsClause
	::=	'implements' ns::names {
 top.ast_TypeNames = ns.ast_TypeNames ;
}

concrete production implementsClauseEmpty_c 
top::implementsClause
	::=	{ 
 top.ast_TypeNames = type_names_none();
}

concrete production namesSnoc_c 
top::names
	::=	many::names ',' one::nameConcrete { 
 top.ast_TypeNames = type_names_snoc (many.ast_TypeNames, one.ast_TypeName);
}

concrete production namesOne_c 
top::names
	::=	one::nameConcrete { 
 top.ast_TypeNames = type_names_one(one.ast_TypeName) ;
}

concrete production classMemberDefinitionsSnoc_c 
top::classMemberDefinitions
	::=	many::classMemberDefinitions one::classMemberDefinition { 
 top.ast_Class_Member_Dcls = class_member_dcls_snoc (many.ast_Class_Member_Dcls, one.ast_Class_Member_Dcl); 
}

concrete production classMemberDefinitionsEmpty_c 
top::classMemberDefinitions
	::=	{
 top.ast_Class_Member_Dcls = class_member_dcls_none(); 
}

-- Now the various things that can be defined inside a class or interface...
-- Note that not all of these are really valid in an interface (constructors,
--   for example), and if this grammar were used for a compiler there would
--   need to be some semantic checks to make sure we're doing the right thing...
concrete production classMemberDefinitionConstructor_c 
top::classMemberDefinition
	::=	mods::modifiersOpt 
		id::Id_t  -- the name of the constructor
		-- parse the formal parameter declarations.
		'(' 
			pdl::parameterDeclarationList 
		')'
		-- get the list of exceptions that this constructor is declared to throw
		tc::throwsClause
		cb::constructorBody { 
 top.ast_Class_Member_Dcl = class_constructor (mods.ast_Modifiers, id, pdl.ast_Formal_Params, tc.ast_Throws, cb.ast_Block);
}

concrete production constructorBody_c
top::constructorBody
	::=	'{' eci::explicitConstructorInvocation ss::blockStatements '}' { 
 top.ast_Block = block (stmt_seq (stmt_constructor_invocation (eci.ast_Constructor_Invocation), ss.ast_Stmt));
}

concrete production constructorBodyNoInvocation_c
top::constructorBody
	::=	'{' ss::blockStatements '}' {
 top.ast_Block = block (ss.ast_Stmt);
}

concrete production constructorBodyJustInvocation_c
top::constructorBody
	::=	'{' eci::explicitConstructorInvocation '}' { 
 top.ast_Block = block (stmt_constructor_invocation (eci.ast_Constructor_Invocation));
}

concrete production constructorBodyEmpty_c
top::constructorBody
	::=	'{' '}' { 
 top.ast_Block = empty_block ();
}

-- inner class
concrete production classMemberDefinitionInnerClass_c 
top::classMemberDefinition
	::=	cd::classDefinition { 
 top.ast_Class_Member_Dcl = inner_class (cd.ast_Class_Dcl);
}

-- inner interface
concrete production classMemberDefinitionInnerInterface_c 
top::classMemberDefinition
	::=	id::interfaceDefinition { 
 top.ast_Class_Member_Dcl = inner_interface (id.ast_Interface_Dcl);
}

concrete production classMemberDefinitionMethod_c
top::classMemberDefinition
	::=	mods::modifiersOpt 
		t::type
		md::methodDeclarator
		-- get the list of exceptions that this method is declared to throw
		tc::throwsClause
		mb::methodBody { 
 top.ast_Class_Member_Dcl = class_method (
			     case mb of
				methodBodyBlock_c (b) -> method_dcl_prod (method_header_declarator (mods.ast_Modifiers, t.ast_Type, md.ast_Method_Declarator, tc.ast_Throws), b.ast_Block) |
				methodBodyEmpty_c (_) -> method_dcl_no_body (method_header_declarator (mods.ast_Modifiers, t.ast_Type, md.ast_Method_Declarator, tc.ast_Throws))
			     end);
}

concrete production methodBodyBlock_c
top::methodBody
	::=	b::blockConcrete {
}

concrete production methodBodyEmpty_c
top::methodBody
	::=	';' {
}

concrete production methodDeclarator_c
top::methodDeclarator
	::=	id::Id_t  -- the name of the method
		-- parse the formal parameter declarations.
		'(' 
			params::parameterDeclarationList 
		')' { 
 top.ast_Method_Declarator = method_declarator (id, params.ast_Formal_Params );
}

concrete production methodDeclaratorArray_c
top::methodDeclarator
	::=	md::methodDeclarator '[' ']' { 
 top.ast_Method_Declarator = method_declarator_array (md.ast_Method_Declarator);
}

concrete production classMemberDefinitionField_c 
top::classMemberDefinition
	::=	mods::modifiersOpt t::type v::variableDeclarators ';' {
 top.ast_Class_Member_Dcl = class_field (field_dcl (mods.ast_Modifiers, t.ast_Type, v.ast_Var_Declarators));
}

-- "static { ... }" class initializer
concrete production classMemberDefinitionClassInitializer_c 
top::classMemberDefinition
	::=	'static' b::blockConcrete { 
 top.ast_Class_Member_Dcl = class_static_initializer (b.ast_Block);
}

-- "{ ... }" instance initializer
concrete production classMemberDefinitionInstanceInitializer_c 
top::classMemberDefinition
	::=	b::blockConcrete {
 top.ast_Class_Member_Dcl = class_block (b.ast_Block); 
}

-- empty
concrete production classMemberDefinitionEmpty_c 
top::classMemberDefinition
	::=	';' { 
 top.ast_Class_Member_Dcl = class_member_empty() ;
}

-- Possible interface members
concrete production interfaceMemberDefinitionsSnoc_c 
top::interfaceMemberDefinitions
	::=	many::interfaceMemberDefinitions one::interfaceMemberDefinition { 
 top.ast_Interface_Member_Dcls = interface_member_dcls_snoc (many.ast_Interface_Member_Dcls, one.ast_Interface_Member_Dcl);
}

concrete production interfaceMemberDefinitionsEmpty_c 
top::interfaceMemberDefinitions
	::=	{ 
 top.ast_Interface_Member_Dcls = interface_member_dcls_none ();
}

concrete production interfaceMemberDefinitionField_c 
top::interfaceMemberDefinition
	::=	mods::modifiersOpt t::type v::variableDeclarators ';' { 
 top.ast_Interface_Member_Dcl = interface_field (field_dcl (mods.ast_Modifiers, t.ast_Type, v.ast_Var_Declarators));
}

concrete production interfaceMemberDefinitionMethod_c
top::interfaceMemberDefinition
	::=	mods::modifiersOpt 
		t::type
		md::methodDeclarator
		-- get the list of exceptions that this method is declared to throw
		tc::throwsClause
		';' { 
 top.ast_Interface_Member_Dcl = interface_method (method_header_declarator (mods.ast_Modifiers, t.ast_Type, md.ast_Method_Declarator, tc.ast_Throws)) ;
}

-- inner class
concrete production interfaceMemberDefinitionInnerClass_c 
top::interfaceMemberDefinition
	::=	cd::classDefinition { 
 top.ast_Interface_Member_Dcl = interface_inner_class (cd.ast_Class_Dcl);
}

 -- inner interface
concrete production interfaceMemberDefinitionInnerInterface_c 
top::interfaceMemberDefinition
	::=	id::interfaceDefinition { 
 top.ast_Interface_Member_Dcl = interface_inner_interface (id.ast_Interface_Dcl);
}

concrete production interfaceMemberDefinitionEmpty_c 
top::interfaceMemberDefinition
	::=	';' { 
 top.ast_Interface_Member_Dcl = interface_empty ();
}

concrete production expressionListOptSome_c 
top::expressionListOpt
	::=	es::expressionList { 
 top.ast_Exprs = es.ast_Exprs;
}

concrete production expressionListOptEmpty_c 
top::expressionListOpt
	::=	{ 
 top.ast_Exprs = exprs_none ();
}

-- This is a list of expressions.
concrete production expressionListSnoc_c 
top::expressionList
	::=	es::expressionList ',' e::expression {
 top.ast_Exprs = exprs_snoc (es.ast_Exprs, e.ast_Expr);
}

concrete production expressionListOne_c 
top::expressionList
	::=	e::expression { 
 top.ast_Exprs = exprs_one (e.ast_Expr);
}

-- Catch obvious constructor calls, but not the expr.super(...) calls
concrete production explicitConstructorInvocationThis_c 
top::explicitConstructorInvocation
	::=	'this' '(' args::expressionListOpt ')' ';' { 
 top.ast_Constructor_Invocation = this_constructor_invocation (args.ast_Exprs);
}

concrete production explicitConstructorInvocationSuper_c 
top::explicitConstructorInvocation
	::=	'super' '(' args::expressionListOpt ')' ';' { 
 top.ast_Constructor_Invocation = super_constructor_invocation (args.ast_Exprs);
}

concrete production explicitConstructorInvocationExpressionThis_c 
top::explicitConstructorInvocation
	::=	e1::primaryExpressionAndArrayCreation '.' 'this' '(' args::expressionListOpt ')' ';' { 
 top.ast_Constructor_Invocation = this_dot_constructor_invocation (e1.ast_Expr, args.ast_Exprs);
}

concrete production explicitConstructorInvocationExpressionSuper_c 
top::explicitConstructorInvocation
	::=	e1::primaryExpressionAndArrayCreation '.' 'super' '(' args::expressionListOpt ')' ';' { 
 top.ast_Constructor_Invocation = super_dot_constructor_invocation (e1.ast_Expr, args.ast_Exprs);
}

concrete production variableDeclaratorsSnoc_c 
top::variableDeclarators
	::=	vds::variableDeclarators ',' vd::variableDeclarator { 
 top.ast_Var_Declarators = var_declarators_snoc (vds.ast_Var_Declarators, vd.ast_Var_Declarator);
}

concrete production variableDeclaratorsOne_c 
top::variableDeclarators
	::=	vd::variableDeclarator { 
 top.ast_Var_Declarators = var_declarators_one (vd.ast_Var_Declarator);
}

-- Declaration of a variable.  This can be a class/instance variable,
--    or a local variable in a method
--  It can also include possible initialization.
concrete production variableDeclarator_c 
top::variableDeclarator
	::=	vd::variableDeclaratorId {
 top.ast_Var_Declarator = var_declarator (vd.ast_Var_Declarator_Id);
}

concrete production variableDeclaratorInitializer_c 
top::variableDeclarator
	::=	vd::variableDeclaratorId '=' init::initializer { 
 top.ast_Var_Declarator = var_declarator_init (vd.ast_Var_Declarator_Id, init.ast_Var_Init);
}

concrete production variableDeclaratorId_c 
top::variableDeclaratorId
	::=	id::Id_t {
 top.ast_Var_Declarator_Id = var_declarator_id (id);
}

concrete production variableDeclaratorIdArray_c 
top::variableDeclaratorId
	::=	vd::variableDeclaratorId '[' ']' { 
 top.ast_Var_Declarator_Id = var_declarator_array (vd.ast_Var_Declarator_Id);
}

-- The two "things" that can initialize an array element are an expression
--   and another (nested) array initializer.
concrete production initializer_c 
top::initializer
	::=	e::expression { 
 top.ast_Var_Init = var_init_expr (e.ast_Expr) ;
}

concrete production initializerArray_c 
top::initializer
	::=	ai::arrayInitializer { 
 top.ast_Var_Init = var_init_array (ai.ast_Array_Init) ;
}

concrete production arrayInitializer_c 
top::arrayInitializer
	::=	'{' inits::initializers ',' '}' { 
 top.ast_Array_Init = array_init (inits.ast_Var_Inits);
}

concrete production arrayInitializerNoComma_c 
top::arrayInitializer
	::=	'{' inits::initializers '}' { 
 top.ast_Array_Init = array_init_no_comma (inits.ast_Var_Inits);
}

concrete production arrayInitializerOnlyComma_c 
top::arrayInitializer
	::=	'{' ',' '}' { 
 top.ast_Array_Init = array_init_no_var_inits ();
}

concrete production arrayInitializerEmpty_c 
top::arrayInitializer
	::=	'{' '}' { 
 top.ast_Array_Init = array_init_empty ();
}

concrete production initializersSnoc_c 
top::initializers
	::=	many::initializers ',' one::initializer { 
 top.ast_Var_Inits = var_inits_snoc (many.ast_Var_Inits, one.ast_Var_Init);
}

concrete production initializersOne_c 
top::initializers
	::=	one::initializer { 
 top.ast_Var_Inits = var_inits_one (one.ast_Var_Init);
}

-- This is a list of exception classes that the method is declared to throw
concrete production throwsClause_c 
top::throwsClause
	::=	'throws' ns::names { 
 top.ast_Throws = throws ( ns.ast_TypeNames ) ;
}

concrete production throwsClauseEmpty_c 
top::throwsClause
	::=	{ 
 top.ast_Throws = throws_none();
}

-- A list of formal parameters
concrete production parameterDeclarationListEmpty_c 
top::parameterDeclarationList
	::=	{ 
 top.ast_Formal_Params = formal_params_none();
}

concrete production parameterDeclarationListSome_c 
top::parameterDeclarationList
	::=	ps::parameterDeclarations { 
 top.ast_Formal_Params = ps.ast_Formal_Params ;
}

concrete production parameterDeclarationsSnoc_c 
top::parameterDeclarations
	::=	ps::parameterDeclarations ',' pd::parameterDeclaration { 
 top.ast_Formal_Params = formal_params_snoc (ps.ast_Formal_Params, pd.ast_Formal_Param);
}

concrete production parameterDeclarationsOne_c 
top::parameterDeclarations
	::=	pd::parameterDeclaration { 
 top.ast_Formal_Params = formal_params_one (pd.ast_Formal_Param);
}

-- A formal parameter.
concrete production parameterDeclaration_c 
top::parameterDeclaration
	::=	t::type vd::variableDeclaratorId {
 top.ast_Formal_Param = formal_param (t.ast_Type, vd.ast_Var_Declarator_Id );
}

concrete production parameterDeclarationFinal_c 
top::parameterDeclaration
	::=	'final' t::type vd::variableDeclaratorId {
 top.ast_Formal_Param = final_formal_param (t.ast_Type, vd.ast_Var_Declarator_Id );
}

-- Compound statement.  This is used in many contexts:
--   Inside a class definition prefixed with "static":
--      it is a class initializer
--   Inside a class definition without "static":
--      it is an instance initializer
--   As the body of a method
--   As a completely independent braced block of code inside a method
--      it starts a new scope for variable definitions
concrete production block_c 
top::blockConcrete
	::=	'{' ss::blockStatements '}' {
 top.ast_Block = block (ss.ast_Stmt);
}

concrete production blockEmpty_c 
top::blockConcrete
	::=	'{' '}' {
 top.ast_Block = empty_block ();
}

concrete production blockStatementsSnoc_c 
top::blockStatements
	::=	many::blockStatements one::blockStatement { 
 top.ast_Stmt = stmt_seq (many.ast_Stmt, one.ast_Stmt);
}

concrete production blockStatementsOne_c 
top::blockStatements
	::=	one::blockStatement { 
 top.ast_Stmt = one.ast_Stmt ;
}

concrete production statementDeclaration_c 
top::blockStatement
	::=	dcl::declaration ';' { 
 top.ast_Stmt = stmt_dcl (dcl.ast_Local_Var_Dcl) ;
}

concrete production statementClass_c 
top::blockStatement
	-- class definition
	::=	cd::classDefinition { 
 top.ast_Stmt = block_stmt_class (cd.ast_Class_Dcl);
}

concrete production statementInterface_c 
top::blockStatement
	-- interface definition
	::=	id::interfaceDefinition { 
 top.ast_Stmt = block_stmt_interface (id.ast_Interface_Dcl);
}

concrete production blockStatement_c 
top::blockStatement
	::=	s::statement { 
 top.ast_Stmt = s.ast_Stmt ;
}

concrete production statementBlock_c 
top::statement
	-- A list of statements in curly braces -- start a new scope
	::=	b::blockConcrete { 
 top.ast_Stmt = stmt_block (b.ast_Block) ;
}

concrete production statementExpression_c 
top::statement
	-- An expression statement.  This could be a method call,
	-- assignment statement, or any other expression evaluated for
	-- side-effects.
	::=	e::statementExpression ';' {
 top.ast_Stmt = stmt_stmt_expr (e.ast_Stmt_Expr); 
}

concrete production statementLabel_c 
top::statement
	-- Attach a label to the front of a statement
	::=	id::Id_t ':' s::statement { 
 top.ast_Stmt = label_prod (id, s.ast_Stmt);
}

concrete production statementIfThenElse_c 
top::statement
	-- If-else statement
	::=	t::If_t '(' e::expression ')' s::statement 'else' s2::statement { 
 top.ast_Stmt = if_then_else (t, e.ast_Expr, s.ast_Stmt, s2.ast_Stmt);
}

concrete production statementIfThen_c 
top::statement
	-- If- statement
	::=	t::If_t '(' e::expression ')' s::statement { 
 top.ast_Stmt = if_then (t, e.ast_Expr, s.ast_Stmt);
}

concrete production statementFor_c 
top::statement
	-- For statement
	::=	'for'
			'('
				init::forInit ';'	-- initializer
				cond::forCond ';'	-- condition test
				iter::forIter		-- updater
			')'
			body::statement { -- statement to loop over
 top.ast_Stmt = for (init.ast_For_Init, cond.ast_For_Test, iter.ast_For_Update, body.ast_Stmt) ;
}

concrete production statementWhile_c 
top::statement
	-- While statement
	::=	t::While_t '(' e::expression ')' s::statement { 
 top.ast_Stmt = while_prod (t, e.ast_Expr, s.ast_Stmt);
}

concrete production statementDo_c 
top::statement
	-- do-while statement
	::=	'do' s::statement t::While_t '(' e::expression ')' ';' {  
 top.ast_Stmt = do (s.ast_Stmt, t, e.ast_Expr);
}

concrete production statementBreak_c 
top::statement
	-- get out of a loop (or switch)
	::=	'break' id::Id_t ';' { 
 top.ast_Stmt = break_label (id);
}

concrete production statementBreakNoLabel_c 
top::statement
	-- get out of a loop (or switch)
	::=	'break' ';' {  
 top.ast_Stmt = break_prod ();
}

concrete production statementContinue_c 
top::statement
	-- get out of a loop (or switch)
	::=	'continue' id::Id_t ';' { 
 top.ast_Stmt = continue_label (id);
}

concrete production statementContinueNoLabel_c 
top::statement
	-- get out of a loop (or switch)
	::=	'continue' ';' {  
 top.ast_Stmt = continue_prod ();
}

concrete production statementReturn_c 
top::statement
	-- Return an expression
	::=	t::Return_t e::expression ';' {
 top.ast_Stmt = return_expr (t, e.ast_Expr);
}

concrete production statementReturnNoExpression_c 
top::statement
	-- Return
	::=	t::Return_t ';' { 
 top.ast_Stmt = return_statement (t);
}

concrete production statementSwitch_c 
top::statement
	-- switch/case statement
	::=	'switch' '(' e::expression ')' sg::switchBlock { 
 top.ast_Stmt = switch_prod (e.ast_Expr, sg.ast_Switch_Block);
}

-- an exception handler try/catch block
concrete production statementTry_c 
top::statement
	::=	'try' b::blockConcrete hs::handlers fc::finallyClause { 
 top.ast_Stmt = case fc of
			finallyClauseEmpty_c () -> try (b.ast_Block, hs.ast_Catches) |
			finallyClause_c (_, b1) -> try_finally (b.ast_Block, hs.ast_Catches, b1.ast_Block)
		end;
}

concrete production statementThrow_c 
top::statement
	-- throw an exception
	::=	'throw' e::expression ';' { 
 top.ast_Stmt = throw (e.ast_Expr);
}

concrete production statementSynchronized_c 
top::statement
	-- synchronize a statement
	::=	'synchronized' '(' e::expression ')' b::blockConcrete {
 top.ast_Stmt = synchronized (e.ast_Expr, b.ast_Block);
}

concrete production statementAssert_c 
top::statement
	::=	'assert' e::expression ':' e2::expression ';' { 
 top.ast_Stmt = assert_colon (e.ast_Expr, e2.ast_Expr);
}

concrete production statementAssertNoExpression_c 
top::statement
	::=	'assert' e::expression ';' { 
 top.ast_Stmt = assert (e.ast_Expr);
}

concrete production statementEmpty_c 
top::statement
	-- empty statement
	::=	';' { 
 top.ast_Stmt = empty_stmt();
}

concrete production switchBlock_c
top::switchBlock
	::= '{' cgs::caseGroups cs::cases '}' { 
 top.ast_Switch_Block = switch_block (cgs.ast_Switch_Groups, cs.ast_Switch_Labels);
}

concrete production switchBlockNoLabels_c
top::switchBlock
	::= '{' cgs::caseGroups '}' { 
 top.ast_Switch_Block = switch_block_no_labels (cgs.ast_Switch_Groups);
}

concrete production switchBlockJustLabels_c
top::switchBlock
	::= '{' cs::cases '}' { 
 top.ast_Switch_Block = switch_block_no_groups (cs.ast_Switch_Labels);
}

concrete production switchBlockEmpty_c
top::switchBlock
	::= '{' '}' { 
 top.ast_Switch_Block = switch_block_empty ();
}

concrete production caseGroupsSnoc_c 
top::caseGroups
	::=	many::caseGroups one::caseGroup { 
 top.ast_Switch_Groups = switch_groups_snoc (many.ast_Switch_Groups, one.ast_Switch_Group);
}

concrete production caseGroupsOne_c 
top::caseGroups
	::=	one::caseGroup { 
 top.ast_Switch_Groups = switch_groups_one (one.ast_Switch_Group);
}

concrete production caseGroup_c 
top::caseGroup
	::=	cs::cases ss::blockStatements { 
 top.ast_Switch_Group = switch_group (cs.ast_Switch_Labels, ss.ast_Stmt);
}

concrete production casesSnoc_c 
top::cases
	::=	many::cases one::aCase { 
 top.ast_Switch_Labels = switch_labels_snoc (many.ast_Switch_Labels, one.ast_Switch_Label);
}

concrete production casesOne_c 
top::cases
	::=	one::aCase { 
 top.ast_Switch_Labels = switch_labels_one (one.ast_Switch_Label);
}

concrete production aCase_c 
top::aCase
	::=	'case' e::expression ':' { 
 top.ast_Switch_Label = switch_label (e.ast_Expr);
}

concrete production aCaseDefault_c 
top::aCase
	::=	'default' ':'	{ 
 top.ast_Switch_Label = switch_label_default ();
}

-- The initializer for a for loop
concrete production forInitDeclaration_c 
top::forInit
		-- if it looks like a declaration, it is
	::=	dcl::declaration { 
 top.ast_For_Init = for_init_dcl (dcl.ast_Local_Var_Dcl);
}

concrete production forInitExpressionList_c 
top::forInit
		-- otherwise it could be an expression list...
	::=	es::statementExpressionList { 
 top.ast_For_Init = for_init_some (es.ast_Stmt_Exprs);
}

concrete production forInitEmpty_c 
top::forInit
	::=	{ 
 top.ast_For_Init = for_init_empty ();
}

concrete production forCond_c 
top::forCond
	::=	expr::expression { 
 top.ast_For_Test = for_test_one (expr.ast_Expr);
}

concrete production forCondEmpty_c 
top::forCond
	::=	{ 
 top.ast_For_Test = for_test_none ();
}

concrete production forIter_c 
top::forIter
	::=	es::statementExpressionList { 
 top.ast_For_Update = for_update_some (es.ast_Stmt_Exprs);
}

concrete production forIterEmpty_c 
top::forIter
	::=	{ 
 top.ast_For_Update = for_update_empty ();
}

concrete production statementExpressionsSnoc_c 
top::statementExpressionList
	::=	many::statementExpressionList ',' one::statementExpression {
 top.ast_Stmt_Exprs = stmt_exprs_snoc (many.ast_Stmt_Exprs, one.ast_Stmt_Expr);
}

concrete production statementExpressionsOne_c 
top::statementExpressionList
	::=	one::statementExpression { 
 top.ast_Stmt_Exprs = stmt_exprs_one (one.ast_Stmt_Expr);
}

concrete production finallyClause_c 
top::finallyClause
	::=	'finally' b::blockConcrete {
}

concrete production finallyClauseEmpty_c 
top::finallyClause
	::=	{
}

concrete production handlersSnoc_c 
top::handlers
	::=	many::handlers one::handler { 
 top.ast_Catches = catches_snoc (many.ast_Catches, one.ast_Catch);
}

concrete production handlersEmpty_c 
top::handlers
	::=	{ 
 top.ast_Catches = catches_none ();
}

-- an exception handler
concrete production handler_c 
top::handler
	::=	'catch' '(' dcl::parameterDeclaration ')' b::blockConcrete { 
 top.ast_Catch = catch (dcl.ast_Formal_Param, b.ast_Block);
}

-- expressions

-- The operators in java have the following precedences:
--    lowest  (13)  = *= /= %= += -= <<= >>= >>>= &= ^= |=
--            (12)  ?:
--            (11)  ||
--            (10)  &&
--            ( 9)  |
--            ( 8)  ^
--            ( 7)  &
--            ( 6)  == !=
--            ( 5)  < <= > >=
--            ( 4)  << >>
--            ( 3)  +(binary) -(binary)
--            ( 2)  * / %
--            ( 1)  ++ -- +(unary) -(unary)  ~  !  (type)
--                  []   () (method call)  . (dot -- name qualification)
--                  new   ()  (explicit parenthesis)
--
-- the last two are not usually on a precedence chart; I put them in
-- to point out that new has a higher precedence than '.', so you
-- can validy use
--     new Frame().show()

concrete production statementExpressionToExpression_c
top::statementExpression
	::=	e1::expression {
 top.ast_Stmt_Expr = if e1.isStmtExpr
			then e1.ast_Stmt_Expr
			else error ("Parse error, expecting a statement");
}

concrete production assign_c
top::expression
	::=	e1::expression t::Eq_t e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> assign (l, t, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production plusAssign_c
top::expression
	::=	e1::expression '+=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> plus_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production minusAssign_c
top::expression
	::=	e1::expression '-=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> minus_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production mulAssign_c
top::expression
	::=	e1::expression '*=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> mul_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production divAssign_c
top::expression
	::=	e1::expression '/=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> div_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production modAssign_c
top::expression
	::=	e1::expression '%=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> mod_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production shiftRightAssign_c
top::expression
	::=	e1::expression '>>=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> rshift_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production unsignedShiftRightAssign_c
top::expression
	::=	e1::expression '>>>=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> urshift_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production shiftLeftAssign_c
top::expression
	::=	e1::expression '<<=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> lshift_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production andAssign_c
top::expression
	::=	e1::expression '&=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> and_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production xorAssign_c
top::expression
	::=	e1::expression '^=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> xor_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production orAssign_c
top::expression
	::=	e1::expression '|=' e2::expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> or_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

-- conditional test (level 12)
concrete production conditionalExpression_c 
top::expression
	::=	e1::expression '?' e2::expression ':' e3::expression {
 top.isStmtExpr = false;
 top.ast_Expr = conditional (e1.ast_Expr, e2.ast_Expr, e3.ast_Expr);
}

-- logical or (||)  (level 11)
concrete production logicalOrExpression_c 
top::expression
	::=	e1::expression '||' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = or_or (e1.ast_Expr, e2.ast_Expr);
}

-- logical and (&&)  (level 10)
concrete production logicalAndExpression_c 
top::expression
	::=	e1::expression '&&' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = and_and (e1.ast_Expr, e2.ast_Expr);
}

-- bitwise or non-short-circuiting or (|)  (level 9)
concrete production inclusiveOrExpression_c 
top::expression
	::=	e1::expression '|' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = or (e1.ast_Expr, e2.ast_Expr);
}

-- exclusive or (^)  (level 8)
concrete production exclusiveOrExpression_c 
top::expression
	::=	e1::expression '^' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = xor (e1.ast_Expr, e2.ast_Expr);
}

-- bitwise or non-short-circuiting and (&)  (level 7)
concrete production andExpression_c 
top::expression
	::=	e1::expression '&' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = and (e1.ast_Expr, e2.ast_Expr);
}

-- equality/inequality (==/!=) (level 6)
concrete production equalityExpression_c 
top::expression
	::=	e1::expression '==' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = eq (e1.ast_Expr, e2.ast_Expr);
}

concrete production inequalityExpression_c 
top::expression
	::=	e1::expression '!=' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = not_eq (e1.ast_Expr, e2.ast_Expr);
}

-- boolean relational expressions (level 5)
concrete production ltExpression_c 
top::expression
	::=	e1::expression '<' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = lt (e1.ast_Expr, e2.ast_Expr);
}

concrete production gtExpression_c 
top::expression
	::=	e1::expression '>' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = gt (e1.ast_Expr, e2.ast_Expr);
}

concrete production leExpression_c 
top::expression
	::=	e1::expression '<=' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = lteq (e1.ast_Expr, e2.ast_Expr);
}

concrete production geExpression_c 
top::expression
	::=	e1::expression '>=' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = gteq (e1.ast_Expr, e2.ast_Expr);
}

-- todo include this
concrete production instanceOf_c 
top::expression
	::=	e1::expression 'instanceof' t::referenceType { 
 top.isStmtExpr = false;
 top.ast_Expr = instanceof (e1.ast_Expr, t.ast_Reference_Type);
}

-- bit shift expressions (level 4)
concrete production shiftLeftExpression_c 
top::expression
	::=	e1::expression '<<' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = lshift (e1.ast_Expr, e2.ast_Expr);
}

concrete production shiftRightExpression_c 
top::expression
	::=	e1::expression '>>' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = rshift (e1.ast_Expr, e2.ast_Expr);
}

concrete production shiftRightUnsignedExpression_c 
top::expression
	::=	e1::expression '>>>' e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = urshift (e1.ast_Expr, e2.ast_Expr);
}

-- binary addition/subtraction (level 3)
concrete production plusExpression_c 
top::expression
	::=	e1::expression t::Plus_t e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = plus (e1.ast_Expr, t, e2.ast_Expr);
}

concrete production minusExpression_c 
top::expression
	::=	e1::expression t::Minus_t e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = minus (e1.ast_Expr, t, e2.ast_Expr);
}

-- multiplication/division/modulo (level 2)
concrete production mulExpression_c 
top::expression
	::=	e1::expression t::Mul_t e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = mul (e1.ast_Expr, t, e2.ast_Expr);
}

concrete production divExpression_c 
top::expression
	::=	e1::expression t::Div_t e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = div (e1.ast_Expr, t, e2.ast_Expr);
}

concrete production modExpression_c 
top::expression
	::=	e1::expression t::Mod_t e2::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = mod (e1.ast_Expr, t, e2.ast_Expr);
}

concrete production preIncExpression_c 
top::expression
	::=	'++' e1::expression { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = pre_inc (e1.ast_Expr);
}

concrete production preDecExpression_c 
top::expression
	::=	'--' e1::expression { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = pre_dec (e1.ast_Expr);
}

concrete production unaryPlusExpression_c 
top::expression
	::=	'+' e1::expression precedence = 140 { 
 top.isStmtExpr = false;
 top.ast_Expr = unary_plus (e1.ast_Expr);
}

concrete production unaryMinusExpression_c 
top::expression
	::=	'-' e1::expression precedence = 140 { 
 top.isStmtExpr = false;
 top.ast_Expr = unary_minus (e1.ast_Expr);
}

concrete production bitNotExpression_c 
top::expression
	::=	'~' e1::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = comp (e1.ast_Expr);
}

concrete production logicalNotExpression_c 
top::expression
	::=	'!' e1::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = not (e1.ast_Expr);
}

-- todo include this
concrete production primitiveTypeCastExpression_c
top::expression
	::=	'(' t::primitiveType ds::declaratorBracketsOpt ')' e1::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = if ds.dims == 0
		then cast_primitive (t.ast_Primitive_Type, e1.ast_Expr)
		else cast_primitive_array (t.ast_Primitive_Type, ds.dims, e1.ast_Expr);
}

concrete production nameCastExpression_c
top::expression
	::=	'(' e::expression ')' e1::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = cast_simple (e.ast_Expr, e1.ast_Expr);
}

concrete production nameArraycastExpression_c
top::expression
	::=	'(' n::nameConcrete ds::declaratorBrackets ')' e1::expression { 
 top.isStmtExpr = false;
 top.ast_Expr = cast_name_array (n.ast_TypeName, ds.dims, e1.ast_Expr);
}

concrete production nameExpression_c
top::expression
	::=	n::nameConcrete { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (lhs_name (n.ast_ExprName));
}

concrete production primaryExpression_c
top::expression
	::=	e1::primaryExpressionAndArrayCreation {
 top.isStmtExpr = e1.isStmtExpr;
 top.ast_Expr = e1.ast_Expr;
 top.ast_Stmt_Expr = e1.ast_Stmt_Expr;
}

concrete production primaryExpressionAndArrayCreation_c
top::primaryExpressionAndArrayCreation
	::=	e1::primaryExpression { 
 top.isStmtExpr = e1.isStmtExpr;
 top.ast_Expr = e1.ast_Expr;
 top.ast_Stmt_Expr = e1.ast_Stmt_Expr;
}

concrete production newPrimitiveArrayExpression_c 
top::primaryExpressionAndArrayCreation
	::=	'new' t::primitiveType exprs::declaratorExpressions ds::declaratorBracketsOpt { 
 top.isStmtExpr = false;
 top.ast_Expr = new_array_no_init_primitive (t.ast_Primitive_Type, exprs.ast_Dim_Exprs, ds.dims);
}

concrete production newNameArrayExpression_c 
top::primaryExpressionAndArrayCreation
	::=	'new' n::nameConcrete exprs::declaratorExpressions ds::declaratorBracketsOpt { 
 top.isStmtExpr = false;
 top.ast_Expr = new_array_no_init_name (n.ast_TypeName, exprs.ast_Dim_Exprs, ds.dims);
}

concrete production newPrimitiveArrayExpressionInitializer_c 
top::primaryExpressionAndArrayCreation
	::=	'new' t::primitiveType ds::declaratorBrackets init::arrayInitializer { 
 top.isStmtExpr = false;
 top.ast_Expr = new_array_init_primitive (t.ast_Primitive_Type, ds.dims, init.ast_Array_Init);
}

concrete production newNameArrayExpressionInitializer_c 
top::primaryExpressionAndArrayCreation
	::=	'new' n::nameConcrete ds::declaratorBrackets init::arrayInitializer {
 top.isStmtExpr = false;
 top.ast_Expr = new_array_init_name (n.ast_TypeName, ds.dims, init.ast_Array_Init);
}

-- qualified names, array expressions, method invocation, post inc/dec
concrete production expressionFieldAccessExpression_c
top::primaryExpression
	::= 	e1::primaryExpressionAndArrayCreation '.' id::Id_t { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (expr_field_access (e1.ast_Expr, id));
}

concrete production superFieldAccessExpression_c
top::primaryExpression
	::= 	'super' '.' id::Id_t { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (super_field_access (id));
}

concrete production nameSuperFieldAccessExpression_c
top::primaryExpression
	::= 	n::nameConcrete '.' 'super' '.' id::Id_t { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (name_super_field_access (n.ast_TypeName, id));
}

concrete production methodCallExpression_c
top::primaryExpression
	::=	n::nameConcrete '(' args::expressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = method_call (n.ast_MethodName, args.ast_Exprs);
}

concrete production expressionMethodCallExpression_c
top::primaryExpression
	::= 	e1::primaryExpressionAndArrayCreation '.' id::Id_t '(' args::expressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = expr_method_call (e1.ast_Expr, id, args.ast_Exprs);
}

concrete production superMethodCallExpression_c
top::primaryExpression
	::= 	'super' '.' id::Id_t '(' args::expressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = super_method_call (id, args.ast_Exprs);
}

concrete production nameSuperMethodCallExpression_c
top::primaryExpression
	::= 	n::nameConcrete '.' 'super' '.' id::Id_t '(' args::expressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = name_super_method_call (n.ast_TypeName, id, args.ast_Exprs);
}

concrete production expressionThisExpression_c
top::primaryExpression
	::= 	n::nameConcrete '.' 'this' {  
 top.isStmtExpr = false;
 top.ast_Expr = name_dot_this (n.ast_TypeName);
}

concrete production expressionDotNewExpression_c
top::primaryExpression
	::= 	e1::primaryExpressionAndArrayCreation '.' 'new' id::Id_t '(' args::expressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_expr (e1.ast_Expr, id, args.ast_Exprs);
}

concrete production expressionDotNewBodyExpression_c
top::primaryExpression
	::= 	e1::primaryExpressionAndArrayCreation '.' 'new' id::Id_t '(' args::expressionListOpt ')' cb::classBlock { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_expr_body (e1.ast_Expr, id, args.ast_Exprs, cb.ast_Class_Body);
}

concrete production nameDotNewExpression_c
top::primaryExpression
	::= 	n::nameConcrete '.' 'new' id::Id_t '(' args::expressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_name (n.ast_TypeName, id, args.ast_Exprs);
}

concrete production nameDotNewBodyExpression_c
top::primaryExpression
	::= 	n::nameConcrete '.' 'new' id::Id_t '(' args::expressionListOpt ')' cb::classBlock { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_name_body (n.ast_TypeName, id, args.ast_Exprs, cb.ast_Class_Body);
}

concrete production arrayAccessExpression_c
top::primaryExpression
	::=	n::nameConcrete '[' e1::expression ']' { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (array_access (n.ast_ExprName, e1.ast_Expr));
}

concrete production expressionArrayAccess_c
top::primaryExpression
	::= 	e1::primaryExpression '[' e2::expression ']' { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (array_access_general (e1.ast_Expr, e2.ast_Expr));
}

concrete production postIncExpression_c 
top::expression
	::=	e1::expression '++' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = post_inc (e1.ast_Expr);
}

concrete production postDecExpression_c 
top::expression
	::=	e1::expression '--' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = post_dec (e1.ast_Expr);
} 

concrete production trueConstExpression_c
top::primaryExpression
	::=	'true' { 
 top.isStmtExpr = false;
 top.ast_Expr = true_const();
}

concrete production falseConstExpression_c
top::primaryExpression
	::=	'false' { 
 top.isStmtExpr = false;
 top.ast_Expr = false_const();
}

concrete production nullConstExpression_c
top::primaryExpression
	::=	'null' { 
 top.isStmtExpr = false;
 top.ast_Expr = null_const();
}

concrete production thisExpression_c
top::primaryExpression
	::=	'this' { 
 top.isStmtExpr = false;
 top.ast_Expr = this();
}

concrete production parenExpression_c
top::primaryExpression
	::=	'(' e1::expression ')' {
 top.isStmtExpr = false;
 top.ast_Expr = e1.ast_Expr;
}

-- look for int.class and int[].class
concrete production primitiveTypeDotClassExpression_c
top::primaryExpression
	::=	t::primitiveType '.' 'class' { 
 top.isStmtExpr = false;
 top.ast_Expr = primitive_dot_class (t.ast_Primitive_Type);
}

concrete production voidDotClassExpression_c
top::primaryExpression
	::=	'void' '.' 'class' { 
 top.isStmtExpr = false;
 top.ast_Expr = void_dot_class ();
}

concrete production nameDotClassExpression_c
top::primaryExpression
	::=	n::nameConcrete '.' 'class' { 
 top.isStmtExpr = false;
 top.ast_Expr = name_dot_class (n.ast_TypeName);
}

concrete production arrayDotClassExpression_c
top::primaryExpression
	::=	t::arrayType '.' 'class' { 
 top.isStmtExpr = false;
 top.ast_Expr = array_dot_class (t.ast_Array_Type);
}

-- Match a, a.b.c refs, a.b.c(...) refs, a.b.c[], a.b.c[].class,
--   and a.b.c.class refs.  Also this(...) and super(...).  Match
--   this or super.

concrete production newClassExpression_c 
top::primaryExpression
	::=	'new' n::nameConcrete '(' args::expressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class (n.ast_TypeName, args.ast_Exprs);
}

concrete production newClassBodyExpression_c 
top::primaryExpression
	::=	'new' n::nameConcrete '(' args::expressionListOpt ')' cb::classBlock {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_body (n.ast_TypeName, args.ast_Exprs, cb.ast_Class_Body);
}

concrete production declaratorExpressionsSnoc_c 
top::declaratorExpressions
	::=	many::declaratorExpressions '[' e1::expression ']' { 
 top.ast_Dim_Exprs = dim_exprs_snoc (many.ast_Dim_Exprs, e1.ast_Expr);
}

concrete production declaratorExpressionsOne_c 
top::declaratorExpressions
	::=	'[' e1::expression ']' { 
 top.ast_Dim_Exprs = dim_exprs_one (e1.ast_Expr);
}

concrete production intConstExpression_c 
top::primaryExpression
	::=	t::Intconst_t { 
 top.isStmtExpr = false;
 top.ast_Expr = int_const(t.lexeme);
}

concrete production charConstExpression_c 
top::primaryExpression
	::=	t::Charconst_t { 
 top.isStmtExpr = false;
 top.ast_Expr = char_const(t.lexeme);
}

concrete production floatConstExpression_c 
top::primaryExpression
	::=	t::Floatconst_t { 
 top.isStmtExpr = false;
 top.ast_Expr = float_const(t.lexeme);
}

concrete production stringConstExpression_c 
top::primaryExpression
	::=	t::Stringconst_t { 
 top.isStmtExpr = false;
 top.ast_Expr = string_const(t.lexeme);
}
