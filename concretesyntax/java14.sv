grammar edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:terminals ;
import edu:umn:cs:melt:ableJ14:abstractsyntax ;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs ;

nonterminal Root_C ;
nonterminal CompilationUnit;
nonterminal PackageDefinition;
nonterminal ImportDefinitions;
nonterminal ImportDefinition;
nonterminal TypeDefinitions;
nonterminal TypeDefinition;
nonterminal Declaration;
nonterminal Type_NT;
nonterminal ArrayType;
nonterminal DeclaratorBracketsOpt;
nonterminal DeclaratorBrackets;
nonterminal ReferenceType;
nonterminal PrimitiveType;
nonterminal NameConcrete;
nonterminal SimpleNameConcrete;
nonterminal QualifiedNameConcrete;
nonterminal ModifiersOpt_NT;
nonterminal Modifiers_NT;
nonterminal Modifier_NT;
nonterminal ClassDefinition;
nonterminal SuperClassClause;
nonterminal InterfaceDefinition;
nonterminal ClassBlock;
nonterminal InterfaceExtends;
nonterminal ImplementsClause;
nonterminal Names;
nonterminal ClassMemberDefinitions;
nonterminal ClassMemberDefinition;
nonterminal InterfaceBlock;
nonterminal InterfaceMemberDefinitions;
nonterminal InterfaceMemberDefinition;
nonterminal ExplicitConstructorInvocation;
nonterminal ExpressionListOpt;
nonterminal ExpressionList;
nonterminal VariableDeclarators;
nonterminal VariableDeclarator;
nonterminal Initializer;
nonterminal Initializers;
nonterminal ArrayInitializer;
nonterminal ThrowsClause;
nonterminal ParameterDeclarationList;
nonterminal ParameterDeclarations;
nonterminal ParameterDeclaration;
nonterminal BlockConcrete;
nonterminal BlockStatements;
nonterminal BlockStatement;
nonterminal Statement;
nonterminal CaseGroups;
nonterminal CaseGroup;
nonterminal Cases;
nonterminal ACase;
nonterminal SwitchBlock;
nonterminal ForInit;
nonterminal ForCond;
nonterminal ForIter;
nonterminal FinallyClause;
nonterminal Handlers;
nonterminal Handler;
nonterminal Expression;
nonterminal DeclaratorExpressions;
nonterminal ConstructorBody;
nonterminal StatementExpression;
nonterminal StatementExpressionList;
nonterminal MethodDeclarator;
nonterminal VariableDeclaratorId;
nonterminal PrimaryExpression;
nonterminal PrimaryExpressionAndArrayCreation;
nonterminal MethodBody;

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

attribute ast_Type occurs on Type_NT ;
attribute ast_Primitive_Type occurs on PrimitiveType;
attribute ast_Reference_Type occurs on ReferenceType;
attribute ast_Array_Type occurs on ArrayType;

attribute ast_PackageName occurs on NameConcrete, SimpleNameConcrete, QualifiedNameConcrete;
attribute ast_TypeName occurs on NameConcrete, SimpleNameConcrete, QualifiedNameConcrete, SuperClassClause;
attribute ast_TypeNames occurs on InterfaceExtends, ImplementsClause, Names;
attribute ast_ExprName occurs on NameConcrete, SimpleNameConcrete, QualifiedNameConcrete;
attribute ast_MethodName occurs on NameConcrete, SimpleNameConcrete, QualifiedNameConcrete; 
attribute ast_PackageOrTypeName occurs on NameConcrete, SimpleNameConcrete, QualifiedNameConcrete;
attribute ast_AmbiguousName occurs on NameConcrete, SimpleNameConcrete, QualifiedNameConcrete;
attribute ast_Root occurs on Root_C, CompilationUnit;
attribute ast_Package_Dcl occurs on PackageDefinition;
attribute ast_Import_Dcls occurs on ImportDefinitions;
attribute ast_Import_Dcl occurs on ImportDefinition;
attribute ast_Type_Dcls occurs on TypeDefinitions;
attribute ast_Type_Dcl occurs on TypeDefinition;
attribute ast_Class_Dcl occurs on ClassDefinition;
attribute ast_Interface_Dcl occurs on InterfaceDefinition;
attribute ast_Modifiers occurs on ModifiersOpt_NT, Modifiers_NT;
attribute ast_Modifier occurs on Modifier_NT;
attribute ast_Class_Body occurs on ClassBlock;
attribute ast_Class_Member_Dcls occurs on  ClassMemberDefinitions;
attribute ast_Interface_Member_Dcls occurs on InterfaceBlock, InterfaceMemberDefinitions;
attribute ast_Class_Member_Dcl occurs on ClassMemberDefinition;
attribute ast_Interface_Member_Dcl occurs on InterfaceMemberDefinition;
attribute ast_Constructor_Invocation occurs on ExplicitConstructorInvocation;

attribute ast_Method_Declarator occurs on MethodDeclarator ;
attribute ast_Formal_Params occurs on ParameterDeclarationList, ParameterDeclarations;
attribute ast_Formal_Param occurs on ParameterDeclaration ;
attribute ast_Throws occurs on ThrowsClause;
attribute ast_Var_Declarator_Id occurs on VariableDeclaratorId ;

attribute ast_Var_Init occurs on Initializer;
attribute ast_Array_Init occurs on ArrayInitializer;
attribute ast_Var_Inits occurs on Initializers;
attribute ast_Dim_Exprs occurs on DeclaratorExpressions;

attribute ast_Block occurs on BlockConcrete, FinallyClause, ConstructorBody;
attribute ast_Stmt occurs on BlockStatements, BlockStatement, Statement;

attribute ast_Local_Var_Dcl occurs on Declaration;
attribute ast_Var_Declarator occurs on VariableDeclarator;
attribute ast_Var_Declarators occurs on VariableDeclarators;

attribute ast_Stmt_Expr occurs on StatementExpression, Expression, PrimaryExpression, PrimaryExpressionAndArrayCreation; -- todo
attribute ast_Expr occurs on Expression, PrimaryExpression, PrimaryExpressionAndArrayCreation;
attribute ast_LHS occurs on Expression;

attribute ast_Stmt_Exprs occurs on StatementExpressionList;
attribute ast_Exprs occurs on ExpressionListOpt, ExpressionList;

attribute ast_For_Init occurs on ForInit;
attribute ast_For_Test occurs on ForCond;
attribute ast_For_Update occurs on ForIter;

attribute ast_Switch_Block occurs on SwitchBlock;
attribute ast_Switch_Groups occurs on CaseGroups;
attribute ast_Switch_Group occurs on CaseGroup;
attribute ast_Switch_Labels occurs on Cases;
attribute ast_Switch_Label occurs on ACase;

attribute ast_Catches occurs on Handlers;
attribute ast_Catch occurs on Handler;

synthesized attribute dims :: Integer ;
attribute dims occurs on DeclaratorBracketsOpt, DeclaratorBrackets;

synthesized attribute isStmtExpr :: Boolean;
attribute isStmtExpr occurs on Expression, PrimaryExpression, PrimaryExpressionAndArrayCreation;

-- Productions

synthesized attribute canparse :: String occurs on Root_C ;
-- Adding aspect on root production
aspect production root_c
top::Root_C 
	::=	cu::CompilationUnit { 
 top.canparse = "success" ;
} 

concrete production root_c
top::Root_C
	::=	cu::CompilationUnit { 
 top.ast_Root = cu.ast_Root ;
}

-- Compilation Unit: In Java, this is a single file.  This is the start
--   rule for this parser
concrete production compilationUnit_c 
top::CompilationUnit
	::=	-- A compilation unit starts with an optional package definition
		pd::PackageDefinition

		-- Next we have a series of zero or more import statements
		ids::ImportDefinitions

		-- Wrapping things up with any number of class or interface
		--    definitions
		tds::TypeDefinitions {
 top.ast_Root = compilation_unit (pd.ast_Package_Dcl, ids.ast_Import_Dcls, tds.ast_Type_Dcls );   
}


-- Package statement: "package" followed by a package name
concrete production packageDefinition_c 
top::PackageDefinition
	::=	'package' n::NameConcrete ';' { 
 top.ast_Package_Dcl = package_dcl (n.ast_PackageName);
}

concrete production packageDefinitionEmpty_c 
top::PackageDefinition
	::=	{
 top.ast_Package_Dcl = package_dcl_none () ;
}

concrete production importDefinitionsSnoc_c 
top::ImportDefinitions
	::=	many::ImportDefinitions one::ImportDefinition {
 top.ast_Import_Dcls = import_dcls_snoc (many.ast_Import_Dcls, one.ast_Import_Dcl);
}

concrete production importDefinitionsEmpty_c 
top::ImportDefinitions
	::=	{
 top.ast_Import_Dcls = import_dcls_none () ;
}

-- Import statement: import followed by a package or class name
concrete production importDefinition_c 
top::ImportDefinition
	::=	t::Import_t n::NameConcrete ';' {
 top.ast_Import_Dcl = import_dcl (t, n.ast_TypeName);
}

concrete production importDefinitionOnDemand_c 
top::ImportDefinition
	::=	'import' n::NameConcrete '.' '*' ';' {
 top.ast_Import_Dcl = import_dcl_on_demand (n.ast_PackageOrTypeName);
}

concrete production typeDefinitionsSnoc_c 
top::TypeDefinitions
	::=	many::TypeDefinitions one::TypeDefinition {
 top.ast_Type_Dcls = type_dcls_snoc (many.ast_Type_Dcls, one.ast_Type_Dcl);
}

concrete production typeDefinitionsEmpty_c 
top::TypeDefinitions
	::=	{
 top.ast_Type_Dcls = type_dcls_none() ;
}

-- A type definition in a file is either a class or interface definition, or nothing.
concrete production typeDefinitionClass_c 
top::TypeDefinition
	::=	cd::ClassDefinition {
 top.ast_Type_Dcl = type_class_dcl (cd.ast_Class_Dcl);
}

concrete production typeDefinitionInterface_c 
top::TypeDefinition
	::=	id::InterfaceDefinition {
 top.ast_Type_Dcl  = type_interface_dcl (id.ast_Interface_Dcl);
}

concrete production typeDefinitionEmpty_c 
top::TypeDefinition
	::=	';' {
 top.ast_Type_Dcl = type_dcl_empty ();
}

-- A declaration is the creation of a reference or primitive-type variable
--   Create a separate Type/Var tree for each var in the var list.
--
concrete production declaration_c 
top::Declaration
	::=	t::Type_NT v::VariableDeclarators {
 top.ast_Local_Var_Dcl = local_var_dcl (t.ast_Type, v.ast_Var_Declarators);
}

concrete production declarationFinal_c 
top::Declaration
	::=	'final' t::Type_NT v::VariableDeclarators {
 top.ast_Local_Var_Dcl = local_var_dcl_final (t.ast_Type, v.ast_Var_Declarators);
}

-- A type specification is a type name with possible brackets afterwards
--   (which would make it an array type).
concrete production typeReference_c 
top::Type_NT
	::= 	t::ReferenceType {
 top.ast_Type = reference_type (t.ast_Reference_Type) ;
}


concrete production typePrimitive_c 
top::Type_NT
	::= 	t::PrimitiveType {
 top.ast_Type = primitive_type (t.ast_Primitive_Type) ;
}

concrete production typeVoid_c 
top::Type_NT
	::= 	'void' {
 top.ast_Type = void_type ();
}

concrete production typeArray_c 
top::ReferenceType
	::= 	t::ArrayType { 
 top.ast_Reference_Type = array_type ( t.ast_Array_Type ) ;
}

concrete production typeArrayReference_c
top::ArrayType
	::=	n::NameConcrete ds::DeclaratorBrackets {
 top.ast_Array_Type = name_array ( n.ast_TypeName, ds.dims ) ;
}

concrete production typeArrayPrimitive_c
top::ArrayType
	::=	t::PrimitiveType ds::DeclaratorBrackets {
 top.ast_Array_Type = primitive_array (t.ast_Primitive_Type, ds.dims ) ;
}

concrete production declaratorBracketsSome
top::DeclaratorBracketsOpt
	::= 	many::DeclaratorBrackets { 
 top.dims = many.dims ;
}

concrete production declaratorBracketsNone
top::DeclaratorBracketsOpt
	::= 	{ 
 top.dims = 0 ;
}

concrete production declaratorBracketsSnoc
top::DeclaratorBrackets 
	::= 	many::DeclaratorBrackets '[' ']' { 
 top.dims = many.dims + 1 ;
}

concrete production declaratorBracketsOne
top::DeclaratorBrackets 
	::= 	'[' ']' { 
 top.dims = 1 ;
}

-- A type name
concrete production nameType_c
top::ReferenceType
	::=	n::NameConcrete {
 top.ast_Reference_Type = name_type( n.ast_TypeName ) ;
}

-- The primitive types.
concrete production booleanType_c
top::PrimitiveType
	::=	'boolean' { 
 top.ast_Primitive_Type = boolean_type() ;
}

concrete production byteType_c
top::PrimitiveType
	::=	'byte' { 
 top.ast_Primitive_Type = byte_type() ;
}

concrete production charType_c
top::PrimitiveType
	::=	'char' { 
 top.ast_Primitive_Type = char_type() ;
}

concrete production shortType_c 
top::PrimitiveType
	::=	'short' { 
 top.ast_Primitive_Type = short_type() ;
}

concrete production intType_c
top::PrimitiveType
	::=	'int' { 
 top.ast_Primitive_Type = int_type() ;
}

concrete production floatType_c
top::PrimitiveType
	::=	'float' { 
 top.ast_Primitive_Type = float_type() ;
}

concrete production longType_c
top::PrimitiveType
	::=	'long' { 
 top.ast_Primitive_Type = long_type() ;
}

concrete production doubleType_c
top::PrimitiveType
	::=	'double' { 
 top.ast_Primitive_Type = double_type() ;
}
	
-- A (possibly-qualified) java name.  We start with the first IDENT
--   and expand its name by adding dots and following IDENTS

concrete production name_c
top::NameConcrete ::= sn::SimpleNameConcrete
{
 top.ast_PackageName  = sn.ast_PackageName ;
 top.ast_TypeName  = sn.ast_TypeName ;
 top.ast_ExprName  = sn.ast_ExprName ;
 top.ast_MethodName  = sn.ast_MethodName ;
 top.ast_PackageOrTypeName  = sn.ast_PackageOrTypeName ;
 top.ast_AmbiguousName  = sn.ast_AmbiguousName ;
  
}

concrete production qname_c
top::NameConcrete ::= qn::QualifiedNameConcrete
{
 top.ast_PackageName  = qn.ast_PackageName ;
 top.ast_TypeName  = qn.ast_TypeName ;
 top.ast_ExprName  = qn.ast_ExprName ;
 top.ast_MethodName  = qn.ast_MethodName ;
 top.ast_PackageOrTypeName  = qn.ast_PackageOrTypeName ;
 top.ast_AmbiguousName  = qn.ast_AmbiguousName ;
  
}

concrete production simpleName_c 
top::SimpleNameConcrete
	::=	id::Id_t {
 top.ast_PackageName = simple_package_name(id) ;
 top.ast_TypeName = simple_type_name (id) ;
 top.ast_ExprName = simple_expr_name(id) ;
 top.ast_MethodName = simple_method_name (id);
 top.ast_PackageOrTypeName = simple_package_or_type_name(id);
 top.ast_AmbiguousName = simple_ambiguous_name (id); 
}

concrete production nameQualified_c 
top::QualifiedNameConcrete
	::=	n::NameConcrete '.' id::Id_t { 
 top.ast_PackageName =  qualified_package_name ( n.ast_PackageName, id ) ;
 top.ast_TypeName = qualified_type_name ( n.ast_PackageOrTypeName, id ) ;
 top.ast_ExprName = qualified_expr_name ( n.ast_AmbiguousName, id ) ;
 top.ast_MethodName = qualified_method_name ( n.ast_AmbiguousName, id ) ;
 top.ast_PackageOrTypeName = qualified_package_or_type_name ( n.ast_PackageOrTypeName, id ) ;
 top.ast_AmbiguousName = qualified_ambiguous_name ( n.ast_AmbiguousName, id ) ;
}

concrete production modifiersSome_c 
top::ModifiersOpt_NT
	::=	many::Modifiers_NT { 
 top.ast_Modifiers = many.ast_Modifiers;
}

concrete production modifiersEmpty_c 
top::ModifiersOpt_NT
	::=	{ 
 top.ast_Modifiers = modifiers_none() ;
}

concrete production modifiersSnoc_c 
top::Modifiers_NT
	::=	many::Modifiers_NT one::Modifier_NT { 
 top.ast_Modifiers = modifiers_snoc (many.ast_Modifiers, one.ast_Modifier) ;
}

concrete production modifiersOne_c 
top::Modifiers_NT
	::=	one::Modifier_NT { 
 top.ast_Modifiers = modifiers_snoc (modifiers_none(), one.ast_Modifier) ;
}

-- modifiers for Java classes, interfaces, class/instance vars and methods
concrete production modifierPrivate_c 
top::Modifier_NT
	::=	'private' { 
 top.ast_Modifier = private();
}

concrete production modifierPublic_c 
top::Modifier_NT
	::=	'public' { 
 top.ast_Modifier = public();
}

concrete production modifierProtected_c 
top::Modifier_NT
	::=	'protected' { 
 top.ast_Modifier = protected();
}

concrete production modifierStatic_c 
top::Modifier_NT
	::=	'static' { 
 top.ast_Modifier = static_mod();
}

concrete production modifierTransient_c 
top::Modifier_NT
	::=	'transient' { 
 top.ast_Modifier = transient();
}

concrete production modifierFinal_c 
top::Modifier_NT
	::=	'final' { 
 top.ast_Modifier = final();
}

concrete production modifierAbstract_c 
top::Modifier_NT
	::=	'abstract' { 
 top.ast_Modifier = abstract_mod();
}

concrete production modifierNative_c 
top::Modifier_NT
	::=	'native' { 
 top.ast_Modifier = native();
}

concrete production modifierSynchronized_c 
top::Modifier_NT
	::=	'synchronized' { 
 top.ast_Modifier = synchronized_mod();
}

concrete production modifierVolatile_c 
top::Modifier_NT
	::=	'volatile' { 
 top.ast_Modifier = volatile();
}

concrete production modifierStrictfp_c 
top::Modifier_NT
	::=	'strictfp' { 
 top.ast_Modifier = strictfp();
}

-- Definition of a Java class
concrete production classDefinition_c 
top::ClassDefinition
	::=	m::ModifiersOpt_NT
		'class' 
		id::Id_t
		-- it _might_ have a superclass...
		sc::SuperClassClause
		-- it might implement some interfaces...
		ic::ImplementsClause
		-- now parse the body of the class
		cb::ClassBlock { 
 top.ast_Class_Dcl = class_dcl ( m.ast_Modifiers, id, sc.ast_TypeName, ic.ast_TypeNames, cb.ast_Class_Body ) ;
}

concrete production superClassClause_c 
top::SuperClassClause
	::=	'extends' n::NameConcrete { 
 top.ast_TypeName = n.ast_TypeName ; 
}

concrete production superClassClauseEmpty_c 
top::SuperClassClause
	::=	{ 
 top.ast_TypeName = getTypeName ("Object") ;
}

-- Definition of a Java Interface
concrete production interfaceDefinition_c 
top::InterfaceDefinition
	::=	m::ModifiersOpt_NT
		'interface' 
		id::Id_t
		-- it might extend some other interfaces
		ie::InterfaceExtends
		-- now parse the body of the interface (looks like a class...)
		ib::InterfaceBlock {
 top.ast_Interface_Dcl = interface_dcl (m.ast_Modifiers, id, ie.ast_TypeNames, ib.ast_Interface_Member_Dcls);
}

-- This is the body of a class.  You can have fields and extra semicolons,
-- That's about it (until you see what a field is...)
concrete production classBlock_c 
top::ClassBlock
	::=	'{' ds::ClassMemberDefinitions '}' { 
 top.ast_Class_Body = class_body (ds.ast_Class_Member_Dcls) ;
}

concrete production interfaceBlock_c 
top::InterfaceBlock
	::=	'{' ds::InterfaceMemberDefinitions '}' {
 top.ast_Interface_Member_Dcls = ds.ast_Interface_Member_Dcls;
}

-- An interface can extend several other interfaces...
concrete production interfaceExtends_c 
top::InterfaceExtends
	::=	'extends' ns::Names {
 top.ast_TypeNames = ns.ast_TypeNames ;
}

concrete production interfaceExtendsEmpty_c 
top::InterfaceExtends
	::=	{ 
 top.ast_TypeNames = type_names_none();
}

-- A class can implement several interfaces...
concrete production implementsClause_c 
top::ImplementsClause
	::=	'implements' ns::Names {
 top.ast_TypeNames = ns.ast_TypeNames ;
}

concrete production implementsClauseEmpty_c 
top::ImplementsClause
	::=	{ 
 top.ast_TypeNames = type_names_none();
}

concrete production namesSnoc_c 
top::Names
	::=	many::Names ',' one::NameConcrete { 
 top.ast_TypeNames = type_names_snoc (many.ast_TypeNames, one.ast_TypeName);
}

concrete production namesOne_c 
top::Names
	::=	one::NameConcrete { 
 top.ast_TypeNames = type_names_one(one.ast_TypeName) ;
}

concrete production classMemberDefinitionsSnoc_c 
top::ClassMemberDefinitions
	::=	many::ClassMemberDefinitions one::ClassMemberDefinition { 
 top.ast_Class_Member_Dcls = class_member_dcls_snoc (many.ast_Class_Member_Dcls, one.ast_Class_Member_Dcl); 
}

concrete production classMemberDefinitionsEmpty_c 
top::ClassMemberDefinitions
	::=	{
 top.ast_Class_Member_Dcls = class_member_dcls_none(); 
}

-- Now the various things that can be defined inside a class or interface...
-- Note that not all of these are really valid in an interface (constructors,
--   for example), and if this grammar were used for a compiler there would
--   need to be some semantic checks to make sure we're doing the right thing...
concrete production classMemberDefinitionConstructor_c 
top::ClassMemberDefinition
	::=	mods::ModifiersOpt_NT
		id::Id_t  -- the name of the constructor
		-- parse the formal parameter declarations.
		'(' 
			pdl::ParameterDeclarationList 
		')'
		-- get the list of exceptions that this constructor is declared to throw
		tc::ThrowsClause
		cb::ConstructorBody { 
 top.ast_Class_Member_Dcl = class_constructor (mods.ast_Modifiers, id, pdl.ast_Formal_Params, tc.ast_Throws, cb.ast_Block);
}

concrete production constructorBody_c
top::ConstructorBody
	::=	'{' eci::ExplicitConstructorInvocation ss::BlockStatements '}' { 
 top.ast_Block = block (stmt_seq (stmt_constructor_invocation (eci.ast_Constructor_Invocation), ss.ast_Stmt));
}

concrete production constructorBodyNoInvocation_c
top::ConstructorBody
	::=	'{' ss::BlockStatements '}' {
 top.ast_Block = block (ss.ast_Stmt);
}

concrete production constructorBodyJustInvocation_c
top::ConstructorBody
	::=	'{' eci::ExplicitConstructorInvocation '}' { 
 top.ast_Block = block (stmt_constructor_invocation (eci.ast_Constructor_Invocation));
}

concrete production constructorBodyEmpty_c
top::ConstructorBody
	::=	'{' '}' { 
 top.ast_Block = empty_block ();
}

-- inner class
concrete production classMemberDefinitionInnerClass_c 
top::ClassMemberDefinition
	::=	cd::ClassDefinition { 
 top.ast_Class_Member_Dcl = inner_class (cd.ast_Class_Dcl);
}

-- inner interface
concrete production classMemberDefinitionInnerInterface_c 
top::ClassMemberDefinition
	::=	id::InterfaceDefinition { 
 top.ast_Class_Member_Dcl = inner_interface (id.ast_Interface_Dcl);
}

concrete production classMemberDefinitionMethod_c
top::ClassMemberDefinition
	::=	mods::ModifiersOpt_NT
		t::Type_NT
		md::MethodDeclarator
		-- get the list of exceptions that this method is declared to throw
		tc::ThrowsClause
		mb::MethodBody { 
 top.ast_Class_Member_Dcl = class_method (
			     case mb of
				methodBodyBlock_c (b) -> method_dcl_prod (method_header_declarator (mods.ast_Modifiers, t.ast_Type, md.ast_Method_Declarator, tc.ast_Throws), b.ast_Block) |
				methodBodyEmpty_c (_) -> method_dcl_no_body (method_header_declarator (mods.ast_Modifiers, t.ast_Type, md.ast_Method_Declarator, tc.ast_Throws))
			     end);
}

concrete production methodBodyBlock_c
top::MethodBody
	::=	b::BlockConcrete {
}

concrete production methodBodyEmpty_c
top::MethodBody
	::=	';' {
}

concrete production methodDeclarator_c
top::MethodDeclarator
	::=	id::Id_t  -- the name of the method
		-- parse the formal parameter declarations.
		'(' 
			params::ParameterDeclarationList 
		')' { 
 top.ast_Method_Declarator = method_declarator (id, params.ast_Formal_Params );
}

concrete production methodDeclaratorArray_c
top::MethodDeclarator
	::=	md::MethodDeclarator '[' ']' { 
 top.ast_Method_Declarator = method_declarator_array (md.ast_Method_Declarator);
}

concrete production classMemberDefinitionField_c 
top::ClassMemberDefinition
	::=	mods::ModifiersOpt_NT t::Type_NT v::VariableDeclarators ';' {
 top.ast_Class_Member_Dcl = class_field (field_dcl (mods.ast_Modifiers, t.ast_Type, v.ast_Var_Declarators));
}

-- "static { ... }" class initializer
concrete production classMemberDefinitionClassInitializer_c 
top::ClassMemberDefinition
	::=	'static' b::BlockConcrete { 
 top.ast_Class_Member_Dcl = class_static_initializer (b.ast_Block);
}

-- "{ ... }" instance initializer
concrete production classMemberDefinitionInstanceInitializer_c 
top::ClassMemberDefinition
	::=	b::BlockConcrete {
 top.ast_Class_Member_Dcl = class_block (b.ast_Block); 
}

-- empty
concrete production classMemberDefinitionEmpty_c 
top::ClassMemberDefinition
	::=	';' { 
 top.ast_Class_Member_Dcl = class_member_empty() ;
}

-- Possible interface members
concrete production interfaceMemberDefinitionsSnoc_c 
top::InterfaceMemberDefinitions
	::=	many::InterfaceMemberDefinitions one::InterfaceMemberDefinition { 
 top.ast_Interface_Member_Dcls = interface_member_dcls_snoc (many.ast_Interface_Member_Dcls, one.ast_Interface_Member_Dcl);
}

concrete production interfaceMemberDefinitionsEmpty_c 
top::InterfaceMemberDefinitions
	::=	{ 
 top.ast_Interface_Member_Dcls = interface_member_dcls_none ();
}

concrete production interfaceMemberDefinitionField_c 
top::InterfaceMemberDefinition
	::=	mods::ModifiersOpt_NT t::Type_NT v::VariableDeclarators ';' { 
 top.ast_Interface_Member_Dcl = interface_field (field_dcl (mods.ast_Modifiers, t.ast_Type, v.ast_Var_Declarators));
}

concrete production interfaceMemberDefinitionMethod_c
top::InterfaceMemberDefinition
	::=	mods::ModifiersOpt_NT
		t::Type_NT
		md::MethodDeclarator
		-- get the list of exceptions that this method is declared to throw
		tc::ThrowsClause
		';' { 
 top.ast_Interface_Member_Dcl = interface_method (method_header_declarator (mods.ast_Modifiers, t.ast_Type, md.ast_Method_Declarator, tc.ast_Throws)) ;
}

-- inner class
concrete production interfaceMemberDefinitionInnerClass_c 
top::InterfaceMemberDefinition
	::=	cd::ClassDefinition { 
 top.ast_Interface_Member_Dcl = interface_inner_class (cd.ast_Class_Dcl);
}

 -- inner interface
concrete production interfaceMemberDefinitionInnerInterface_c 
top::InterfaceMemberDefinition
	::=	id::InterfaceDefinition { 
 top.ast_Interface_Member_Dcl = interface_inner_interface (id.ast_Interface_Dcl);
}

concrete production interfaceMemberDefinitionEmpty_c 
top::InterfaceMemberDefinition
	::=	';' { 
 top.ast_Interface_Member_Dcl = interface_empty ();
}

concrete production expressionListOptSome_c 
top::ExpressionListOpt
	::=	es::ExpressionList { 
 top.ast_Exprs = es.ast_Exprs;
}

concrete production expressionListOptEmpty_c 
top::ExpressionListOpt
	::=	{ 
 top.ast_Exprs = exprs_none ();
}

-- This is a list of expressions.
concrete production expressionListSnoc_c 
top::ExpressionList
	::=	es::ExpressionList ',' e::Expression {
 top.ast_Exprs = exprs_snoc (es.ast_Exprs, e.ast_Expr);
}

concrete production expressionListOne_c 
top::ExpressionList
	::=	e::Expression { 
 top.ast_Exprs = exprs_one (e.ast_Expr);
}

-- Catch obvious constructor calls, but not the expr.super(...) calls
concrete production explicitConstructorInvocationThis_c 
top::ExplicitConstructorInvocation
	::=	'this' '(' args::ExpressionListOpt ')' ';' { 
 top.ast_Constructor_Invocation = this_constructor_invocation (args.ast_Exprs);
}

concrete production explicitConstructorInvocationSuper_c 
top::ExplicitConstructorInvocation
	::=	'super' '(' args::ExpressionListOpt ')' ';' { 
 top.ast_Constructor_Invocation = super_constructor_invocation (args.ast_Exprs);
}

concrete production explicitConstructorInvocationExpressionThis_c 
top::ExplicitConstructorInvocation
	::=	e1::PrimaryExpressionAndArrayCreation '.' 'this' '(' args::ExpressionListOpt ')' ';' { 
 top.ast_Constructor_Invocation = this_dot_constructor_invocation (e1.ast_Expr, args.ast_Exprs);
}

concrete production explicitConstructorInvocationExpressionSuper_c 
top::ExplicitConstructorInvocation
	::=	e1::PrimaryExpressionAndArrayCreation '.' 'super' '(' args::ExpressionListOpt ')' ';' { 
 top.ast_Constructor_Invocation = super_dot_constructor_invocation (e1.ast_Expr, args.ast_Exprs);
}

concrete production variableDeclaratorsSnoc_c 
top::VariableDeclarators
	::=	vds::VariableDeclarators ',' vd::VariableDeclarator { 
 top.ast_Var_Declarators = var_declarators_snoc (vds.ast_Var_Declarators, vd.ast_Var_Declarator);
}

concrete production variableDeclaratorsOne_c 
top::VariableDeclarators
	::=	vd::VariableDeclarator { 
 top.ast_Var_Declarators = var_declarators_one (vd.ast_Var_Declarator);
}

-- Declaration of a variable.  This can be a class/instance variable,
--    or a local variable in a method
--  It can also include possible initialization.
concrete production variableDeclarator_c 
top::VariableDeclarator
	::=	vd::VariableDeclaratorId {
 top.ast_Var_Declarator = var_declarator (vd.ast_Var_Declarator_Id);
}

concrete production variableDeclaratorInitializer_c 
top::VariableDeclarator
	::=	vd::VariableDeclaratorId '=' init::Initializer { 
 top.ast_Var_Declarator = var_declarator_init (vd.ast_Var_Declarator_Id, init.ast_Var_Init);
}

concrete production variableDeclaratorId_c 
top::VariableDeclaratorId
	::=	id::Id_t {
 top.ast_Var_Declarator_Id = var_declarator_id (id);
}

concrete production variableDeclaratorIdArray_c 
top::VariableDeclaratorId
	::=	vd::VariableDeclaratorId '[' ']' { 
 top.ast_Var_Declarator_Id = var_declarator_array (vd.ast_Var_Declarator_Id);
}

-- The two "things" that can initialize an array element are an expression
--   and another (nested) array initializer.
concrete production initializer_c 
top::Initializer
	::=	e::Expression { 
 top.ast_Var_Init = var_init_expr (e.ast_Expr) ;
}

concrete production initializerArray_c 
top::Initializer
	::=	ai::ArrayInitializer { 
 top.ast_Var_Init = var_init_array (ai.ast_Array_Init) ;
}

concrete production arrayInitializer_c 
top::ArrayInitializer
	::=	'{' inits::Initializers ',' '}' { 
 top.ast_Array_Init = array_init (inits.ast_Var_Inits);
}

concrete production arrayInitializerNoComma_c 
top::ArrayInitializer
	::=	'{' inits::Initializers '}' { 
 top.ast_Array_Init = array_init_no_comma (inits.ast_Var_Inits);
}

concrete production arrayInitializerOnlyComma_c 
top::ArrayInitializer
	::=	'{' ',' '}' { 
 top.ast_Array_Init = array_init_no_var_inits ();
}

concrete production arrayInitializerEmpty_c 
top::ArrayInitializer
	::=	'{' '}' { 
 top.ast_Array_Init = array_init_empty ();
}

concrete production initializersSnoc_c 
top::Initializers
	::=	many::Initializers ',' one::Initializer { 
 top.ast_Var_Inits = var_inits_snoc (many.ast_Var_Inits, one.ast_Var_Init);
}

concrete production initializersOne_c 
top::Initializers
	::=	one::Initializer { 
 top.ast_Var_Inits = var_inits_one (one.ast_Var_Init);
}

-- This is a list of exception classes that the method is declared to throw
concrete production throwsClause_c 
top::ThrowsClause
	::=	'throws' ns::Names { 
 top.ast_Throws = throws ( ns.ast_TypeNames ) ;
}

concrete production throwsClauseEmpty_c 
top::ThrowsClause
	::=	{ 
 top.ast_Throws = throws_none();
}

-- A list of formal parameters
concrete production parameterDeclarationListEmpty_c 
top::ParameterDeclarationList
	::=	{ 
 top.ast_Formal_Params = formal_params_none();
}

concrete production parameterDeclarationListSome_c 
top::ParameterDeclarationList
	::=	ps::ParameterDeclarations { 
 top.ast_Formal_Params = ps.ast_Formal_Params ;
}

concrete production parameterDeclarationsSnoc_c 
top::ParameterDeclarations
	::=	ps::ParameterDeclarations ',' pd::ParameterDeclaration { 
 top.ast_Formal_Params = formal_params_snoc (ps.ast_Formal_Params, pd.ast_Formal_Param);
}

concrete production parameterDeclarationsOne_c 
top::ParameterDeclarations
	::=	pd::ParameterDeclaration { 
 top.ast_Formal_Params = formal_params_one (pd.ast_Formal_Param);
}

-- A formal parameter.
concrete production parameterDeclaration_c 
top::ParameterDeclaration
	::=	t::Type_NT vd::VariableDeclaratorId {
 top.ast_Formal_Param = formal_param (t.ast_Type, vd.ast_Var_Declarator_Id );
}

concrete production parameterDeclarationFinal_c 
top::ParameterDeclaration
	::=	'final' t::Type_NT vd::VariableDeclaratorId {
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
top::BlockConcrete
	::=	'{' ss::BlockStatements '}' {
 top.ast_Block = block (ss.ast_Stmt);
}

concrete production blockEmpty_c 
top::BlockConcrete
	::=	'{' '}' {
 top.ast_Block = empty_block ();
}

concrete production blockStatementsSnoc_c 
top::BlockStatements
	::=	many::BlockStatements one::BlockStatement { 
 top.ast_Stmt = stmt_seq (many.ast_Stmt, one.ast_Stmt);
}

concrete production blockStatementsOne_c 
top::BlockStatements
	::=	one::BlockStatement { 
 top.ast_Stmt = one.ast_Stmt ;
}

concrete production statementDeclaration_c 
top::BlockStatement
	::=	dcl::Declaration ';' { 
 top.ast_Stmt = stmt_dcl (dcl.ast_Local_Var_Dcl) ;
}

concrete production statementClass_c 
top::BlockStatement
	-- class definition
	::=	cd::ClassDefinition { 
 top.ast_Stmt = block_stmt_class (cd.ast_Class_Dcl);
}

concrete production statementInterface_c 
top::BlockStatement
	-- interface definition
	::=	id::InterfaceDefinition { 
 top.ast_Stmt = block_stmt_interface (id.ast_Interface_Dcl);
}

concrete production blockStatement_c 
top::BlockStatement
	::=	s::Statement { 
 top.ast_Stmt = s.ast_Stmt ;
}

concrete production statementBlock_c 
top::Statement
	-- A list of statements in curly braces -- start a new scope
	::=	b::BlockConcrete { 
 top.ast_Stmt = stmt_block (b.ast_Block) ;
}

concrete production statementExpression_c 
top::Statement
	-- An expression statement.  This could be a method call,
	-- assignment statement, or any other expression evaluated for
	-- side-effects.
	::=	e::StatementExpression ';' {
 top.ast_Stmt = stmt_stmt_expr (e.ast_Stmt_Expr); 
}

concrete production statementLabel_c 
top::Statement
	-- Attach a label to the front of a statement
	::=	id::Id_t ':' s::Statement { 
 top.ast_Stmt = label_prod (id, s.ast_Stmt);
}

concrete production statementIfThenElse_c 
top::Statement
	-- If-else statement
	::=	t::If_t '(' e::Expression ')' s::Statement 'else' s2::Statement { 
 top.ast_Stmt = if_then_else (t, e.ast_Expr, s.ast_Stmt, s2.ast_Stmt);
}

concrete production statementIfThen_c 
top::Statement
	-- If- statement
	::=	t::If_t '(' e::Expression ')' s::Statement { 
 top.ast_Stmt = if_then (t, e.ast_Expr, s.ast_Stmt);
}

concrete production statementFor_c 
top::Statement
	-- For statement
	::=	'for'
			'('
				init::ForInit ';'	-- initializer
				cond::ForCond ';'	-- condition test
				iter::ForIter		-- updater
			')'
			body::Statement { -- statement to loop over
 top.ast_Stmt = for (init.ast_For_Init, cond.ast_For_Test, iter.ast_For_Update, body.ast_Stmt) ;
}

concrete production statementWhile_c 
top::Statement
	-- While statement
	::=	t::While_t '(' e::Expression ')' s::Statement { 
 top.ast_Stmt = while_prod (t, e.ast_Expr, s.ast_Stmt);
}

concrete production statementDo_c 
top::Statement
	-- do-while statement
	::=	'do' s::Statement t::While_t '(' e::Expression ')' ';' {  
 top.ast_Stmt = do (s.ast_Stmt, t, e.ast_Expr);
}

concrete production statementBreak_c 
top::Statement
	-- get out of a loop (or switch)
	::=	'break' id::Id_t ';' { 
 top.ast_Stmt = break_label (id);
}

concrete production statementBreakNoLabel_c 
top::Statement
	-- get out of a loop (or switch)
	::=	'break' ';' {  
 top.ast_Stmt = break_prod ();
}

concrete production statementContinue_c 
top::Statement
	-- get out of a loop (or switch)
	::=	'continue' id::Id_t ';' { 
 top.ast_Stmt = continue_label (id);
}

concrete production statementContinueNoLabel_c 
top::Statement
	-- get out of a loop (or switch)
	::=	'continue' ';' {  
 top.ast_Stmt = continue_prod ();
}

concrete production statementReturn_c 
top::Statement
	-- Return an expression
	::=	t::Return_t e::Expression ';' {
 top.ast_Stmt = return_expr (t, e.ast_Expr);
}

concrete production statementReturnNoExpression_c 
top::Statement
	-- Return
	::=	t::Return_t ';' { 
 top.ast_Stmt = return_statement (t);
}

concrete production statementSwitch_c 
top::Statement
	-- switch/case statement
	::=	'switch' '(' e::Expression ')' sg::SwitchBlock { 
 top.ast_Stmt = switch_prod (e.ast_Expr, sg.ast_Switch_Block);
}

-- an exception handler try/catch block
concrete production statementTry_c 
top::Statement
	::=	'try' b::BlockConcrete hs::Handlers fc::FinallyClause { 
 top.ast_Stmt = case fc of
			finallyClauseEmpty_c () -> try (b.ast_Block, hs.ast_Catches) |
			finallyClause_c (_, b1) -> try_finally (b.ast_Block, hs.ast_Catches, b1.ast_Block)
		end;
}

concrete production statementThrow_c 
top::Statement
	-- throw an exception
	::=	'throw' e::Expression ';' { 
 top.ast_Stmt = throw (e.ast_Expr);
}

concrete production statementSynchronized_c 
top::Statement
	-- synchronize a statement
	::=	'synchronized' '(' e::Expression ')' b::BlockConcrete {
 top.ast_Stmt = synchronized (e.ast_Expr, b.ast_Block);
}

concrete production statementAssert_c 
top::Statement
	::=	'assert' e::Expression ':' e2::Expression ';' { 
 top.ast_Stmt = assert_colon (e.ast_Expr, e2.ast_Expr);
}

concrete production statementAssertNoExpression_c 
top::Statement
	::=	'assert' e::Expression ';' { 
 top.ast_Stmt = assert (e.ast_Expr);
}

concrete production statementEmpty_c 
top::Statement
	-- empty statement
	::=	';' { 
 top.ast_Stmt = empty_stmt();
}

concrete production switchBlock_c
top::SwitchBlock
	::= '{' cgs::CaseGroups cs::Cases '}' { 
 top.ast_Switch_Block = switch_block (cgs.ast_Switch_Groups, cs.ast_Switch_Labels);
}

concrete production switchBlockNoLabels_c
top::SwitchBlock
	::= '{' cgs::CaseGroups '}' { 
 top.ast_Switch_Block = switch_block_no_labels (cgs.ast_Switch_Groups);
}

concrete production switchBlockJustLabels_c
top::SwitchBlock
	::= '{' cs::Cases '}' { 
 top.ast_Switch_Block = switch_block_no_groups (cs.ast_Switch_Labels);
}

concrete production switchBlockEmpty_c
top::SwitchBlock
	::= '{' '}' { 
 top.ast_Switch_Block = switch_block_empty ();
}

concrete production caseGroupsSnoc_c 
top::CaseGroups
	::=	many::CaseGroups one::CaseGroup { 
 top.ast_Switch_Groups = switch_groups_snoc (many.ast_Switch_Groups, one.ast_Switch_Group);
}

concrete production caseGroupsOne_c 
top::CaseGroups
	::=	one::CaseGroup { 
 top.ast_Switch_Groups = switch_groups_one (one.ast_Switch_Group);
}

concrete production caseGroup_c 
top::CaseGroup
	::=	cs::Cases ss::BlockStatements { 
 top.ast_Switch_Group = switch_group (cs.ast_Switch_Labels, ss.ast_Stmt);
}

concrete production casesSnoc_c 
top::Cases
	::=	many::Cases one::ACase { 
 top.ast_Switch_Labels = switch_labels_snoc (many.ast_Switch_Labels, one.ast_Switch_Label);
}

concrete production casesOne_c 
top::Cases
	::=	one::ACase { 
 top.ast_Switch_Labels = switch_labels_one (one.ast_Switch_Label);
}

concrete production aCase_c 
top::ACase
	::=	'case' e::Expression ':' { 
 top.ast_Switch_Label = switch_label (e.ast_Expr);
}

concrete production aCaseDefault_c 
top::ACase
	::=	'default' ':'	{ 
 top.ast_Switch_Label = switch_label_default ();
}

-- The initializer for a for loop
concrete production forInitDeclaration_c 
top::ForInit
		-- if it looks like a declaration, it is
	::=	dcl::Declaration { 
 top.ast_For_Init = for_init_dcl (dcl.ast_Local_Var_Dcl);
}

concrete production forInitExpressionList_c 
top::ForInit
		-- otherwise it could be an expression list...
	::=	es::StatementExpressionList { 
 top.ast_For_Init = for_init_some (es.ast_Stmt_Exprs);
}

concrete production forInitEmpty_c 
top::ForInit
	::=	{ 
 top.ast_For_Init = for_init_empty ();
}

concrete production forCond_c 
top::ForCond
	::=	expr::Expression { 
 top.ast_For_Test = for_test_one (expr.ast_Expr);
}

concrete production forCondEmpty_c 
top::ForCond
	::=	{ 
 top.ast_For_Test = for_test_none ();
}

concrete production forIter_c 
top::ForIter
	::=	es::StatementExpressionList { 
 top.ast_For_Update = for_update_some (es.ast_Stmt_Exprs);
}

concrete production forIterEmpty_c 
top::ForIter
	::=	{ 
 top.ast_For_Update = for_update_empty ();
}

concrete production statementExpressionsSnoc_c 
top::StatementExpressionList
	::=	many::StatementExpressionList ',' one::StatementExpression {
 top.ast_Stmt_Exprs = stmt_exprs_snoc (many.ast_Stmt_Exprs, one.ast_Stmt_Expr);
}

concrete production statementExpressionsOne_c 
top::StatementExpressionList
	::=	one::StatementExpression { 
 top.ast_Stmt_Exprs = stmt_exprs_one (one.ast_Stmt_Expr);
}

concrete production finallyClause_c 
top::FinallyClause
	::=	'finally' b::BlockConcrete {
}

concrete production finallyClauseEmpty_c 
top::FinallyClause
	::=	{
}

concrete production handlersSnoc_c 
top::Handlers
	::=	many::Handlers one::Handler { 
 top.ast_Catches = catches_snoc (many.ast_Catches, one.ast_Catch);
}

concrete production handlersEmpty_c 
top::Handlers
	::=	{ 
 top.ast_Catches = catches_none ();
}

-- an exception handler
concrete production handler_c 
top::Handler
	::=	'catch' '(' dcl::ParameterDeclaration ')' b::BlockConcrete { 
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
top::StatementExpression
	::=	e1::Expression {
 top.ast_Stmt_Expr = if e1.isStmtExpr
			then e1.ast_Stmt_Expr
			else error ("Parse error, expecting a statement");
}

concrete production assign_c
top::Expression
	::=	e1::Expression t::Eq_t e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> assign (l, t, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production plusAssign_c
top::Expression
	::=	e1::Expression '+=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> plus_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production minusAssign_c
top::Expression
	::=	e1::Expression '-=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> minus_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production mulAssign_c
top::Expression
	::=	e1::Expression '*=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> mul_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production divAssign_c
top::Expression
	::=	e1::Expression '/=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> div_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production modAssign_c
top::Expression
	::=	e1::Expression '%=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> mod_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production shiftRightAssign_c
top::Expression
	::=	e1::Expression '>>=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> rshift_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production unsignedShiftRightAssign_c
top::Expression
	::=	e1::Expression '>>>=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> urshift_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production shiftLeftAssign_c
top::Expression
	::=	e1::Expression '<<=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> lshift_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production andAssign_c
top::Expression
	::=	e1::Expression '&=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> and_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production xorAssign_c
top::Expression
	::=	e1::Expression '^=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> xor_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

concrete production orAssign_c
top::Expression
	::=	e1::Expression '|=' e2::Expression {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = case e1.ast_Expr of
                        expr_lhs (l) -> or_assign (l, e2.ast_Expr) |
                        e -> error ("Parse error, assignment to non-LHS")
                     end;
}

-- conditional test (level 12)
concrete production conditionalExpression_c 
top::Expression
	::=	e1::Expression '?' e2::Expression ':' e3::Expression {
 top.isStmtExpr = false;
 top.ast_Expr = conditional (e1.ast_Expr, e2.ast_Expr, e3.ast_Expr);
}

-- logical or (||)  (level 11)
concrete production logicalOrExpression_c 
top::Expression
	::=	e1::Expression '||' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = or_or (e1.ast_Expr, e2.ast_Expr);
}

-- logical and (&&)  (level 10)
concrete production logicalAndExpression_c 
top::Expression
	::=	e1::Expression '&&' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = and_and (e1.ast_Expr, e2.ast_Expr);
}

-- bitwise or non-short-circuiting or (|)  (level 9)
concrete production inclusiveOrExpression_c 
top::Expression
	::=	e1::Expression '|' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = or (e1.ast_Expr, e2.ast_Expr);
}

-- exclusive or (^)  (level 8)
concrete production exclusiveOrExpression_c 
top::Expression
	::=	e1::Expression '^' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = xor (e1.ast_Expr, e2.ast_Expr);
}

-- bitwise or non-short-circuiting and (&)  (level 7)
concrete production andExpression_c 
top::Expression
	::=	e1::Expression '&' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = and (e1.ast_Expr, e2.ast_Expr);
}

-- equality/inequality (==/!=) (level 6)
concrete production equalityExpression_c 
top::Expression
	::=	e1::Expression '==' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = eq (e1.ast_Expr, e2.ast_Expr);
}

concrete production inequalityExpression_c 
top::Expression
	::=	e1::Expression '!=' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = not_eq (e1.ast_Expr, e2.ast_Expr);
}

-- boolean relational expressions (level 5)
concrete production ltExpression_c 
top::Expression
	::=	e1::Expression '<' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = lt (e1.ast_Expr, e2.ast_Expr);
}

concrete production gtExpression_c 
top::Expression
	::=	e1::Expression '>' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = gt (e1.ast_Expr, e2.ast_Expr);
}

concrete production leExpression_c 
top::Expression
	::=	e1::Expression '<=' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = lteq (e1.ast_Expr, e2.ast_Expr);
}

concrete production geExpression_c 
top::Expression
	::=	e1::Expression '>=' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = gteq (e1.ast_Expr, e2.ast_Expr);
}

-- todo include this
concrete production instanceOf_c 
top::Expression
	::=	e1::Expression 'instanceof' t::ReferenceType { 
 top.isStmtExpr = false;
 top.ast_Expr = instanceof (e1.ast_Expr, t.ast_Reference_Type);
}

-- bit shift expressions (level 4)
concrete production shiftLeftExpression_c 
top::Expression
	::=	e1::Expression '<<' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = lshift (e1.ast_Expr, e2.ast_Expr);
}

concrete production shiftRightExpression_c 
top::Expression
	::=	e1::Expression '>>' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = rshift (e1.ast_Expr, e2.ast_Expr);
}

concrete production shiftRightUnsignedExpression_c 
top::Expression
	::=	e1::Expression '>>>' e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = urshift (e1.ast_Expr, e2.ast_Expr);
}

-- binary addition/subtraction (level 3)
concrete production plusExpression_c 
top::Expression
	::=	e1::Expression t::Plus_t e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = plus (e1.ast_Expr, t, e2.ast_Expr);
}

concrete production minusExpression_c 
top::Expression
	::=	e1::Expression t::Minus_t e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = minus (e1.ast_Expr, t, e2.ast_Expr);
}

-- multiplication/division/modulo (level 2)
concrete production mulExpression_c 
top::Expression
	::=	e1::Expression t::Mul_t e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = mul (e1.ast_Expr, t, e2.ast_Expr);
}

concrete production divExpression_c 
top::Expression
	::=	e1::Expression t::Div_t e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = div (e1.ast_Expr, t, e2.ast_Expr);
}

concrete production modExpression_c 
top::Expression
	::=	e1::Expression t::Mod_t e2::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = mod (e1.ast_Expr, t, e2.ast_Expr);
}

concrete production preIncExpression_c 
top::Expression
	::=	'++' e1::Expression { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = pre_inc (e1.ast_Expr);
}

concrete production preDecExpression_c 
top::Expression
	::=	'--' e1::Expression { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = pre_dec (e1.ast_Expr);
}

concrete production unaryPlusExpression_c 
top::Expression
	::=	'+' e1::Expression precedence = 140 { 
 top.isStmtExpr = false;
 top.ast_Expr = unary_plus (e1.ast_Expr);
}

concrete production unaryMinusExpression_c 
top::Expression
	::=	'-' e1::Expression precedence = 140 { 
 top.isStmtExpr = false;
 top.ast_Expr = unary_minus (e1.ast_Expr);
}

concrete production bitNotExpression_c 
top::Expression
	::=	'~' e1::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = comp (e1.ast_Expr);
}

concrete production logicalNotExpression_c 
top::Expression
	::=	'!' e1::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = not (e1.ast_Expr);
}

-- todo include this
concrete production primitiveTypeCastExpression_c
top::Expression
	::=	'(' t::PrimitiveType ds::DeclaratorBracketsOpt ')' e1::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = if ds.dims == 0
		then cast_primitive (t.ast_Primitive_Type, e1.ast_Expr)
		else cast_primitive_array (t.ast_Primitive_Type, ds.dims, e1.ast_Expr);
}

concrete production nameCastExpression_c
top::Expression
	::=	'(' e::Expression ')' e1::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = cast_simple (e.ast_Expr, e1.ast_Expr);
}

concrete production nameArraycastExpression_c
top::Expression
	::=	'(' n::NameConcrete ds::DeclaratorBrackets ')' e1::Expression { 
 top.isStmtExpr = false;
 top.ast_Expr = cast_name_array (n.ast_TypeName, ds.dims, e1.ast_Expr);
}

concrete production nameExpression_c
top::Expression
	::=	n::NameConcrete { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (lhs_name (n.ast_ExprName));
}

concrete production primaryExpression_c
top::Expression
	::=	e1::PrimaryExpressionAndArrayCreation {
 top.isStmtExpr = e1.isStmtExpr;
 top.ast_Expr = e1.ast_Expr;
 top.ast_Stmt_Expr = e1.ast_Stmt_Expr;
}

concrete production primaryExpressionAndArrayCreation_c
top::PrimaryExpressionAndArrayCreation
	::=	e1::PrimaryExpression { 
 top.isStmtExpr = e1.isStmtExpr;
 top.ast_Expr = e1.ast_Expr;
 top.ast_Stmt_Expr = e1.ast_Stmt_Expr;
}

concrete production newPrimitiveArrayExpression_c 
top::PrimaryExpressionAndArrayCreation
	::=	'new' t::PrimitiveType exprs::DeclaratorExpressions ds::DeclaratorBracketsOpt { 
 top.isStmtExpr = false;
 top.ast_Expr = new_array_no_init_primitive (t.ast_Primitive_Type, exprs.ast_Dim_Exprs, ds.dims);
}

concrete production newNameArrayExpression_c 
top::PrimaryExpressionAndArrayCreation
	::=	'new' n::NameConcrete exprs::DeclaratorExpressions ds::DeclaratorBracketsOpt { 
 top.isStmtExpr = false;
 top.ast_Expr = new_array_no_init_name (n.ast_TypeName, exprs.ast_Dim_Exprs, ds.dims);
}

concrete production newPrimitiveArrayExpressionInitializer_c 
top::PrimaryExpressionAndArrayCreation
	::=	'new' t::PrimitiveType ds::DeclaratorBrackets init::ArrayInitializer { 
 top.isStmtExpr = false;
 top.ast_Expr = new_array_init_primitive (t.ast_Primitive_Type, ds.dims, init.ast_Array_Init);
}

concrete production newNameArrayExpressionInitializer_c 
top::PrimaryExpressionAndArrayCreation
	::=	'new' n::NameConcrete ds::DeclaratorBrackets init::ArrayInitializer {
 top.isStmtExpr = false;
 top.ast_Expr = new_array_init_name (n.ast_TypeName, ds.dims, init.ast_Array_Init);
}

-- qualified names, array expressions, method invocation, post inc/dec
concrete production expressionFieldAccessExpression_c
top::PrimaryExpression
	::= 	e1::PrimaryExpressionAndArrayCreation '.' id::Id_t { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (expr_field_access (e1.ast_Expr, id));
}

concrete production superFieldAccessExpression_c
top::PrimaryExpression
	::= 	'super' '.' id::Id_t { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (super_field_access (id));
}

concrete production nameSuperFieldAccessExpression_c
top::PrimaryExpression
	::= 	n::NameConcrete '.' 'super' '.' id::Id_t { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (name_super_field_access (n.ast_TypeName, id));
}

concrete production methodCallExpression_c
top::PrimaryExpression
	::=	n::NameConcrete '(' args::ExpressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = method_call (n.ast_MethodName, args.ast_Exprs);
}

concrete production expressionMethodCallExpression_c
top::PrimaryExpression
	::= 	e1::PrimaryExpressionAndArrayCreation '.' id::Id_t '(' args::ExpressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = expr_method_call (e1.ast_Expr, id, args.ast_Exprs);
}

concrete production superMethodCallExpression_c
top::PrimaryExpression
	::= 	'super' '.' id::Id_t '(' args::ExpressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = super_method_call (id, args.ast_Exprs);
}

concrete production nameSuperMethodCallExpression_c
top::PrimaryExpression
	::= 	n::NameConcrete '.' 'super' '.' id::Id_t '(' args::ExpressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = name_super_method_call (n.ast_TypeName, id, args.ast_Exprs);
}

concrete production expressionThisExpression_c
top::PrimaryExpression
	::= 	n::NameConcrete '.' 'this' {  
 top.isStmtExpr = false;
 top.ast_Expr = name_dot_this (n.ast_TypeName);
}

concrete production expressionDotNewExpression_c
top::PrimaryExpression
	::= 	e1::PrimaryExpressionAndArrayCreation '.' 'new' id::Id_t '(' args::ExpressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_expr (e1.ast_Expr, id, args.ast_Exprs);
}

concrete production expressionDotNewBodyExpression_c
top::PrimaryExpression
	::= 	e1::PrimaryExpressionAndArrayCreation '.' 'new' id::Id_t '(' args::ExpressionListOpt ')' cb::ClassBlock { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_expr_body (e1.ast_Expr, id, args.ast_Exprs, cb.ast_Class_Body);
}

concrete production nameDotNewExpression_c
top::PrimaryExpression
	::= 	n::NameConcrete '.' 'new' id::Id_t '(' args::ExpressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_name (n.ast_TypeName, id, args.ast_Exprs);
}

concrete production nameDotNewBodyExpression_c
top::PrimaryExpression
	::= 	n::NameConcrete '.' 'new' id::Id_t '(' args::ExpressionListOpt ')' cb::ClassBlock { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_name_body (n.ast_TypeName, id, args.ast_Exprs, cb.ast_Class_Body);
}

concrete production arrayAccessExpression_c
top::PrimaryExpression
	::=	n::NameConcrete '[' e1::Expression ']' { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (array_access (n.ast_ExprName, e1.ast_Expr));
}

concrete production expressionArrayAccess_c
top::PrimaryExpression
	::= 	e1::PrimaryExpression '[' e2::Expression ']' { 
 top.isStmtExpr = false;
 top.ast_Expr = expr_lhs (array_access_general (e1.ast_Expr, e2.ast_Expr));
}

concrete production postIncExpression_c 
top::Expression
	::=	e1::Expression '++' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = post_inc (e1.ast_Expr);
}

concrete production postDecExpression_c 
top::Expression
	::=	e1::Expression '--' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = post_dec (e1.ast_Expr);
} 

concrete production trueConstExpression_c
top::PrimaryExpression
	::=	'true' { 
 top.isStmtExpr = false;
 top.ast_Expr = true_const();
}

concrete production falseConstExpression_c
top::PrimaryExpression
	::=	'false' { 
 top.isStmtExpr = false;
 top.ast_Expr = false_const();
}

concrete production nullConstExpression_c
top::PrimaryExpression
	::=	'null' { 
 top.isStmtExpr = false;
 top.ast_Expr = null_const();
}

concrete production thisExpression_c
top::PrimaryExpression
	::=	'this' { 
 top.isStmtExpr = false;
 top.ast_Expr = this();
}

concrete production parenExpression_c
top::PrimaryExpression
	::=	'(' e1::Expression ')' {
 top.isStmtExpr = false;
 top.ast_Expr = e1.ast_Expr;
}

-- look for int.class and int[].class
concrete production primitiveTypeDotClassExpression_c
top::PrimaryExpression
	::=	t::PrimitiveType '.' 'class' { 
 top.isStmtExpr = false;
 top.ast_Expr = primitive_dot_class (t.ast_Primitive_Type);
}

concrete production voidDotClassExpression_c
top::PrimaryExpression
	::=	'void' '.' 'class' { 
 top.isStmtExpr = false;
 top.ast_Expr = void_dot_class ();
}

concrete production nameDotClassExpression_c
top::PrimaryExpression
	::=	n::NameConcrete '.' 'class' { 
 top.isStmtExpr = false;
 top.ast_Expr = name_dot_class (n.ast_TypeName);
}

concrete production arrayDotClassExpression_c
top::PrimaryExpression
	::=	t::ArrayType '.' 'class' { 
 top.isStmtExpr = false;
 top.ast_Expr = array_dot_class (t.ast_Array_Type);
}

-- Match a, a.b.c refs, a.b.c(...) refs, a.b.c[], a.b.c[].class,
--   and a.b.c.class refs.  Also this(...) and super(...).  Match
--   this or super.

concrete production newClassExpression_c 
top::PrimaryExpression
	::=	'new' n::NameConcrete '(' args::ExpressionListOpt ')' { 
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class (n.ast_TypeName, args.ast_Exprs);
}

concrete production newClassBodyExpression_c 
top::PrimaryExpression
	::=	'new' n::NameConcrete '(' args::ExpressionListOpt ')' cb::ClassBlock {
 top.isStmtExpr = true;
 top.ast_Expr = expr_stmt_expr (top.ast_Stmt_Expr);
 top.ast_Stmt_Expr = new_class_body (n.ast_TypeName, args.ast_Exprs, cb.ast_Class_Body);
}

concrete production declaratorExpressionsSnoc_c 
top::DeclaratorExpressions
	::=	many::DeclaratorExpressions '[' e1::Expression ']' { 
 top.ast_Dim_Exprs = dim_exprs_snoc (many.ast_Dim_Exprs, e1.ast_Expr);
}

concrete production declaratorExpressionsOne_c 
top::DeclaratorExpressions
	::=	'[' e1::Expression ']' { 
 top.ast_Dim_Exprs = dim_exprs_one (e1.ast_Expr);
}

concrete production intConstExpression_c 
top::PrimaryExpression
	::=	t::Intconst_t { 
 top.isStmtExpr = false;
 top.ast_Expr = int_const(t.lexeme);
}

concrete production charConstExpression_c 
top::PrimaryExpression
	::=	t::Charconst_t { 
 top.isStmtExpr = false;
 top.ast_Expr = char_const(t.lexeme);
}

concrete production floatConstExpression_c 
top::PrimaryExpression
	::=	t::Floatconst_t { 
 top.isStmtExpr = false;
 top.ast_Expr = float_const(t.lexeme);
}

concrete production stringConstExpression_c 
top::PrimaryExpression
	::=	t::Stringconst_t { 
 top.isStmtExpr = false;
 top.ast_Expr = string_const(t.lexeme);
}
