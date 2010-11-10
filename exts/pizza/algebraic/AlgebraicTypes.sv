grammar edu:umn:cs:melt:ableJ14:exts:pizza:algebraic ;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;

terminal algebraicTerm        'algebraic'    dominates {Id_t};

-- Concrete Syntax
--------------------------------------------------

concrete production algebraic_class_dcl_c
cdcl::classDefinition ::= mods::modifiersOpt 'algebraic' 'class' id::Id_t cb::classBlock {
 cdcl.ast_Class_Dcl = algebraic_class_dcl (mods.ast_Modifiers, id, getTypeName ("Object"), type_names_none (), cb.ast_Class_Body ) ;
}

concrete production class_case_dcl_c
cdcl::classMemberDefinition ::= 'case' id::Id_t ';'  {
  cdcl.ast_Class_Member_Dcl = class_case_dcl (id) ;
}

concrete production class_case_dcl_variant_c
cdcl::classMemberDefinition ::= 'case' id::Id_t '(' dcls::Case_parameters_c ')' ';'  {
  cdcl.ast_Class_Member_Dcl = class_case_dcl_variant (id, dcls.ast_CaseParams) ;
}

nonterminal Case_parameters_c ;
synthesized attribute ast_CaseParams :: CaseParams occurs on Case_parameters_c ;

concrete production case_params_one_c
vdcls::Case_parameters_c ::= t::type  { 
  vdcls.ast_CaseParams = case_params_one (t.ast_Type); 
}

concrete production case_params_cons_c
vdcls::Case_parameters_c ::= t::type ',' vdclstail::Case_parameters_c { 
  vdcls.ast_CaseParams = case_params_cons (t.ast_Type, vdclstail.ast_CaseParams); 
}

-- Abstract Syntax
--------------------------------------------------

synthesized attribute class_member_dcls :: Class_Member_Dcls occurs on Class_Body ;

abstract production algebraic_class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.pp = mods.pp ++ "algebraic class " ++ cname.lexeme ++ " extends " ++ parent.pp ++ 
		(case inters'' of type_names_none () -> "" | _ -> " implements " ++ inters.pp end) ++ " " ++ cb.pp;
 forwards to getClassDclTree ([algebraic_case_class_dcl (mods'', cname,  parent'', inters'', newClassBodyWithTag'' )] ++ cb.caseClasses);

 cb.datatypeClassName = cname.lexeme;

 local attribute newClassBodyWithTag :: Class_Body ;
 newClassBodyWithTag = class_body (class_member_dcls_snoc (cb.class_member_dcls, tag_dcl''));

 -- int tag;
 local attribute tag_dcl :: Class_Member_Dcl ;
 tag_dcl = class_field (field_dcl (
				modifiers_none(), 
				primitive_type (int_type()) ,
				var_declarators_one (var_declarator (var_declarator_id (terminal (Id_t, "tag")))))) ;
}

abstract production algebraic_case_class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
  cdcl.pp = mods.pp ++ "algebraic class " ++ cname.lexeme ++ " extends " ++ parent.pp ++ 
		(case inters'' of type_names_none () -> "" | _ -> " implements " ++ inters.pp end) ++ cb.pp;
  cdcl.basepp = mods.basepp ++ "class " ++ cname.lexeme ++ " extends " ++ parent.basepp ++ 
		(case inters'' of type_names_none () -> "" | _ -> " implements " ++ inters.basepp end) ++ " " ++ cb.basepp;

  -- check for duplicate pattern names in cb.caseReps
  cdcl.errors := mods.errors ++ inters.errors ++ cb.errors ;

  cdcl.type_defs = [ envItem (fqn.qualifiedName,
				fqn, 
				classDcl (fqn,
					  classTypeRepDefs ( algebraic_class_type_rep_defs (cname.lexeme, fqn.qualifiedName, mods.modlist, parentResolvedName,
												inters.fullyQualifiedNames, cb.field_defs, cb.method_defs, cb.constructor_defs,
												cb.inner_type_defs, cb.caseReps)))) ];
--  cdcl.type_defs = error ("casereps are " ++ unparseCaseReps (cb.caseReps));

  local attribute fqn :: FullyQualifiedName;
  fqn = getQualifiedFQN (cdcl.qualifiersSoFar, cname.lexeme);

  local attribute thisClassType :: TypeRep;
  thisClassType = retrieveClass (fqn, cdcl.type_env);

  cdcl.classtyperep = thisClassType.classtyperep;

  local attribute parentResolvedName :: FullyQualifiedName;
  parentResolvedName = case parent.resolvedPackageOrTypeName of
			fully_qualified_name_none () -> parent.fullyQualifiedName |
			fully_qualified_name_unknown () -> parent.fullyQualifiedName |
			_ -> parent.resolvedPackageOrTypeName
		       end;

  cb.enclosingType = thisClassType;
  cb.type_env = thisClassType.classtyperep.innerTypes ++ cdcl.type_env;

  cb.qualifiersSoFar = fqn;
}

function getClassDclTree
Class_Dcl ::= cds::[Class_Dcl] {
 return if null (cds)
	then class_dcl_none ()
	else class_dcl_seq (head (cds), getClassDclTree (tail (cds)));
}

-- Algebraic Class Type
--------------------------------------------------

attribute caseReps occurs on ClassTypeRepDefs, ClassTypeRep ;

abstract production algebraic_class_type_rep_defs
ct::ClassTypeRepDefs ::= cname::String qualifiedName_::String modlist_::[ Modifier ] defs_superClass_::FullyQualifiedName defs_interfaces_::[ FullyQualifiedName ] 
			defs_fields_::[ EnvItem ] defs_methods_::[ EnvItem ] defs_constructors_::[ EnvItem ] defs_innerTypes_::[ EnvItem ] caseReps_::[ CaseRep ] {
 ct.unparse = "algebraic_class_type_rep_defs (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ defs_superClass_.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (defs_interfaces_) ++ ", " ++ unparseEnvItems (defs_fields_) ++ ", " ++ unparseEnvItems (defs_methods_) ++ ", " ++ 
		unparseEnvItems (defs_constructors_) ++ ", " ++ unparseEnvItems (defs_innerTypes_) ++ ", " ++ unparseCaseReps (caseReps_) ++ ")";

 ct.caseReps = caseReps_;
 forwards to class_type_rep_defs (cname, qualifiedName_, modlist_, defs_superClass_, 
					defs_interfaces_, defs_fields_, defs_methods_, defs_constructors_, defs_innerTypes_);
}

abstract production algebraic_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] caseReps_::[ CaseRep ] environment::[ ScopeEnv ] {
 ct.unparse = "algebraic_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ", " ++ unparseCaseReps (caseReps_) ++ ")";

 ct.caseReps = caseReps_;
 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

aspect production convert_type_rep
top::ConvertedTypeRep ::= old::TypeRep environment::[ ScopeEnv ] {

 convertedTypeRep <- 	
		case new (old) of
		  classTypeRepDefs (algebraic_class_type_rep_defs (id, qualifiedName_, superClass_, modlist_, interfaces_, 
									fields_, methods_, constructors_, innerTypes_, caseReps_))
		->  
		[ classTypeRep (algebraic_class_type_rep (id, qualifiedName_, superClass_, modlist_, interfaces_, 
									fields_, methods_, constructors_, innerTypes_, convertCaseReps (caseReps_, environment), environment)) ] |
		  _
		->
		[ :: TypeRep ]
		end;

}

function convertCaseReps
[ CaseRep ] ::= oldCaseReps::[ CaseRep ] environment::[ ScopeEnv ] {

 return if null (oldCaseReps)
	then []
	else newCaseRep'' :: convertCaseReps (tail (oldCaseReps), environment);

 local attribute oldCaseRep :: CaseRep;
 oldCaseRep = head (oldCaseReps);

 local attribute newCaseRep :: CaseRep;
 newCaseRep = case_rep (oldCaseRep.caseName, oldCaseRep.tag, convertCaseParams (oldCaseRep.variant_defs, environment));
}

function convertCaseParams
[ CaseParam ] ::= caseParams::[ CaseParam ] environment::[ ScopeEnv ] {

 return if null (caseParams)
	then []
	else newCaseParam'' :: convertCaseParams (tail (caseParams), environment);

 local attribute oldCaseParam :: CaseParam;
 oldCaseParam = head (caseParams);

 local attribute newCaseParam :: CaseParam;
 newCaseParam = case_param (oldCaseParam.name_id, convert_type_rep (oldCaseParam.typerep, environment).typerep);
}

aspect production unknown_class_type_rep
t::ClassTypeRep ::= {
 t.caseReps = [];
}

function unparseCaseReps
String ::= cases_::[ CaseRep ] {
 return "[" ++ unparseCaseRepsHelper (cases_) ++ "]";
}

function unparseCaseRepsHelper
String ::= cases_::[ CaseRep ] {
 return if null (cases_)
	then ""
	else ((head (cases_)).unparse ++ (if null (tail (cases_)) then "" else (", " ++ unparseCaseRepsHelper (tail (cases_)))));
}

-- Case Member Dcl
---------------------------------------------------

synthesized attribute caseClasses :: [Class_Dcl] occurs on Class_Body, Class_Member_Dcls, Class_Member_Dcl;
autocopy attribute datatypeClassName :: String;
attribute datatypeClassName occurs on Class_Body, Class_Member_Dcls, Class_Member_Dcl;

abstract production class_case_dcl
cdcl::Class_Member_Dcl ::= id::Id_t {
 cdcl.pp = "case " ++ id.lexeme ++ ";" ;

 forwards to class_field (field_dcl (
				modifiers_snoc (modifiers_one (final ()), static_mod ()),
				primitive_type (int_type ()) ,
				var_declarators_one (var_declarator_init (var_declarator_id (terminal (Id_t, id.lexeme ++ "_tag")),
									  var_init_expr (int_const (toString (tag_value))))))) ;
 local attribute tag_value :: Integer ;
 tag_value = genInt() ;

 cdcl.caseReps = [case_rep (id.lexeme, tag_value, [] )] ;

--class Nil extends List {
--	Nil () {
--		this.tag = Nil_tag;
--	}
--}

 cdcl.caseClasses = [class_dcl (
			modifiers_none (), 
			id,  
			getTypeName (cdcl.datatypeClassName), 
			type_names_none (),
			class_body (class_member_dcls_one (
				class_constructor (modifiers_none (), id, formal_params_none (), throws_none (),
					block (stmt_stmt_expr (assign (
						expr_field_access (this (), terminal (Id_t, "tag")), 
						terminal (Eq_t, "="),
						expr_lhs (lhs_name (qualified_expr_name (simple_ambiguous_name (terminal (Id_t, cdcl.datatypeClassName)), 
													terminal (Id_t, id.lexeme ++ "_tag")))))))))))];
}

abstract production class_case_dcl_variant
cdcl::Class_Member_Dcl ::= id::Id_t  dcls::CaseParams {
 cdcl.pp = "case " ++ id.lexeme ++ " ( " ++ dcls.pp ++ " ) ;" ;

 forwards to class_field (field_dcl (
				modifiers_snoc (modifiers_one (final ()), static_mod ()),
				primitive_type (int_type()) ,
				var_declarators_one (var_declarator_init (var_declarator_id (terminal (Id_t, id.lexeme ++ "_tag")),
									  var_init_expr (int_const (toString (tag_value))))))) ;
 local attribute tag_value :: Integer ;
 tag_value = genInt() ;

 cdcl.caseReps = [case_rep (id.lexeme, tag_value, dcls.variant_defs )] ;

--class Cons extends List {
--	char head;
--	List tail;
--	Cons (char head, List tail) {
--		this.tag = Cons_tag;
--		this.head = head;
--		this.tail = tail;
--	}
--}

 cdcl.caseClasses = [class_dcl (
			modifiers_none (), 
			id,  
			getTypeName (cdcl.datatypeClassName),
			type_names_none (), 
			class_body (class_member_dcls_snoc (
				dcls.fieldDeclarations,
				class_constructor (modifiers_none (), id, dcls.constructorParams, throws_none (),
					block (stmt_seq (
						dcls.fieldDefinitions,
						stmt_stmt_expr (assign (
									expr_field_access (this (), terminal (Id_t, "tag")), 
									terminal (Eq_t, "="),
									expr_lhs (lhs_name (qualified_expr_name (simple_ambiguous_name(terminal (Id_t,cdcl.datatypeClassName)),
																terminal (Id_t,id.lexeme++ "_tag"))))))))))))];

 dcls.variantCount = 1;
}

--------------------------------------
-- Case variant decls
--------------------------------------

nonterminal CaseParams with pp, errors, type_env, file_name, pp_indent,
		availableLocalTypes, availableImportedSingleTypes, availableCurrentPackageTypes, availableImportedOnDemandTypes, 
		thisPackage, qualifiersSoFar, availableFullyQualifiedTypes;

synthesized attribute constructorParams :: Formal_Params 	occurs on CaseParams ;
synthesized attribute fieldDeclarations :: Class_Member_Dcls 	occurs on CaseParams ;
synthesized attribute fieldDefinitions  :: Stmt 		occurs on CaseParams ;
inherited attribute variantCount        :: Integer 		occurs on CaseParams ;

abstract production case_params_one
vdcls::CaseParams ::= t::Type {
 vdcls.pp = t.pp;
 vdcls.errors := t.errors;

 local attribute fieldName :: String;
 fieldName = t.typerep.eqName ++ "__field__" ++ toString (vdcls.variantCount);

 local attribute id :: Id_t;
 id = terminal (Id_t, fieldName);

 vdcls.constructorParams = formal_params_one (formal_param (t'', var_declarator_id (id)));
 vdcls.variant_defs = [ case_param (id, t.resolvedTypeRep ) ] ;
 vdcls.fieldDeclarations = class_member_dcls_one (class_field (field_dcl (
									modifiers_none(), 
									t'',
									var_declarators_one (var_declarator (var_declarator_id (id))))));

 vdcls.fieldDefinitions = stmt_stmt_expr (
				assign (
					expr_field_access (this (), id), 
					terminal (Eq_t, "="),
					getExpr (id.lexeme)));
}

abstract production case_params_cons
vdcls::CaseParams ::= t::Type vdclstail::CaseParams {
 vdcls.pp = t.pp ++ ", " ++ vdclstail.pp ;
 vdcls.errors := t.errors ++ vdclstail.errors;

 local attribute fieldName :: String;
 fieldName = t.typerep.eqName ++ "__field__" ++ toString (vdcls.variantCount);

 local attribute id :: Id_t;
 id = terminal (Id_t, fieldName);

 vdcls.constructorParams = formal_params_cons (formal_param (t'', var_declarator_id (id)), vdclstail.constructorParams);
 vdcls.variant_defs = [ case_param (id, t.resolvedTypeRep ) ] ++ vdclstail.variant_defs ;
 vdcls.fieldDeclarations = class_member_dcls_snoc (	vdclstail.fieldDeclarations,
							class_field (field_dcl (
									modifiers_none(), 
									t'',
									var_declarators_one (var_declarator (var_declarator_id (id))))));

 vdcls.fieldDefinitions = stmt_seq (
				stmt_stmt_expr (
					assign (
						expr_field_access (this (), id), 
						terminal (Eq_t, "="),
						getExpr (id.lexeme))), 
				vdclstail.fieldDefinitions);

 vdclstail.variantCount = vdcls.variantCount + 1;
}


-- CaseRep
----------

nonterminal CaseRep with caseName, tag, variant_defs, unparse ;
synthesized attribute caseName :: String;
synthesized attribute tag :: Integer ;
synthesized attribute caseReps :: [ CaseRep ] occurs on Class_Body, Class_Member_Dcl, Class_Member_Dcls ;
synthesized attribute variant_defs :: [ CaseParam ] occurs on CaseParams ;

abstract production case_rep
cr::CaseRep ::= n::String t::Integer vdcls::[ CaseParam ] {
 cr.caseName = n; 
 cr.tag = t;
 cr.variant_defs = vdcls ;
 cr.unparse = "case_rep (\"" ++ n ++ "\", " ++ toString (t) ++ ", " ++ unparseCaseParams (vdcls) ++ ")";
}

nonterminal CaseParam with name_id, typerep, unparse ;
synthesized attribute name_id :: Id_t;

abstract production case_param
v::CaseParam ::= id::Id_t t::TypeRep { 
 v.name_id = id'' ;  
 v.typerep = t'' ;
 v.unparse = "case_param (terminal (Id_t, \"" ++ id.lexeme ++ "\", " ++ t.unparse ++ ")";
}

abstract production error_case_param
v::CaseParam ::= {
 v.name_id = terminal (Id_t, "ErrorVariant");
 v.typerep = unknownTypeRep ();
 v.unparse = "error_case_param ()";
}

function lookupPattern
[CaseRep] ::= n::String crs::[CaseRep] {
 return if null (crs)
	then []
	else if (head (crs)).caseName == n
		then [head (crs)]
		else lookupPattern (n, tail (crs));
}

function unparseCaseParams
String ::= cases_::[ CaseParam ] {
 return "[" ++ unparseCaseParamsHelper (cases_) ++ "]";
}

function unparseCaseParamsHelper
String ::= cases_::[ CaseParam ] {
 return if null (cases_)
	then ""
	else ((head (cases_)).unparse ++ (if null (tail (cases_)) then "" else (", " ++ unparseCaseParamsHelper (tail (cases_)))));
}

-- Aspects
----------

aspect production class_body
cb::Class_Body ::= dcls::Class_Member_Dcls {
 cb.class_member_dcls = dcls'' ;
 cb.caseClasses = dcls.caseClasses;
 cb.caseReps = dcls.caseReps;
}

aspect production class_member_dcls_none
cdcls::Class_Member_Dcls ::= {
 cdcls.caseClasses = [];
 cdcls.caseReps = [];
}

aspect production class_member_dcls_snoc
cdcls::Class_Member_Dcls ::= cdcls1::Class_Member_Dcls cdcl::Class_Member_Dcl {
 cdcls.caseClasses = cdcls1.caseClasses ++ cdcl.caseClasses;
 cdcls.caseReps = cdcls1.caseReps ++ cdcl.caseReps;
}

aspect production class_member_dcls_one
cdcls::Class_Member_Dcls ::= cdcl::Class_Member_Dcl {
 cdcls.caseClasses = cdcl.caseClasses;
 cdcls.caseReps = cdcl.caseReps;
}

aspect production class_member_dcl_seq
cdcl::Class_Member_Dcl ::= cdcl1::Class_Member_Dcl cdcl2::Class_Member_Dcl {
 cdcl.caseClasses = cdcl1.caseClasses ++ cdcl2.caseClasses;
 cdcl.caseReps = cdcl1.caseReps ++ cdcl2.caseReps;
}

aspect production class_member_empty
cdcl::Class_Member_Dcl ::= {
 cdcl.caseClasses = [];
 cdcl.caseReps = [];
}

aspect production inner_class
cdcl::Class_Member_Dcl ::= cd::Class_Dcl {
 cdcl.caseClasses = [];
 cdcl.caseReps = [];
}

aspect production inner_interface
cdcl::Class_Member_Dcl ::= id::Interface_Dcl {
 cdcl.caseClasses = [];
 cdcl.caseReps = [];
}

aspect production class_block
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.caseClasses = [];
 cdcl.caseReps = [];
}

aspect production class_static_initializer
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.caseClasses = [];
 cdcl.caseReps = [];
}

aspect production class_field
cdcl::Class_Member_Dcl ::= f::Field_Dcl {
 cdcl.caseClasses = [];
 cdcl.caseReps = [];
}

aspect production class_method
cdcl::Class_Member_Dcl ::= mdcl::Method_Dcl {
 cdcl.caseClasses = [];
 cdcl.caseReps = [];
}

aspect production class_constructor
cdcl::Class_Member_Dcl ::= mods::Modifiers id::Id_t fps::Formal_Params thr::Throws cb::Block {
 cdcl.caseClasses = [];
 cdcl.caseReps = [];
}

-- Formal Params Cons (not snoc)

abstract production formal_params_cons
fps::Formal_Params ::= fp::Formal_Param fps1::Formal_Params {
  fps.pp = fp.pp ++ ", " ++ fps1.pp;
  fps.basepp = fp.basepp ++ ", " ++ fps1.basepp;
  fps.defs = fp.defs ++ fps1.defs;
  fps.errors := fp.errors ++ fps1.errors;
  fps.param_types = [fp.typerep] ++ fps1.param_types;

  fps.neededImportedSingleTypes = fp.neededImportedSingleTypes ++ fps1.neededImportedSingleTypes;
  fps.neededCurrentPackageTypes = fp.neededCurrentPackageTypes ++ fps1.neededCurrentPackageTypes;
  fps.neededImportedOnDemandTypes = fp.neededImportedOnDemandTypes ++ fps1.neededImportedOnDemandTypes;
  fps.neededFullyQualifiedTypes = fp.neededFullyQualifiedTypes ++ fps1.neededFullyQualifiedTypes;
  fps.resolvedTypeReps = [ fp.resolvedTypeRep ] ++ fps1.resolvedTypeReps;
}
