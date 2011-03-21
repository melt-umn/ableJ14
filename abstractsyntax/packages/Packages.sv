grammar edu:umn:cs:melt:ableJ14:abstractsyntax:packages;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals;
import core;

-- Imported Types and Current Package Types
-------------------------------------------

synthesized attribute localTypes :: [ String ] occurs on Type_Dcls, Type_Dcl, Class_Dcl, Interface_Dcl, Stmt, Class_Body, Class_Member_Dcl, Class_Member_Dcls,
							 Interface_Member_Dcl, Interface_Member_Dcls;
synthesized attribute onDemandImports :: [ FullyQualifiedName ] occurs on Root, Import_Dcls, Import_Dcl;
synthesized attribute singleImports :: [ FullyQualifiedName ] occurs on Root, Import_Dcls, Import_Dcl;

aspect production compilation_unit
r::Root ::= pd::Package_Dcl ids::Import_Dcls tds::Type_Dcls {
  r.onDemandImports = ids.onDemandImports ++ 
			[ getQualifiedFQN (getSimpleFQN ("java"), "lang") ];

  r.singleImports = ids.singleImports;
}

aspect production import_dcls_none
idcls::Import_Dcls ::= {
  idcls.onDemandImports = [];
  idcls.singleImports = [];
}

aspect production import_dcls_snoc
idcls::Import_Dcls ::= idcls1::Import_Dcls idcl::Import_Dcl {
  idcls.onDemandImports = idcls1.onDemandImports ++ idcl.onDemandImports;
  idcls.singleImports = idcls1.singleImports ++ idcl.singleImports;
}

aspect production import_dcl
idcl::Import_Dcl ::= t::Import_t n::TypeName {
  idcl.onDemandImports = [];
  idcl.singleImports = [ n.fullyQualifiedName ];
}

aspect production import_dcl_on_demand
idcl::Import_Dcl ::= n::PackageOrTypeName {
  idcl.onDemandImports = [ n.fullyQualifiedName ];
  idcl.singleImports = [];
}

aspect production type_dcls_snoc
tdcls::Type_Dcls ::= tdcls1::Type_Dcls tdcl::Type_Dcl {
  tdcls.localTypes = tdcls1.localTypes ++ tdcl.localTypes;
}

aspect production type_class_dcl
td::Type_Dcl ::= cdcl::Class_Dcl {
  td.localTypes = cdcl.localTypes;
}

aspect production type_interface_dcl
td::Type_Dcl ::= idcl::Interface_Dcl {
  td.localTypes = idcl.localTypes;
}

aspect production class_dcl_seq
cdcl::Class_Dcl ::= cdcl1::Class_Dcl cdcl2::Class_Dcl {
 cdcl.localTypes = cdcl1.localTypes ++ cdcl2.localTypes;
}

aspect production class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
  cdcl.localTypes = [ cname.lexeme ];
}

aspect production class_body
cb::Class_Body ::= dcls::Class_Member_Dcls {
 cb.localTypes = dcls.localTypes;
}

aspect production class_member_dcls_snoc
cdcls::Class_Member_Dcls ::= cdcls1::Class_Member_Dcls cdcl::Class_Member_Dcl {
 cdcls.localTypes = cdcls1.localTypes ++ cdcl.localTypes;
}

aspect production class_member_dcls_one
cdcls::Class_Member_Dcls ::= cdcl::Class_Member_Dcl {
 cdcls.localTypes = cdcl.localTypes;
}

aspect production class_member_dcl_seq
cdcl::Class_Member_Dcl ::= cdcl1::Class_Member_Dcl cdcl2::Class_Member_Dcl {
 cdcl.localTypes = cdcl1.localTypes ++ cdcl2.localTypes;
}

aspect production inner_class
cdcl::Class_Member_Dcl ::= cd::Class_Dcl {
 cdcl.localTypes = cd.localTypes;
}

aspect production inner_interface
cdcl::Class_Member_Dcl ::= id::Interface_Dcl {
 cdcl.localTypes = id.localTypes;
}

aspect production interface_dcl
idcl::Interface_Dcl ::= mods::Modifiers iname::Id_t inters::TypeNames dcls::Interface_Member_Dcls {
  idcl.localTypes = [ iname.lexeme ];
}

aspect production interface_member_dcls_snoc
idcls::Interface_Member_Dcls ::= idcls1::Interface_Member_Dcls idcl::Interface_Member_Dcl {
 idcls.localTypes = idcls1.localTypes ++ idcl.localTypes;
}

aspect production interface_inner_class
idcl::Interface_Member_Dcl ::= cd::Class_Dcl {
 idcl.localTypes = cd.localTypes;
}

aspect production interface_inner_interface
idcl::Interface_Member_Dcl ::= id::Interface_Dcl {
 idcl.localTypes = id.localTypes;
}

aspect production stmt_seq
seq::Stmt ::= stmt1::Stmt stmt2::Stmt {
 seq.localTypes = stmt1.localTypes ++ stmt2.localTypes;
}

aspect production block_stmt_class
s::Stmt ::= cdcl::Class_Dcl {
 s.localTypes = cdcl.localTypes;
}

aspect production block_stmt_interface
s::Stmt ::= idcl::Interface_Dcl {
 s.localTypes = idcl.localTypes;
}

-- other aspects in Aspects.sv


-- Available Types
------------------------------------------------------

autocopy attribute availableLocalTypes :: [ String ];
attribute availableLocalTypes occurs on	                PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs;

autocopy attribute availableImportedSingleTypes :: [ LFQN ];
attribute availableImportedSingleTypes occurs on 	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

autocopy attribute availableCurrentPackageTypes :: [ LFQN ];
attribute availableCurrentPackageTypes occurs on 	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

autocopy attribute availableImportedOnDemandTypes :: [ LFQN ];
attribute availableImportedOnDemandTypes occurs on 	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

aspect production compilation_unit
r::Root ::= pd::Package_Dcl ids::Import_Dcls tds::Type_Dcls {
 tds.availableLocalTypes = tds.localTypes;
}

aspect production class_body
cb::Class_Body ::= dcls::Class_Member_Dcls {
 dcls.availableLocalTypes = dcls.localTypes ++ cb.availableLocalTypes;
}

aspect production interface_dcl
idcl::Interface_Dcl ::= mods::Modifiers iname::Id_t inters::TypeNames dcls::Interface_Member_Dcls {
 dcls.availableLocalTypes = dcls.localTypes ++ idcl.availableLocalTypes;
}

aspect production block_stmt_class
s::Stmt ::= cdcl::Class_Dcl {
 cdcl.availableLocalTypes = cdcl.localTypes ++ s.availableLocalTypes;
}

aspect production block_stmt_interface
s::Stmt ::= idcl::Interface_Dcl {
 idcl.availableLocalTypes = idcl.localTypes ++ s.availableLocalTypes;
}

aspect production stmt_seq
seq::Stmt ::= stmt1::Stmt stmt2::Stmt {
 stmt2.availableLocalTypes = stmt1.localTypes ++ seq.availableLocalTypes;
}

-- FQNs_Errors
------------------------------------------------------------

nonterminal FQNs_Errors with fullyQualifiedNames, errors;

abstract production fqns_errors
tes::FQNs_Errors ::= fullyQualifiedNames_::[ FullyQualifiedName ] errors_::[ Error ] {
    tes.fullyQualifiedNames = fullyQualifiedNames_;
    tes.errors := errors_;
}

nonterminal LFQNs_Errors with L_FQNs, errors;
synthesized attribute L_FQNs :: [ LFQN ];

abstract production lfqns_errors
tes::LFQNs_Errors ::= L_FQNs_::[ LFQN ] errors_::[ Error ] {
    tes.L_FQNs = L_FQNs_;
    tes.errors := errors_;
}

function getAvailableSingleTypes
LFQNs_Errors ::= singleTypeImports::[ FullyQualifiedName ] classPathDirectories::[ String ] {
 local attribute temp :: LFQNs_Errors;
 temp = getAvailableSingleTypesHelper (uniqueFullyQualifiedNames (singleTypeImports), classPathDirectories);

 return lfqns_errors (uniqueLFQNs (temp.L_FQNs), temp.errors);
}

function getAvailableSingleTypesHelper
LFQNs_Errors ::= singleTypeImports::[ FullyQualifiedName ] classPathDirectories::[ String ] {

 local attribute firstImport :: FullyQualifiedName;
 firstImport = head (singleTypeImports);

 local attribute first :: LFQNs_Errors;
 first = findSingleImportInClassPath (firstImport, classPathDirectories);

 local attribute rest :: LFQNs_Errors;
 rest = getAvailableSingleTypesHelper (tail (singleTypeImports), classPathDirectories);

 local attribute lfqns::[ LFQN ];
 lfqns = if null (singleTypeImports)
 		then []
         else first.L_FQNs ++ rest.L_FQNs;

 local attribute errs::[ Error ];
 errs = if null (singleTypeImports)
 		then []
        else first.errors ++ rest.errors;

 return lfqns_errors (lfqns, errs);
}

function findSingleImportInClassPath
LFQNs_Errors ::= singleTypeImport::FullyQualifiedName classPathDirectories:: [ String ] {

 local attribute firstDirectory :: String;
 firstDirectory = head (classPathDirectories);

 local attribute jextFilePath :: String;
 jextFilePath = firstDirectory ++ "/" ++ singleTypeImport.qualifiedFileName ++ ".jext";

 local attribute javaFilePath :: String;
 javaFilePath = firstDirectory ++ "/" ++ singleTypeImport.qualifiedFileName ++ ".java";

 local attribute fileInFirstDirectory :: Boolean;
 fileInFirstDirectory = isFile (jextFilePath, unsafeIO ()).iovalue || isFile (javaFilePath, unsafeIO ()).iovalue;

 return if null (classPathDirectories)
		then lfqns_errors ([], [ mkError (singleTypeImport.line_no, "Unknown import type: " ++ singleTypeImport.qualifiedName) ])
	else if fileInFirstDirectory
		then lfqns_errors ([ lfqn (firstDirectory, singleTypeImport) ] , [])
	else findSingleImportInClassPath (singleTypeImport, tail (classPathDirectories));
}

function getAvailableCurrentPackageTypes
LFQNs_Errors ::= currentPack::FullyQualifiedName classPathDirectories::[ String ] currentDirectory::String currentFile::String {

 local attribute classPathSearch :: LFQNs_Errors;
 classPathSearch = findCurrentPackageInClassPath (currentPack, classPathDirectories, currentFile);

 local attribute currentDirectoryOnlySearch :: LFQNs_Errors;
 currentDirectoryOnlySearch = findCurrentPackageInClassPath (currentPack, [ currentDirectory ], currentFile);
 
 local attribute lfqns::[ LFQN ];
 lfqns = case currentPack'' of
-- if there is no package, we search in the current directory
		fully_qualified_name_none () -> currentDirectoryOnlySearch.L_FQNs |
		fully_qualified_name_unknown () -> [ ] |
		_ -> classPathSearch.L_FQNs
	end;

 local attribute errs::[ Error ];
 errs = [ ];

 return lfqns_errors (lfqns, errs);
}

function findCurrentPackageInClassPath
LFQNs_Errors ::= currentPack::FullyQualifiedName classPathDirectories:: [ String ] currentFile::String {

 local attribute firstDirectory :: String;
 firstDirectory = head (classPathDirectories);

 local attribute packagePath :: String;
 packagePath = firstDirectory ++ "/" ++ currentPack.qualifiedFileName;

 local attribute packageInFirstDirectory :: Boolean;
 packageInFirstDirectory = isDirectory (packagePath, unsafeIO ()).iovalue;

 local attribute packageContents :: [ String ];
 packageContents = listContents (packagePath, unsafeIO ()).iovalue;

 local attribute javaLFQNs :: [ LFQN ];
 javaLFQNs = getJavaFileLFQNsWithoutCurrentFile (firstDirectory, currentPack, packageContents, currentFile);

 return if null (classPathDirectories)
		then lfqns_errors ([], [])
	else if packageInFirstDirectory
		then lfqns_errors (javaLFQNs , [])
	else findCurrentPackageInClassPath (currentPack, tail (classPathDirectories), currentFile);
}

function getJavaFileLFQNsWithoutCurrentFile
[ LFQN ] ::= location_::String packageFQN::FullyQualifiedName directoryContents::[ String ] currentFile::String {
 local attribute fileName :: String;
 fileName = head (directoryContents);
 
 local attribute notExtension :: String;
 notExtension = substring (0, lastindexof (".", fileName), fileName);

 local attribute extension :: String;
 extension = substring (lastindexof (".", fileName) + 1, length (fileName), fileName);

 return if null (directoryContents)
	then []
	else if (extension == "jext" || extension == "java") && fileName != currentFile
		then lfqn (location_, case packageFQN of
					fully_qualified_name_none () -> getSimpleFQN (notExtension) |
					_ -> getQualifiedFQN (packageFQN, notExtension)
				      end) :: getJavaFileLFQNs (location_, packageFQN, tail (directoryContents))
		else getJavaFileLFQNs (location_, packageFQN, tail (directoryContents));
}

function getAvailableOnDemandTypes
LFQNs_Errors ::= onDemImports::[ FullyQualifiedName ] classPathDirectories::[ String ] {
 local attribute temp :: LFQNs_Errors;
 temp = getAvailableOnDemandTypesHelper (uniqueFullyQualifiedNames (onDemImports), classPathDirectories);

 return lfqns_errors (uniqueLFQNs (temp.L_FQNs), temp.errors);
}

function getAvailableOnDemandTypesHelper
LFQNs_Errors ::= onDemImports::[ FullyQualifiedName ] classPathDirectories::[ String ] {

 local attribute firstImport :: FullyQualifiedName;
 firstImport = head (onDemImports);

 local attribute first :: LFQNs_Errors;
 first = findOnDemandImportsInClassPath (firstImport, classPathDirectories);

 local attribute rest :: LFQNs_Errors;
 rest = getAvailableOnDemandTypesHelper (tail (onDemImports), classPathDirectories);

 local attribute lfqns::[ LFQN ];
 lfqns = if null (onDemImports)
 		then []
	 else first.L_FQNs ++ rest.L_FQNs;

 local attribute errs::[ Error ];
 errs = if null (onDemImports)
 		then []
 	else first.errors ++ rest.errors;

 return lfqns_errors (lfqns, errs);
}

function findOnDemandImportsInClassPath
LFQNs_Errors ::= onDemImport::FullyQualifiedName classPathDirectories:: [ String ] {

 local attribute firstDirectory :: String;
 firstDirectory = head (classPathDirectories);

 local attribute packagePath :: String;
 packagePath = firstDirectory ++ "/" ++ onDemImport.qualifiedFileName;

 local attribute packageInFirstDirectory :: Boolean;
 packageInFirstDirectory = isDirectory (packagePath, unsafeIO ()).iovalue;

 local attribute packageContents :: [ String ];
 packageContents = listContents (packagePath, unsafeIO ()).iovalue;

 local attribute javaLFQNs :: [ LFQN ];
 javaLFQNs = getJavaFileLFQNs (firstDirectory, onDemImport, packageContents);

 return if null (classPathDirectories)
		then lfqns_errors ([], [ mkError (onDemImport.line_no, "Unknown import package: " ++ onDemImport.qualifiedName) ])
	else if packageInFirstDirectory
		then lfqns_errors (javaLFQNs , [])
	else findOnDemandImportsInClassPath (onDemImport, tail (classPathDirectories));
}

function getJavaFileLFQNs
[ LFQN ] ::= location_::String packageFQN::FullyQualifiedName directoryContents::[ String ] {
 local attribute filename :: String;
 filename = head (directoryContents);
 
 local attribute notExtension :: String;
 notExtension = substring (0, lastindexof (".", filename), filename);

 local attribute extension :: String;
 extension = substring (lastindexof (".", filename) + 1, length (filename), filename);

 return if null (directoryContents)
	then []
	else if extension == "jext" || extension == "java"
		then lfqn (location_, getQualifiedFQN (packageFQN, notExtension)) :: getJavaFileLFQNs (location_, packageFQN, tail (directoryContents))
		else getJavaFileLFQNs (location_, packageFQN, tail (directoryContents));
}

-- thisPackage
--------------

autocopy attribute thisPackage :: FullyQualifiedName ;
attribute thisPackage occurs on			  	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Package_Dcl, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

-- qualifiersSoFar is used to construct the FQN of inner types. We start out with thisPackage and add qualifiers in each class

autocopy attribute qualifiersSoFar :: FullyQualifiedName ;
attribute qualifiersSoFar occurs on			PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Package_Dcl, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

synthesized attribute thisPackage_syn :: FullyQualifiedName ;
attribute thisPackage_syn occurs on Root;	

aspect production compilation_unit
r::Root ::= pd::Package_Dcl ids::Import_Dcls tds::Type_Dcls {
  r.thisPackage_syn = pd.fullyQualifiedName;

  pd.thisPackage = r.thisPackage_syn;
  ids.thisPackage = r.thisPackage_syn;
  tds.thisPackage = r.thisPackage_syn;

  pd.qualifiersSoFar = r.thisPackage_syn;
  ids.qualifiersSoFar = r.thisPackage_syn;
  tds.qualifiersSoFar = r.thisPackage_syn;
}

aspect production package_dcl
p::Package_Dcl ::= n::PackageName {
 p.fullyQualifiedName = n.fullyQualifiedName;
}

aspect production package_dcl_none
p::Package_Dcl ::= {
 p.fullyQualifiedName = fully_qualified_name_none ();
}

-- TypeSearchResult
-------------------

nonterminal TypeSearchResult  with isImportedSingleType, isCurrentPackageType, isImportedOnDemandType, L_FQN, errors;
synthesized attribute isImportedSingleType :: Boolean;
synthesized attribute isCurrentPackageType :: Boolean;
synthesized attribute isImportedOnDemandType :: Boolean;
synthesized attribute L_FQN :: LFQN;

abstract production type_search_result
pnt::TypeSearchResult ::= isImportedSingleType_::Boolean isCurrentPackageType_::Boolean isImportedOnDemandType_::Boolean 
				L_FQN_::LFQN errors_::[ Error ] {
 pnt.isImportedSingleType = isImportedSingleType_;
 pnt.isCurrentPackageType = isCurrentPackageType_;
 pnt.isImportedOnDemandType = isImportedOnDemandType_;
 pnt.L_FQN = L_FQN_;
 pnt.errors := errors_;
}

-- aspects for other productions in Aspects.sv
synthesized attribute resolvedTypeName :: FullyQualifiedName;
attribute resolvedTypeName occurs on 	PackageOrTypeName, TypeName, AmbiguousName;

aspect production simple_package_or_type_name 
ptn::PackageOrTypeName ::= id::Id_t {

 resolvedType = findType (id, ptn.thisPackage, ptn.availableLocalTypes, ptn.availableImportedSingleTypes, ptn.availableCurrentPackageTypes,ptn.availableImportedOnDemandTypes);

 ptn.resolvedTypeName = resolvedType.L_FQN.fullyQualifiedName;

 ptn.neededImportedSingleTypes = if resolvedType.isImportedSingleType
                                 then [ resolvedType.L_FQN ]
                                 else [ ];

 ptn.neededCurrentPackageTypes = if resolvedType.isCurrentPackageType
                                 then [ resolvedType.L_FQN ]
                                 else [ ];

 ptn.neededImportedOnDemandTypes = if resolvedType.isImportedOnDemandType
                                   then [ resolvedType.L_FQN ]
                                   else [ ];
}

aspect production simple_type_name
tn::TypeName ::= id::Id_t {

 resolvedType = findType (id, tn.thisPackage, tn.availableLocalTypes, tn.availableImportedSingleTypes, tn.availableCurrentPackageTypes, tn.availableImportedOnDemandTypes);

 tn.resolvedTypeName = resolvedType.L_FQN.fullyQualifiedName;

 tn.neededImportedSingleTypes = if resolvedType.isImportedSingleType
                                 then [ resolvedType.L_FQN ]
                                 else [ ];

 tn.neededCurrentPackageTypes = if resolvedType.isCurrentPackageType
                                 then [ resolvedType.L_FQN ]
                                 else [ ];

 tn.neededImportedOnDemandTypes = if resolvedType.isImportedOnDemandType
                                   then [ resolvedType.L_FQN ]
                                   else [ ];
}

aspect production simple_ambiguous_name
an::AmbiguousName ::= id::Id_t {

 resolvedType = findType (id, an.thisPackage, an.availableLocalTypes, an.availableImportedSingleTypes, an.availableCurrentPackageTypes, an.availableImportedOnDemandTypes);

 an.resolvedTypeName = resolvedType.L_FQN.fullyQualifiedName;

 an.neededImportedSingleTypes = if resolvedType.isImportedSingleType
                                 then [ resolvedType.L_FQN ]
                                 else [ ];

 an.neededCurrentPackageTypes = if resolvedType.isCurrentPackageType
                                 then [ resolvedType.L_FQN ]
                                 else [ ];

 an.neededImportedOnDemandTypes = if resolvedType.isImportedOnDemandType
                                   then [ resolvedType.L_FQN ]
                                   else [ ];
}

aspect production qualified_package_or_type_name
ptn::PackageOrTypeName ::= pn::PackageOrTypeName id::Id_t {
    ptn.resolvedTypeName = fully_qualified_name_unknown ();
}

aspect production qualified_type_name
tn::TypeName ::= pn::PackageOrTypeName id::Id_t {
    tn.resolvedTypeName = fully_qualified_name_unknown ();
}

aspect production qualified_ambiguous_name
andi::AmbiguousName ::= an::AmbiguousName  id::Id_t {
  andi.resolvedTypeName = fully_qualified_name_unknown ();
}

synthesized attribute resolvedTypeRep :: TypeRep occurs on Type, Reference_Type, Array_Type, Formal_Param;
synthesized attribute resolvedTypeReps :: [ TypeRep ] occurs on Formal_Params;

aspect production primitive_type
t::Type ::= t1::Primitive_Type {
 t.resolvedTypeRep = t1.typerep;
}

aspect production reference_type
t::Type ::= t1::Reference_Type {
 t.resolvedTypeRep = t1.resolvedTypeRep;
}

aspect production void_type
t::Type ::= {
 t.resolvedTypeRep = voidTypeRep ();
}

aspect production name_type
t::Reference_Type ::= n::TypeName {
 t.resolvedTypeRep = fullyQualifiedNameTypeRep (n.resolvedTypeName);
}

aspect production array_type
t::Reference_Type ::= t1::Array_Type {
 t.resolvedTypeRep = t1.resolvedTypeRep;
}

aspect production primitive_array
t::Array_Type ::= t1::Primitive_Type ds::Integer {
 t.resolvedTypeRep = t.typerep;
}

aspect production name_array
t::Array_Type ::= n::TypeName ds::Integer {
 t.resolvedTypeRep = arrayTypeRep (fullyQualifiedNameTypeRep (n.resolvedTypeName), ds);
}

aspect production formal_params_none
fps::Formal_Params ::= {
  fps.resolvedTypeReps = [];
}

aspect production formal_params_one
fps::Formal_Params ::= fp::Formal_Param {
  fps.resolvedTypeReps = [ fp.resolvedTypeRep ];
}

aspect production formal_params_snoc
fps::Formal_Params ::= fps1::Formal_Params fp::Formal_Param {
  fps.resolvedTypeReps = fps1.resolvedTypeReps ++ [ fp.resolvedTypeRep ];
}

aspect production formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
  fp.resolvedTypeRep = t.resolvedTypeRep;
}

-- Functions
-------------------------------------

function findType
TypeSearchResult ::= id::Id_t currentPackage::FullyQualifiedName 
			theLocalTypes::[ String ] singleTypes::[ LFQN ] currentPackageTypes::[ LFQN ] onDemandTypes::[ LFQN ] {

-- search for types from this compilation unit
 local attribute localSearchResult :: [ LFQN ];
 localSearchResult = findLocalType (id, theLocalTypes, currentPackage);

 local attribute singleTypeSearchResult :: [ LFQN ];
 singleTypeSearchResult = findExternalType (id, singleTypes);

-- search for types from this package, but not this compilation unit
 local attribute thisPackageSearchResult :: [ LFQN ];
 thisPackageSearchResult = findExternalType (id, currentPackageTypes);

 local attribute onDemandTypeSearchResult :: [ LFQN ];
 onDemandTypeSearchResult = findExternalType (id, onDemandTypes);

 return if length (localSearchResult) == 1
           then type_search_result (false, false, false, head (localSearchResult), [])
	else if length (localSearchResult) > 1
	   then type_search_result (false, false, false, lfqn ("DummyLocation", fully_qualified_name_none ()),
			[mkError (id.line, "Multiple matches for " ++ id.lexeme ++ " in current compilation unit " ++ currentPackage.qualifiedName)])

        else if length (singleTypeSearchResult) == 1
	   then type_search_result (true, false, false, head (singleTypeSearchResult), [])
        else if length (singleTypeSearchResult) > 1
	   then type_search_result (false, false, false, lfqn ("DummyLocation", fully_qualified_name_none ()),
			[mkError (id.line, "Multiple matches for " ++ id.lexeme ++ " in single type imports ")])

        else if length (thisPackageSearchResult) == 1
	   then type_search_result (false, true, false, head (thisPackageSearchResult), [])
        else if length (thisPackageSearchResult) > 1
	   then type_search_result (false, false, false, lfqn ("DummyLocation", fully_qualified_name_none ()),
			[mkError (id.line, "Multiple matches for " ++ id.lexeme ++ " in current package " ++ currentPackage.qualifiedName)])

        else if length (onDemandTypeSearchResult) == 1
	   then type_search_result (false, false, true, head (onDemandTypeSearchResult), [])
        else if length (onDemandTypeSearchResult) > 1
	   then type_search_result (false, false, false, lfqn ("DummyLocation", fully_qualified_name_none ()),
			[mkError (id.line, "Multiple matches for " ++ id.lexeme ++ " in on demand imports ")])

	else type_search_result (false, false, false, lfqn ("DummyLocation", fully_qualified_name_unknown ()),
			[mkError (id.line, "Unknown identifier " ++ id.lexeme)]);
}

function findLocalType
[ LFQN ] ::= id::Id_t availableTypes::[ String ] currentPackage::FullyQualifiedName {
 local attribute firstType :: String;
 firstType = head (availableTypes);

 return if null (availableTypes)
	  then []
	else ((if id.lexeme == firstType
	         then [ lfqn ("DummyLocation", fully_qualified_name_qualified (currentPackage, id)) ]
	       else [] ) 
	      ++ findLocalType (id, tail (availableTypes), currentPackage));
}

function findExternalType
[ LFQN ] ::= id::Id_t availableTypes::[ LFQN ] {
 local attribute firstType :: LFQN;
 firstType = head (availableTypes);

 return if null (availableTypes)
	  then []
	else ((if id.lexeme == firstType.fullyQualifiedName.name
	         then [ firstType ]
	       else [] ) 
	      ++ findExternalType (id, tail (availableTypes)));
}

-- Dealing with Fully Qualified Names
-------------------------------------

aspect production compilation_unit
r::Root ::= pd::Package_Dcl ids::Import_Dcls tds::Type_Dcls {
 r.neededFullyQualifiedTypes = tds.neededFullyQualifiedTypes
				++ [ 	getQualifiedFQN ( getQualifiedFQN ( getSimpleFQN ("java"), "lang"), "Object"), 
					getQualifiedFQN ( getQualifiedFQN ( getSimpleFQN ("java"), "lang"), "String") ];
}

aspect production simple_package_or_type_name 
ptn::PackageOrTypeName ::= id::Id_t {
 ptn.neededFullyQualifiedTypes = case resolvedType.L_FQN.fullyQualifiedName of
					fully_qualified_name_unknown () -> [ ptn.fullyQualifiedName ] |
					_ -> [ ]
				 end;
}

aspect production simple_type_name
tn::TypeName ::= id::Id_t {
 tn.neededFullyQualifiedTypes = case resolvedType.L_FQN.fullyQualifiedName of
					fully_qualified_name_unknown () -> [ tn.fullyQualifiedName ] |
					_ -> [ ]
				end;
}

aspect production simple_ambiguous_name
an::AmbiguousName ::= id::Id_t {
 an.neededFullyQualifiedTypes = case resolvedType.L_FQN.fullyQualifiedName of
					fully_qualified_name_unknown () -> [ an.fullyQualifiedName ] |
					_ -> [ ]
				end;
}

aspect production qualified_package_or_type_name
ptn::PackageOrTypeName ::= pn::PackageOrTypeName id::Id_t {
 ptn.neededFullyQualifiedTypes = if null (pn.neededFullyQualifiedTypes)
				 then []
				 else [ ptn.fullyQualifiedName ];
}

aspect production qualified_type_name
tn::TypeName ::= pn::PackageOrTypeName id::Id_t {
 tn.neededFullyQualifiedTypes = if null (pn.neededFullyQualifiedTypes)
				 then []
				 else [ tn.fullyQualifiedName ];
}

aspect production qualified_ambiguous_name
andi::AmbiguousName ::= an::AmbiguousName  id::Id_t {
 andi.neededFullyQualifiedTypes = if null (an.neededFullyQualifiedTypes)
				  then []
				  else [ andi.fullyQualifiedName ];
}

aspect production qualified_expr_name
en::ExprName ::= an::AmbiguousName id::Id_t  { 
 en.neededFullyQualifiedTypes = if null (an.neededFullyQualifiedTypes)
				  then []
				  else [ en.fullyQualifiedName ];
}

aspect production qualified_method_name
mn::MethodName ::= an::AmbiguousName  id::Id_t  { 
 mn.neededFullyQualifiedTypes = if null (an.neededFullyQualifiedTypes)
				  then []
				  else [ mn.fullyQualifiedName ];
}

-- other aspects in Aspects.sv


-- This function removes duplicates and calls the function below
function getFullyQualifiedTypes
LFQNs_Errors ::= fullyQualifiedTypes::[ FullyQualifiedName ] classPathDirectories::[ String ] {
 local attribute temp :: LFQNs_Errors;
 temp = getFullyQualifiedTypesHelper (uniqueFullyQualifiedNames (fullyQualifiedTypes), classPathDirectories);

 return lfqns_errors (uniqueLFQNs (temp.L_FQNs), temp.errors);
}

-- This function takes a list of FQNs (say a.b.c, c.d) and generates all possible FQNs
-- from them (a, a.b, a.b.c, c, c.d) and looks for them in a list of directories.
function getFullyQualifiedTypesHelper
LFQNs_Errors ::= fullyQualifiedTypes::[ FullyQualifiedName ] classPathDirectories::[ String ] {

 local attribute firstType :: FullyQualifiedName;
 firstType = head (fullyQualifiedTypes);

 local attribute listOfNames :: [ String ];
 listOfNames = if null (firstType.pathList)
               then error ("firstType is null in getFullyQualifiedTypesHelper")
               else firstType.pathList;

 local attribute first :: LFQNs_Errors;
 first = recurseFullyQualifiedType (getSimpleFQN (head (listOfNames)), tail (listOfNames), classPathDirectories);

 local attribute rest :: LFQNs_Errors;
 rest = getFullyQualifiedTypesHelper (tail (fullyQualifiedTypes), classPathDirectories);

 local attribute lfqns::[ LFQN ];
 lfqns = if null (fullyQualifiedTypes)
 		then []
         else (first.L_FQNs ++ rest.L_FQNs);

 local attribute errs::[ Error ];
 errs = if null (fullyQualifiedTypes)
 		then []
 	else (first.errors ++ rest.errors);

 return lfqns_errors (lfqns, errs);
}

-- This function looks for an FQN (or part thereof) in a set of directories
function recurseFullyQualifiedType
LFQNs_Errors ::= fullyQualifiedTypeSoFar::FullyQualifiedName restOfType::[ String ] classPathDirectories::[ String ] {

 local attribute first :: LFQNs_Errors;
 first = recurseFullyQualifiedTypeHelper (fullyQualifiedTypeSoFar, restOfType, head (classPathDirectories));

 return if null (classPathDirectories)
		then lfqns_errors ([], [ mkError (fullyQualifiedTypeSoFar.line_no, "Unknown type: " ++ fullyQualifiedTypeSoFar.qualifiedName) ])
	else if null (first.errors) -- Means the type has been found in first, use better programming style
		then first
	else recurseFullyQualifiedType (fullyQualifiedTypeSoFar, restOfType, tail (classPathDirectories));
}

-- This function looks for an FQN (or part thereof) in a particular location. 
-- If it finds a file, it returns that, the rest of the FQN is presumably a member of the type.
-- If it finds a directory, it searches in that directory with the rest of the FQN
-- If the whole FQN is a directory, it raises an error
-- If the FQN is not present in the location, it raises an error
-- It returns null errors only if the type has been found (the function above uses this).
-- These error messages aren't actually used
function recurseFullyQualifiedTypeHelper
LFQNs_Errors ::= fullyQualifiedTypeSoFar::FullyQualifiedName restOfType::[ String ] location_::String {

 local attribute directoryName :: String;
 directoryName = location_ ++ "/" ++ fullyQualifiedTypeSoFar.qualifiedFileName;

 local attribute foundDirectory :: Boolean;
 foundDirectory = isDirectory (directoryName, unsafeIO ()).iovalue;

 local attribute jextFileName :: String;
 jextFileName = directoryName ++ ".jext";

 local attribute javaFileName :: String;
 javaFileName = directoryName ++ ".java";

 local attribute foundFile :: Boolean;
 foundFile = isFile (jextFileName, unsafeIO ()).iovalue || isFile (javaFileName, unsafeIO ()).iovalue;

 return if foundDirectory && null (restOfType)
                then lfqns_errors ([],
				  [ mkError (fullyQualifiedTypeSoFar.line_no, fullyQualifiedTypeSoFar.qualifiedName ++ " is a package, not a type") ])
 	else if foundDirectory
                then recurseFullyQualifiedTypeHelper (getQualifiedFQN (fullyQualifiedTypeSoFar, head (restOfType)), tail (restOfType), location_)
 	else if foundFile
                then lfqns_errors ([ lfqn (location_, fullyQualifiedTypeSoFar) ],
				  [])
        else lfqns_errors ([],
			  [ mkError (fullyQualifiedTypeSoFar.line_no, "Unknown type: " ++ fullyQualifiedTypeSoFar.qualifiedName) ]);
}

autocopy attribute availableFullyQualifiedTypes :: [ LFQN ];
attribute availableFullyQualifiedTypes occurs on 	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
							Root, Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs, Import_Dcls, Import_Dcl;

synthesized attribute resolvedPackageOrTypeName :: FullyQualifiedName occurs on PackageOrTypeName, TypeName, AmbiguousName;
synthesized attribute isPackage :: Boolean occurs on PackageOrTypeName, TypeName, AmbiguousName;

aspect production simple_package_or_type_name 
ptn::PackageOrTypeName ::= id::Id_t {

 ptn.resolvedPackageOrTypeName = case ptn.resolvedTypeName of
                                      fully_qualified_name_none () -> fully_qualified_name_none () |
                                      fully_qualified_name_unknown () -> if resolvedPackageOrType.found
										then ptn.fullyQualifiedName
										else fully_qualified_name_none () |
                                      _ -> ptn.resolvedTypeName
                                 end;

 ptn.isPackage = case ptn.resolvedTypeName of
                      fully_qualified_name_unknown () -> resolvedPackageOrType.isPackage |
                      _ -> false
                 end;

 resolvedPackageOrType = lookupPackageOrType (id.line, id.lexeme, ptn.availableFullyQualifiedTypes);
}

aspect production simple_type_name
tn::TypeName ::= id::Id_t {
 tn.resolvedPackageOrTypeName = tn.resolvedTypeName;
 tn.isPackage = false;
}

aspect production simple_ambiguous_name
an::AmbiguousName ::= id::Id_t {
 an.resolvedPackageOrTypeName = case an.resolvedTypeName of
                                      fully_qualified_name_none () -> fully_qualified_name_none () |
                                      fully_qualified_name_unknown () -> if resolvedPackageOrType.found
										then an.fullyQualifiedName
										else fully_qualified_name_none () |
                                      _ -> an.resolvedTypeName
                                end;

 an.isPackage = case an.resolvedTypeName of
                      fully_qualified_name_unknown () -> resolvedPackageOrType.isPackage |
                      _ -> false
                 end;

 resolvedPackageOrType = lookupPackageOrType (id.line, id.lexeme, an.availableFullyQualifiedTypes);
}


-- if pn has been resolved to a type in either the first (resolvedTypeName) or second (resolvedPackageOrTypeName) pass,
--    then ptn temporarily set to unknown, it has to be searched for in type_env in AmbiguousNames.sv
-- if pn has been resolved to a package after the second pass
--    then we search for ptn in its second pass. it may be either a package or type. if it is not found, then it is set to none
-- if pn is (temporarily) unknown after two passes, then ptn is also (temporarily) unknown

aspect production qualified_package_or_type_name
ptn::PackageOrTypeName ::=  pn::PackageOrTypeName id::Id_t {  
 ptn.resolvedPackageOrTypeName = case pn.resolvedTypeName of
                                      fully_qualified_name_none () -> fully_qualified_name_none () |
                                      fully_qualified_name_unknown () -> 
                                         (case pn.resolvedPackageOrTypeName of
					       fully_qualified_name_unknown () -> fully_qualified_name_unknown () |
					       fqn -> (if pn.isPackage 
							then (if !resolvedPackageOrType.found
								then fully_qualified_name_none ()
								else ptn.fullyQualifiedName)
							else fully_qualified_name_unknown ())
					  end) |
                                      fqn -> fully_qualified_name_unknown ()
                                end;

 ptn.isPackage = case pn.resolvedTypeName of
                     fully_qualified_name_unknown () -> 
                         (case pn.resolvedPackageOrTypeName of
			      fully_qualified_name_unknown () -> false |
			      fqn -> (if pn.isPackage
				      then resolvedPackageOrType.isPackage
				      else false)
			  end) |
                     fqn -> false
                end;

 resolvedPackageOrType = lookupPackageOrType (ptn.fullyQualifiedName.line_no, ptn.fullyQualifiedName.qualifiedName, ptn.availableFullyQualifiedTypes);
}

aspect production qualified_type_name
tn::TypeName ::= pn::PackageOrTypeName id::Id_t {  
 tn.resolvedPackageOrTypeName = case pn.resolvedTypeName of
                                      fully_qualified_name_none () -> fully_qualified_name_none () |
                                      fully_qualified_name_unknown () -> 
                                         (case pn.resolvedPackageOrTypeName of
					       fully_qualified_name_unknown () -> fully_qualified_name_unknown () |
					       fqn -> (if pn.isPackage 
							then (if !resolvedPackageOrType.found
								then fully_qualified_name_none ()
								else tn.fullyQualifiedName)
							else fully_qualified_name_unknown ())
					  end) |
                                      fqn -> fully_qualified_name_unknown ()
                                end;

 tn.isPackage = false;
 resolvedPackageOrType = lookupPackageOrType (tn.fullyQualifiedName.line_no, tn.fullyQualifiedName.qualifiedName, tn.availableFullyQualifiedTypes);
}

aspect production qualified_ambiguous_name
andi::AmbiguousName ::= an::AmbiguousName  id::Id_t  { 
 andi.resolvedPackageOrTypeName = case an.resolvedTypeName of
                                      fully_qualified_name_none () -> fully_qualified_name_none () |
                                      fully_qualified_name_unknown () -> 
                                         (case an.resolvedPackageOrTypeName of
					       fully_qualified_name_unknown () -> fully_qualified_name_unknown () |
					       fqn -> (if an.isPackage 
							then (if !resolvedPackageOrType.found
								then fully_qualified_name_none ()
								else andi.fullyQualifiedName)
							else fully_qualified_name_unknown ())
					  end) |
                                      fqn -> fully_qualified_name_unknown ()
                                  end;

 andi.isPackage = case an.resolvedTypeName of
                     fully_qualified_name_unknown () -> 
                         (case an.resolvedPackageOrTypeName of
			      fully_qualified_name_unknown () -> false |
			      fqn -> (if an.isPackage
				      then resolvedPackageOrType.isPackage
				      else false)
			  end) |
                     fqn -> false
                  end;

 resolvedPackageOrType = lookupPackageOrType (andi.fullyQualifiedName.line_no, andi.fullyQualifiedName.qualifiedName, andi.availableFullyQualifiedTypes);
}

-- PackageOrTypeSearchResult
----------------------------

nonterminal PackageOrTypeSearchResult  with found, isPackage, errors;
synthesized attribute found :: Boolean;

abstract production package_or_type_search_result
pnt::PackageOrTypeSearchResult ::= found_::Boolean isPackage_::Boolean errors_::[ Error ] {
 pnt.found = found_;
 pnt.isPackage = isPackage_;
 pnt.errors := errors_;
}

function lookupPackageOrType
PackageOrTypeSearchResult ::= lineNumber::Integer pot::String types::[ LFQN ] {
 return if null (types)
		then package_or_type_search_result (false, false, [ mkError (lineNumber, "Unknown identifier " ++ pot) ])
	else if search > 0
		then package_or_type_search_result (true, search == 1, [])
	else lookupPackageOrType (lineNumber, pot, tail (types));

 local attribute search :: Integer;
 search = areInitialPartsSame (pot, (head (types)).fullyQualifiedName.qualifiedName);
}

-- returns 0: s1 is not the initial substring of s2
-- returns 1: s1 is the initial substring of s2, s1 is a package
-- returns 2: s1 and s2 are the same, s1 is a type

function areInitialPartsSame
Integer ::= s1::String s2::String {
 return if length (s1) == 0 || length (s2) == 0
		then 0 -- we will never correctly compare two empty strings
	else if substring (0, 1, s1) == substring (0, 1, s2)
		then (if length (s1) == 1
			then (if length (s2) == 1
				then 2
				else 1)
			else areInitialPartsSame (substring (1, length (s1), s1), substring (1, length (s2), s2)))
	else 0;
}

-- defs and env functions
-------------------------

abstract production fullyQualifiedNameTypeRep
t::TypeRep ::= fqn::FullyQualifiedName {
 t.eqName = fqn.qualifiedName;
 t.pp = "fullyQualifiedNameTypeRep (\"" ++ fqn.qualifiedName ++ "\")";
 t.unparse = "fullyQualifiedNameTypeRep (" ++ fqn.unparse ++ ")";

 forwards to defaultTypeRep (t);
}

function convertDefsToFullyQualifiedDefs
[ EnvItem ] ::= items::[ EnvItem ] {
 return if null (items)
	then []
	else ((case head (items) of
	       	    envItem (	name_, 
				fully_qualified_name_none (), 
				dclRep)
		-> 
		    envItem (	name_,
				fully_qualified_name_none (),
				    (dclRep)) |

	       	    envItem (	name_, 
				fqn_, 
				dclRep)
		-> 
		    envItem (	fqn_.qualifiedName, 
				    (fqn_), 
				    (dclRep))

	       end )
		:: convertDefsToFullyQualifiedDefs (tail (items)));
}

function filterOutImportedDefs 
[ EnvItem ] ::= allDefs::[ EnvItem ] importedTypes::[ FullyQualifiedName ] {
 return if null (allDefs)
	then []
	else ((if memberFullyQualifiedName ((head (allDefs)).fullyQualifiedName, importedTypes)
		then [ head (allDefs) ]
		else [] ) 
		++ filterOutImportedDefs (tail (allDefs), importedTypes));
}

function filterOutPackageDefs
[ EnvItem ] ::= allDefs::[ EnvItem ] pack::FullyQualifiedName {
 return if null (allDefs)
	then []
	else ((if samePackage ((head (allDefs)).fullyQualifiedName, pack)
		then [ head (allDefs) ]
		else [] ) 
		++ filterOutPackageDefs (tail (allDefs), pack));
}

function samePackage
Boolean ::= typ::FullyQualifiedName pack::FullyQualifiedName {
 return case typ'' of
		fully_qualified_name_none () -> false |
		fully_qualified_name_simple (id1) -> false |
		fully_qualified_name_qualified (qn1, id1) -> equalFullyQualifiedName (    (qn1), pack)
	end;
}

-- String Utility Functions
---------------------------

function memberString
Boolean ::= elt::String ls::[String] {
 return if null (ls) 
	then false 
	else (elt == head (ls) || memberString (elt, tail (ls)));
}

function removeDupes
[String] ::= ss::[String] {
 return if null (ss)
	then []
	else if memberString (head (ss), tail (ss))
		then removeDupes (tail (ss))
		else head (ss) :: removeDupes (tail (ss));
}

function getAllStrings
String ::= ls::[String] delimiter::String {
 return if null(ls) 
	then "" 
	else head (ls) ++ delimiter ++ getAllStrings (tail (ls), delimiter);
}

--function stringsFromStringList
--[String] ::= sl::StringList {
-- return if sl.empty
--	then []
--	else sl.sh :: stringsFromStringList (sl.st);
--}

function lastindexof
Integer ::= elt::String s::String {
 return lastindexof2 (elt, s, length (s) - 1);
}

function lastindexof2
Integer ::= elt::String s::String current::Integer {
 return if current == -1
           then -1
        else if substring (current, current + length (elt), s) == elt
           then current
        else lastindexof2 (elt, s, current - 1);
}

function unparseStrings
String ::= se::[ String ] {
 return "[" ++ unparseStringsHelper (se) ++ "]";
}

function unparseStringsHelper
String ::= se::[ String ] {
 return if null (se)
	then ""
	else "\"" ++ head (se) ++ "\"" ++ (if null (tail (se)) then "" else ",\n\n") ++ unparseStringsHelper (tail (se));
}
