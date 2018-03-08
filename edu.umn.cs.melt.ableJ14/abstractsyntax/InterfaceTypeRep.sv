grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

import  edu:umn:cs:melt:ableJ14:terminals;

nonterminal InterfaceTypeRepDefs with unparse, name, qualifiedName, modlist, defs_interfaces, defs_fields, defs_methods, defs_innerTypes;

abstract production interface_type_rep_defs
it::InterfaceTypeRepDefs ::= iname::String qualifiedName_::String modlist_::[ Modifier ] defs_interfaces_::[ FullyQualifiedName ] 
			defs_fields_::[ EnvItem ] defs_methods_::[ EnvItem ] defs_innerTypes_::[ EnvItem ] {

 it.unparse = "interface_type_rep_defs (\"" ++ iname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++
		unparseFullyQualifiedNames (defs_interfaces_) ++ ", " ++ unparseEnvItems (defs_fields_) ++ ", " ++ unparseEnvItems (defs_methods_) ++ ", " ++ 
		unparseEnvItems (defs_innerTypes_) ++ ")";

 it.name = iname;
 it.qualifiedName = qualifiedName_;
 it.modlist = modlist_;
 it.defs_interfaces = defs_interfaces_;
 it.defs_fields = defs_fields_;
 it.defs_methods = defs_methods_;
 it.defs_innerTypes = defs_innerTypes_;
}

nonterminal InterfaceTypeRep with unparse, name, qualifiedName, modlist, interfaces, fields, methods, innerTypes;

abstract production interface_type_rep
it::InterfaceTypeRep ::= iname::String qualifiedName_::String modlist_::[ Modifier ] interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 it.unparse = "interface_type_rep (\"" ++ iname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (innerTypes_) ++ ")";

 it.name = iname;
 it.qualifiedName = qualifiedName_;
 it.modlist = modlist_;

 it.interfaces = retrieveInterfaces (interfaces_, environment);

 --

 it.fields = if !null (superFields)
		then appendNewScope (convertedFields, superFields) 
		else [ scopeEnv (4, convertedFields) ];

 local attribute superFields :: [ ScopeEnv ];
 superFields = getSuperFields (it.interfaces);

 local attribute convertedFields :: [ EnvItem ];
 convertedFields = convertEnvItems (fields_, environment);

 --

 it.methods = if !null (superMethods)
		then appendNewScope (convertedMethods, superMethods) 
		else [ scopeEnv (4, convertedMethods) ];

 local attribute superMethods :: [ ScopeEnv ];
 superMethods = getSuperMethods (it.interfaces);

 local attribute convertedMethods :: [ EnvItem ];
 convertedMethods = convertEnvItems (methods_, environment);

 --

 it.innerTypes = if !null (superInnerTypes)
		then appendNewScope (convertedInnerTypes, superInnerTypes) 
		else [ scopeEnv (4, convertedInnerTypes) ];

 local attribute superInnerTypes :: [ ScopeEnv ];
 superInnerTypes = getSuperInnerTypes (it.interfaces);

 local attribute convertedInnerTypes :: [ EnvItem ];
 convertedInnerTypes = convertEnvItems (innerTypes_, environment);
}

abstract production unknown_interface_type_rep
it::InterfaceTypeRep ::= {

 it.unparse = "unknown_interface_type_rep ()";
 it.name = "Unknown";
 it.qualifiedName = "Unknown";
 it.modlist = [];
 it.fields = [];
 it.methods = [];
 it.innerTypes = [];
 it.interfaces = [];
}

function getSuperFields
[ ScopeEnv ] ::= supers::[ TypeRep ] {
 return if null (supers)
	then []
	else (if head (supers).isReferenceType
		then head (supers).interfacetyperep.fields
		else []) ++ getSuperFields (tail (supers));
}

function getSuperMethods
[ ScopeEnv ] ::= supers::[ TypeRep ] {
 return if null (supers)
	then []
	else (if head (supers).isReferenceType
		then head (supers).interfacetyperep.methods
		else []) ++ getSuperMethods (tail (supers));
}

function getSuperInnerTypes
[ ScopeEnv ] ::= supers::[ TypeRep ] {
 return if null (supers)
	then []
	else (if head (supers).isReferenceType
		then head (supers).interfacetyperep.innerTypes
		else []) ++ getSuperInnerTypes (tail (supers));
}
