grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

import edu:umn:cs:melt:ableJ14:terminals;

nonterminal ClassTypeRepDefs with unparse, name, qualifiedName, modlist, defs_superClass, defs_interfaces, defs_fields, defs_methods, defs_constructors, defs_innerTypes;

synthesized attribute qualifiedName :: String;
synthesized attribute defs_superClass :: FullyQualifiedName ;
synthesized attribute defs_interfaces :: [ FullyQualifiedName ] ;
synthesized attribute defs_fields :: [ EnvItem ];
synthesized attribute defs_methods :: [ EnvItem ];
synthesized attribute defs_constructors :: [ EnvItem ];
synthesized attribute defs_innerTypes :: [ EnvItem ];

abstract production class_type_rep_defs
ct::ClassTypeRepDefs ::= cname::String qualifiedName_::String modlist_::[ Modifier ] defs_superClass_::FullyQualifiedName defs_interfaces_::[ FullyQualifiedName ] 
			defs_fields_::[ EnvItem ] defs_methods_::[ EnvItem ] defs_constructors_::[ EnvItem ] defs_innerTypes_::[ EnvItem ] {
 ct.unparse = "class_type_rep_defs (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ defs_superClass_.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (defs_interfaces_) ++ ", " ++ unparseEnvItems (defs_fields_) ++ ", " ++ unparseEnvItems (defs_methods_) ++ ", " ++ 
		unparseEnvItems (defs_constructors_) ++ ", " ++ unparseEnvItems (defs_innerTypes_) ++ ")";

 ct.name = cname;
 ct.qualifiedName = qualifiedName_;
 ct.modlist = modlist_;
 ct.defs_superClass = defs_superClass_;
 ct.defs_interfaces = defs_interfaces_;
 ct.defs_fields = defs_fields_;
 ct.defs_methods = defs_methods_;
 ct.defs_constructors = defs_constructors_;
 ct.defs_innerTypes = defs_innerTypes_;
}

abstract production object_class_type_rep_defs
ct::ClassTypeRepDefs ::= modlist_::[ Modifier ] defs_interfaces_::[ FullyQualifiedName ] 
			defs_fields_::[ EnvItem ] defs_methods_::[ EnvItem ] defs_constructors_::[ EnvItem ] defs_innerTypes_::[ EnvItem ] {
 ct.unparse = "object_class_type_rep_defs (" ++ unparseModifiers (modlist_) ++ ", " ++
		unparseFullyQualifiedNames (defs_interfaces_) ++ ", " ++ unparseEnvItems (defs_fields_) ++ ", " ++ unparseEnvItems (defs_methods_) ++ ", " ++ 
		unparseEnvItems (defs_constructors_) ++ ", " ++ unparseEnvItems (defs_innerTypes_) ++ ")";

 ct.name = "Object";
 ct.qualifiedName = "java.lang.Object";
 ct.modlist = modlist_;
 ct.defs_superClass = error ("Asking object_class_type_rep_defs for defs_superClass");
 ct.defs_interfaces = defs_interfaces_;
 ct.defs_fields = defs_fields_;
 ct.defs_methods = defs_methods_;
 ct.defs_constructors = defs_constructors_;
 ct.defs_innerTypes = defs_innerTypes_;
}

nonterminal ClassTypeRep with unparse, name, qualifiedName, modlist, superClass, interfaces, fields, methods, constructors, innerTypes;

synthesized attribute fields :: [ ScopeEnv ];
synthesized attribute methods :: [ ScopeEnv ];
synthesized attribute constructors :: [ ScopeEnv ];
synthesized attribute innerTypes :: [ ScopeEnv ];
synthesized attribute superClass :: TypeRep ;
synthesized attribute interfaces :: [ TypeRep ] ;

abstract production class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 ct.name = cname;
 ct.qualifiedName = qualifiedName_;
 ct.modlist = modlist_;

 ct.superClass = retrieveClass (parent, environment);
 ct.interfaces = retrieveInterfaces (interfaces_, environment);

 ct.fields = if ct.superClass.isReferenceType
		then appendNewScope (convertedFields, ct.superClass.classtyperep.fields) 
		else [ scopeEnv (4, convertedFields) ];

 local attribute convertedFields :: [ EnvItem ];
 convertedFields = convertEnvItems (fields_, environment);

 ct.methods = if ct.superClass.isReferenceType
		then appendNewScope (convertedMethods, ct.superClass.classtyperep.methods) 
		else [ scopeEnv (4, convertedMethods) ];

 local attribute convertedMethods :: [ EnvItem ];
 convertedMethods = convertEnvItems (methods_, environment);

 ct.constructors = if ct.superClass.isReferenceType
			then appendNewScope (myConstructors_, ct.superClass.classtyperep.constructors) 
			else [ scopeEnv (4, myConstructors_) ];

 local attribute myConstructors_ :: [ EnvItem ];
 myConstructors_ = if null (convertedConstructors)
			then getDefaultConstructor (terminal (Id_t, cname))
			else convertedConstructors;

 local attribute convertedConstructors :: [ EnvItem ];
 convertedConstructors = convertEnvItems (constructors_, environment);

 ct.innerTypes = if ct.superClass.isReferenceType
			then appendNewScope (convertedInnerTypes, ct.superClass.classtyperep.innerTypes) 
			else [ scopeEnv (4, convertedInnerTypes) ];

 local attribute convertedInnerTypes :: [ EnvItem ];
 convertedInnerTypes = convertEnvItems (innerTypes_, environment);
}

abstract production object_class_type_rep
ct::ClassTypeRep ::= modlist_::[ Modifier ] interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] 
				constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "object_class_type_rep (\"" ++ unparseModifiers (modlist_) ++ ", " ++
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 ct.name = "Object";
 ct.qualifiedName = "java.lang.Object";
 ct.superClass = error ("Asking object_class_type_rep for superClass"); -- unknownTypeRep();
 ct.modlist = modlist_;
 ct.interfaces = retrieveInterfaces (interfaces_, environment);

 ct.fields = [ scopeEnv (4, convertedFields) ];

 local attribute convertedFields :: [ EnvItem ];
 convertedFields = convertEnvItems (fields_, environment);

 ct.methods = [ scopeEnv (4, convertedMethods) ];

 local attribute convertedMethods :: [ EnvItem ];
 convertedMethods = convertEnvItems (methods_, environment);

 ct.constructors = [ scopeEnv (4, myConstructors_) ];

 local attribute myConstructors_ :: [ EnvItem ];
 myConstructors_ = if null (convertedConstructors)
			then getDefaultConstructor (terminal (Id_t, "Object"))
			else convertedConstructors;

 local attribute convertedConstructors :: [ EnvItem ];
 convertedConstructors = convertEnvItems (constructors_, environment);

 ct.innerTypes = [ scopeEnv (4, convertedInnerTypes) ];

 local attribute convertedInnerTypes :: [ EnvItem ];
 convertedInnerTypes = convertEnvItems (innerTypes_, environment);
}

abstract production string_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "string_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

-- Boxed types --
abstract production boolean_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "boolean_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

abstract production char_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "char_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

abstract production byte_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "byte_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

abstract production short_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "short_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

abstract production integer_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "integer_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

abstract production long_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "long_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

abstract production float_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "float_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

abstract production double_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {

 ct.unparse = "double_class_type_rep (\"" ++ cname ++ "\", \"" ++ qualifiedName_ ++ "\", " ++ unparseModifiers (modlist_) ++ ", " ++ parent.unparse ++ ", " ++ 
		unparseFullyQualifiedNames (interfaces_) ++ ", " ++ unparseEnvItems (fields_) ++ ", " ++ unparseEnvItems (methods_) ++ ", " ++ 
		unparseEnvItems (constructors_) ++ ", " ++ unparseEnvItems (innerTypes_) ++ ")";

 forwards to class_type_rep (cname, qualifiedName_, modlist_, parent, interfaces_, fields_, methods_, constructors_, innerTypes_, environment);
}

abstract production unknown_class_type_rep
ct::ClassTypeRep ::= {
 ct.unparse = "unknown_class_type_rep ()";
 ct.name = "Unknown";
 ct.qualifiedName = "Unknown";
 ct.modlist = [];
 ct.fields = [];
 ct.methods = [];
 ct.constructors = [];
 ct.innerTypes = [];
 ct.superClass = error ("Asking unknown_class_type_rep for superClass"); -- unknownTypeRep()
 ct.interfaces = [];
}

