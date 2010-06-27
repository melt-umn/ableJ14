grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:terminals;


-- The environment is a list of the set of declarations for a scope.
-- The first set of declarations is the innermost scope, the last is
-- the outermost scope.

-- Environment and definitions
autocopy attribute env :: [ ScopeEnv ];
synthesized attribute defs :: [ EnvItem ];

autocopy attribute type_env :: [ ScopeEnv ];
synthesized attribute type_defs :: [ EnvItem ];

-------------------------------------------------------------------

nonterminal TheEnv with scope_envs, current_scope_depth ;
synthesized attribute scope_envs :: [ ScopeEnv ] ;
synthesized attribute current_scope_depth :: Integer ;

inherited attribute theenv :: TheEnv ;

function env_lookup_name
[ DclInfo ] ::= n::String e::TheEnv
{ -- Invariant: the returned list is sorted in descending order
  -- according to scope depth.

 return lookupName (n, e.scope_envs);
}

function env_lookup_id
[ DclInfo ] ::= id::Id_t e::TheEnv
{
 return lookupName (id.lexeme,e.scope_envs);
}

function lookupName
[ DclInfo ] ::= n::String senvs::[ ScopeEnv ]
{
 return if null (senvs)
        then []
        else scope_lookup_name(n,head(senvs)) ++ lookupName (n,tail(senvs)) ;
}

function lookupNameOneScope
[ DclInfo ] ::= n::String senvs::[ ScopeEnv ]
{
 local attribute first_scope_results :: [ DclInfo ];
 first_scope_results = scope_lookup_name (n, head(senvs));

 return if null (senvs)
        	then []
	else if null (first_scope_results)
		then lookupNameOneScope (n, tail (senvs))
        else first_scope_results;
}

function lookupFirstName
[ DclInfo ] ::= n::String senvs::[ ScopeEnv ]
{
 local attribute first_scope_results :: [ DclInfo ];
 first_scope_results = scope_lookup_first_name (n, head(senvs));

 return if null (senvs)
        	then []
	else if null (first_scope_results)
		then lookupFirstName (n, tail (senvs))
        else first_scope_results;
}

function lookupId
[ DclInfo ] ::= id::Id_t senvs::[ ScopeEnv ]
{
 return lookupName (id.lexeme, senvs);
}

function lookupIdOneScope
[ DclInfo ] ::= id::Id_t senvs::[ ScopeEnv ]
{
 return lookupNameOneScope (id.lexeme, senvs);
}

-- Dcl Info
--------------------------------------------------
nonterminal DclInfo with scope_depth, dclrep, name ;

synthesized attribute scope_depth :: Integer ;
synthesized attribute dclrep :: DclRep ;

abstract production dclInfo
d::DclInfo ::= s::Integer dr::DclRep n::String
{ d.scope_depth = s;  d.dclrep = dr;  d.name = n ; }

function typerep_on_DclInfo
TypeRep ::= d::DclInfo
{ return       if   d.dclrep.is_field
               then d.dclrep.field_rep.typerep
          else if d.dclrep.is_local
               then d.dclrep.local_rep.typerep
          else if d.dclrep.is_param
               then d.dclrep.param_rep.typerep
          else error ("Attempt to get typerep from DclInfo that is not local, param, or field") ;
}

-- ScopeEnv
--------------------------------------------------
-- A ScopeEnv contains the declarations for a particular scope.
nonterminal ScopeEnv with scope_depth, env_items ;
synthesized attribute env_items :: [ EnvItem ] ;


-- ScopeEnv creation productions
--------------------------------
abstract production scopeEnv 
se::ScopeEnv ::= s::Integer  eitems::[ EnvItem ] { 
 se.unparse = "scopeEnv (" ++ toString (s) ++ ", " ++ unparseEnvItems (eitems) ++ ")";
 se.scope_depth = s;  
 se.env_items = eitems; 
}

-- ScopeEnv lookup functions
----------------------------

function scope_lookup_name
[ DclInfo ] ::= n::String se::ScopeEnv 
{
 return scope_lookup_name_helper (n, se.env_items, se.scope_depth); 
}

function scope_lookup_name_helper
[ DclInfo ] ::= n::String eitems::[ EnvItem ] scp::Integer
{
 return if null (eitems)
        then []
        else if first_item.name == n
             then cons(dclInfo(scp,first_item.dclrep,n) ,rest)
             else rest ;

 local attribute first_item :: EnvItem ;
 first_item = head(eitems);

 local attribute rest :: [ DclInfo ] ;
 rest = scope_lookup_name_helper (n, tail(eitems), scp);
}

function scope_lookup_first_name
[ DclInfo ] ::= n::String se::ScopeEnv 
{
 return scope_lookup_first_name_helper (n, se.env_items, se.scope_depth); 
}

function scope_lookup_first_name_helper
[ DclInfo ] ::= n::String eitems::[ EnvItem ] scp::Integer
{
 return if null (eitems)
        then []
        else if first_item.name == n
             then [ dclInfo(scp,first_item.dclrep,n) ]
             else rest ;

 local attribute first_item :: EnvItem ;
 first_item = head(eitems);

 local attribute rest :: [ DclInfo ] ;
 rest = scope_lookup_name_helper (n, tail(eitems), scp);
}

function scope_lookup_id
[ DclInfo ] ::= id::Id_t se::ScopeEnv 
{
 return scope_lookup_name (id.lexeme,se);
}

-- EnvItem
--------------------------------------------------
nonterminal EnvItem with name, fullyQualifiedName, dclrep ;
synthesized attribute unparse :: String occurs on EnvItem, ScopeEnv;

abstract production envItem
e::EnvItem ::= name_::String fullyQualifiedName_::FullyQualifiedName dclrep_::DclRep
{
 e.name = name_ ; 
 e.fullyQualifiedName = fullyQualifiedName_;
 e.dclrep = dclrep_ ;
 e.unparse = "envItem (\"" ++ name_ ++ "\", " ++ fullyQualifiedName_.unparse ++ ", " ++ dclrep_.unparse ++ ")";
}

abstract production variableNameBinding
e::EnvItem ::= n::String ref_expr::Expr  {
 e.name = n ;
}

function newScopeEnv
[ ScopeEnv ] ::= its ::[ EnvItem ]
{
 return [ scopeEnv (0, its) ];
}

function appendWithinScope
[ ScopeEnv ] ::= ds::[ EnvItem ] ses::[ ScopeEnv ]
{
 return if null (ses)
	then newScopeEnv (ds)
	else [ scopeEnv (head (ses).scope_depth, ds ++ head (ses).env_items) ] ++ tail (ses);
}

function appendNewScope
[ ScopeEnv ] ::= ds::[ EnvItem ] ses::[ ScopeEnv ]
{
 return if null (ses)
	then newScopeEnv (ds)
	else [ scopeEnv (head (ses).scope_depth + 1, ds) ] ++ ses;
}

function getDefs
[ EnvItem ] ::= ses::[ ScopeEnv ] {
 return if null (ses)
	then []
	else (head (ses)).env_items ++ getDefs (tail (ses));
}

-- Printing
-----------------------------------------------------

function unparseEnv
String ::= se::[ ScopeEnv ] {
 return "[" ++ unparseEnvHelper (se) ++ "]";
}

function unparseEnvHelper
String ::= se::[ ScopeEnv ] {
 return if null (se)
	then ""
	else (head (se)).unparse ++ (if null (tail (se)) then "" else ",\n") ++ unparseEnvHelper (tail (se));
}

function unparseEnvItems
String ::= se::[ EnvItem ] {
 return "[" ++ unparseEnvItemsHelper (se) ++ "]";
}

function unparseEnvItemsHelper
String ::= se::[ EnvItem ] {
 return if null (se)
	then ""
	else (head (se)).unparse ++ (if null (tail (se)) then "" else ",\n\n") ++ unparseEnvItemsHelper (tail (se));
}

-- Lookup and Conversion Functions
----------------------------------

function retrieveTypeRep2
TypeRep ::= typeName::String environment::[ ScopeEnv ] fileName::String lineNumber::Integer {

 local attribute envSearch :: [ DclInfo ] ;
 envSearch = lookupFirstName (typeName, environment);

 local attribute firstDclRep :: DclRep;
 firstDclRep = (head (envSearch)).dclrep;

 return if null (envSearch)
		then error ("Type " ++ typeName ++ " from file " ++ fileName ++ ":" ++ toString (lineNumber) ++ " not found in the environment")
	else if firstDclRep.is_type
		then firstDclRep.typerep
	else error ("Search for type " ++ typeName ++ " returns " ++ (head (envSearch)).dclrep.name);
}

function retrieveTypeRep
TypeRep ::= typeName::String environment::[ ScopeEnv ] {

 local attribute envSearch :: [ DclInfo ] ;
 envSearch = lookupFirstName (typeName, environment);

 local attribute firstDclRep :: DclRep;
 firstDclRep = (head (envSearch)).dclrep;

 return if null (envSearch)
		then error ("Type " ++ typeName ++ " not found in the environment")
	else if firstDclRep.is_type
		then firstDclRep.typerep
	else error ("Search for type " ++ typeName ++ " returns " ++ (head (envSearch)).dclrep.name);
}

function retrieveClass2
TypeRep ::= fqn::FullyQualifiedName environment::[ ScopeEnv ] fileName::String lineNumber::Integer {

 local attribute envSearch :: [ DclInfo ] ;
 envSearch = lookupFirstName (fqn.qualifiedName, environment);

 return case fqn of
	fully_qualified_name_none () -> error ("Retrieving fully_qualified_name_none in retrieveClass2") | --errorTypeRep () |
	fully_qualified_name_unknown () -> unknownTypeRep () |
	_ -> if null (envSearch)
		then error ("Class " ++ fqn.qualifiedName ++ " from file " ++ fileName ++ ":" ++ toString (lineNumber) ++ " not found in the environment")
	     else (case (head (envSearch)).dclrep of
			dcl_rep_class (class_dcl_rep (_, tr)) ->     (tr) |
			_ -> error ("Search for class " ++ fqn.qualifiedName ++ " from file " ++ fileName ++ ":" ++ toString (lineNumber) ++ " returns " ++ (head (envSearch)).dclrep.name)
		   end)
	end;
}

function retrieveClass
TypeRep ::= fqn::FullyQualifiedName environment::[ ScopeEnv ] {

 local attribute envSearch :: [ DclInfo ] ;
 envSearch = lookupFirstName (fqn.qualifiedName, environment);

 return case fqn of
	fully_qualified_name_none () -> error ("Retrieving fully_qualified_name_none in retrieveClass") | --errorTypeRep () |
	fully_qualified_name_unknown () -> unknownTypeRep () |
	_ -> if null (envSearch)
		then error ("Class " ++ fqn.qualifiedName ++ " not found in the environment")
	     else (case (head (envSearch)).dclrep of
			dcl_rep_class (class_dcl_rep (_, tr)) ->     (tr) |
			_ -> error ("Search for class " ++ fqn.qualifiedName ++ " returns " ++ (head (envSearch)).dclrep.name)
		   end)
	end;
}

function retrieveInterface
TypeRep ::= fqn::FullyQualifiedName environment::[ ScopeEnv ] {

 local attribute envSearch :: [ DclInfo ] ;
 envSearch = lookupFirstName (fqn.qualifiedName, environment);

 return case fqn of
	fully_qualified_name_none () -> error ("Retrieving fully_qualified_name_none in retrieveInterface") | --errorTypeRep () |
	fully_qualified_name_unknown () -> unknownTypeRep () |
	_ -> if null (envSearch)
		then error ("Interface " ++ fqn.qualifiedName ++ " not found in the environment")
	     else (case (head (envSearch)).dclrep of
			dcl_rep_interface (interface_dcl_rep (_, tr)) ->     (tr) |
			_ -> error ("Search for interface " ++ fqn.qualifiedName ++ " returns " ++ (head (envSearch)).dclrep.name)
		   end)
	end;
}

function retrieveInterfaces
[ TypeRep ] ::= interfaceNames::[ FullyQualifiedName ] environment::[ ScopeEnv ] {

 return if null (interfaceNames)
	then []
	else retrieveInterface (head (interfaceNames), environment) :: retrieveInterfaces (tail (interfaceNames), environment);
}

-- We create a new nonterminal that contains the results of the conversion
-- from .defs EnvItem's to EnvItem's that contain pointers to the global environment.

-- We use production attributes and collection attributes to allow extensions such as pizza and sql
-- to add checks and conversions of their own

nonterminal ConvertedEnvItem with env_item;
synthesized attribute env_item :: EnvItem;

function convertEnvItems
[ EnvItem ] ::= items::[ EnvItem ] environment::[ ScopeEnv ] {
 return if null (items)
	then []
	else convert_env_item (head (items), environment).env_item :: convertEnvItems (tail (items), environment);
}

abstract production convert_env_item
top::ConvertedEnvItem ::= old::EnvItem environment::[ ScopeEnv ] {
 top.env_item = if null (convertedEnvItems)
			then error ("Internal compiler error: " ++ old.name ++ " has no conversion check specified in convert_env_item")
		else if length (convertedEnvItems) > 1
			then error ("Internal compiler error: " ++ old.name ++ " has multiple conversion checks specified in convert_env_item")
		else head (convertedEnvItems);

 production attribute convertedEnvItems :: [ EnvItem ] with ++;
 convertedEnvItems := 
	case new (old) of
		  envItem (	name_, 
				_, 
				dcl_rep_class (class_dcl_rep (fqn, typeRep)))
		->  
		[  envItem (	name_, 
				fqn, 
				dcl_rep_class (class_dcl_rep (fqn, convert_type_rep (typeRep, environment).typerep))) ]

		|

		  envItem (	name_, 
				_, 
				dcl_rep_interface (interface_dcl_rep (fqn, typeRep)))
		-> 
		[  envItem (	name_, 
				fqn, 
				dcl_rep_interface (interface_dcl_rep (fqn, convert_type_rep (typeRep, environment).typerep))) ]

		|

		  envItem (	name_, 
				fqn, 
				dcl_rep_field (field_dcl_rep (id, mods, typeRep))) 
		-> 
		[ envItem (	name_,
				fqn, 
				dcl_rep_field (field_dcl_rep (id, mods, convert_type_rep (typeRep, environment).typerep))) ]

		|

		  envItem (	name_,
				fqn, 
				dcl_rep_method (method_dcl_rep (id, mods, returnTypeRep, paramTypeReps))) 
		-> 
		[ envItem (	name_,
				fqn, 
				dcl_rep_method (method_dcl_rep (id, mods, convert_type_rep (returnTypeRep, environment).typerep, convertTypeReps (paramTypeReps, environment)))) ]

		|

		  envItem (	name_, 
				fqn, 
				dcl_rep_constructor (constructor_dcl_rep (id, mods, paramTypeReps))) 
		-> 
		[ envItem (	name_,
				fqn,
				dcl_rep_constructor (constructor_dcl_rep (id, mods, convertTypeReps (paramTypeReps, environment)))) ] 

		|		

 		  _
		->
		[ :: EnvItem ]
	       end;
}

nonterminal ConvertedTypeRep with typerep;

function convertTypeReps
[ TypeRep ] ::= typeReps::[ TypeRep ] environment::[ ScopeEnv ] {

 return if null (typeReps)
	then []
	else (convert_type_rep (head (typeReps), environment).typerep :: convertTypeReps (tail (typeReps), environment));
}

abstract production convert_type_rep
top::ConvertedTypeRep ::= old::TypeRep environment::[ ScopeEnv ] {

 top.typerep =  if null (convertedTypeRep)
			then error ("Internal compiler error: " ++ old.eqName ++ " has no conversion check specified in convert_type_rep")
		else if length (convertedTypeRep) > 1
			then error ("Internal compiler error: " ++ old.eqName ++ " has multiple conversion checks specified in convert_type_rep")
		else head (convertedTypeRep);
		
 local attribute envSearch :: [ DclInfo ] ;
 envSearch = lookupFirstName (old.eqName, environment);

 production attribute convertedTypeRep :: [ TypeRep ] with ++;
 convertedTypeRep := 
	case new (old) of
		  fullyQualifiedNameTypeRep (_) 
		-> 
		[ if null (envSearch)
			then errorTypeRep ([ mkError (-1, "Unknown type in convert_type_rep")]) --error ("Unknown type in convert_type_rep")
			else (case (head (envSearch)).dclrep of
				dcl_rep_interface (interface_dcl_rep (_, tr)) -> tr |
				dcl_rep_class (class_dcl_rep (_, tr)) -> tr |
				_ -> error ("Search for " ++ old.eqName ++ " in convert_type_rep returns " ++ (head (envSearch)).dclrep.name)
			      end) ]

		|

		  classTypeRepDefs (object_class_type_rep_defs (modlist_, interfaces_, fields_, methods_, constructors_, innerTypes_))
		->  
		[ classTypeRep (object_class_type_rep (modlist_, interfaces_, fields_, methods_, constructors_, innerTypes_, environment)) ]

		|

		  classTypeRepDefs (class_type_rep_defs (id, qualifiedName_, modlist_, superClass_, interfaces_, fields_, methods_, constructors_, innerTypes_))
		->  
		[  classTypeRep (	if qualifiedName_ == "java.lang.String"
					  	then string_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment) 
					else if qualifiedName_ == "java.lang.Boolean"
						then boolean_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment) 
					else if qualifiedName_ == "java.lang.Char"
						then char_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment)
					else if qualifiedName_ == "java.lang.Byte"
						then byte_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment) 
					else if qualifiedName_ == "java.lang.Short"
						then short_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment) 
					else if qualifiedName_ == "java.lang.Integer"
						then integer_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment) 	
					else if qualifiedName_ == "java.lang.Long"
						then long_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment) 
					else if qualifiedName_ == "java.lang.Float"
						then float_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment) 
					else if qualifiedName_ == "java.lang.Double"
						then double_class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment) 
					else class_type_rep (id, qualifiedName_, modlist_,     (superClass_), 
										interfaces_, fields_, methods_, constructors_, innerTypes_, environment)) ]
 
		|

		  interfaceTypeRepDefs (interface_type_rep_defs (id, qualifiedName_, modlist_, interfaces_, fields_, methods_, innerTypes_))
		-> 
		[ interfaceTypeRep (interface_type_rep (id, qualifiedName_, modlist_, interfaces_, fields_, methods_, innerTypes_, environment)) ]

		|

		  arrayTypeRep (t, dims) 
		-> 
		[ arrayTypeRep (convert_type_rep (t, environment).typerep, dims) ] 

		|

		  byteTypeRep ()
		-> 
		[ byteTypeRep () ] 

		|

		  shortTypeRep ()
		-> 
		[ shortTypeRep () ] 

		|

		  charTypeRep ()
		-> 
		[ charTypeRep () ] 

		|

		  intTypeRep ()
		-> 
		[ intTypeRep () ] 

		|

		  longTypeRep ()
		-> 
		[ longTypeRep () ] 

		|

		  floatTypeRep ()
		-> 
		[ floatTypeRep () ] 

		|

		  doubleTypeRep ()
		-> 
		[ doubleTypeRep () ] 

		|

		  booleanTypeRep ()
		-> 
		[ booleanTypeRep () ] 

		|

		  voidTypeRep ()
		-> 
		[ voidTypeRep () ] 

		|

		  _ 
		->
		[ :: TypeRep ]
	      end;
}

-- .defs file format

synthesized attribute neededTypes :: [ LFQN ];
nonterminal DefsFileInfo with unparse, neededTypes, type_defs, errors;
synthesized attribute defsFileInfo :: DefsFileInfo occurs on Root;

abstract production root_defs_file_info
top::Root ::= dfi::DefsFileInfo {
  top.defsFileInfo = dfi;
}

abstract production defs_file_info
top::DefsFileInfo ::= neededTypes_::[ LFQN ] typeDefs::[ EnvItem ] errs::[ Error ] {
 top.unparse = "defs_file_info (" ++ unparseLFQNs (neededTypes_) ++ ", " ++ unparseEnvItems (typeDefs) ++ ", " ++ unparseErrors (errs) ++ ")";
 top.neededTypes = neededTypes_;
 top.type_defs = typeDefs;
 top.errors := errs;
}

