grammar edu:umn:cs:melt:ableJ14:driver:defsfileinfo;
export edu:umn:cs:melt:ableJ14:driver:defsfileinfo;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:concretesyntax;
export edu:umn:cs:melt:ableJ14:concretesyntax;
syntax edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:terminals;
syntax edu:umn:cs:melt:ableJ14:terminals;

lexer class defs_kwd ;

nonterminal DefsFileInfo_C with abstractDefsFileInfo;
nonterminal ErrorList_C with abstractErrorList;
nonterminal Errors_C with abstractErrorList;
nonterminal Error_C with abstractError;
nonterminal EnvItemList_C with abstractEnvItemList;
nonterminal EnvItems_C with abstractEnvItemList;
nonterminal EnvItem_C with abstractEnvItem;
nonterminal DclRep_C with abstractDclRep;
nonterminal TypeRep_C with abstractTypeRep;
nonterminal TypeReps_C with abstractTypeRepList;
nonterminal TypeRepList_C with abstractTypeRepList;
nonterminal ClassTypeRepDefs_C with abstractClassTypeRepDefs;
nonterminal InterfaceTypeRepDefs_C with abstractInterfaceTypeRepDefs;
nonterminal Modifier_C with abstractModifier;
nonterminal Modifiers_C with abstractModifierList;
nonterminal ModifierList_C with abstractModifierList;
nonterminal FullyQualifiedName_C with abstractFullyQualifiedName;
nonterminal FullyQualifiedNames_C with abstractFullyQualifiedNameList;
nonterminal FullyQualifiedNameList_C with abstractFullyQualifiedNameList;

synthesized attribute abstractDefsFileInfo :: DefsFileInfo;
synthesized attribute abstractErrorList :: [ Error ];
synthesized attribute abstractError :: Error;
synthesized attribute abstractEnvItemList :: [ EnvItem ];
synthesized attribute abstractEnvItem :: EnvItem;
synthesized attribute abstractDclRep :: DclRep;
synthesized attribute abstractTypeRep :: TypeRep;
synthesized attribute abstractTypeRepList :: [ TypeRep ];
synthesized attribute abstractClassTypeRepDefs :: ClassTypeRepDefs;
synthesized attribute abstractInterfaceTypeRepDefs :: InterfaceTypeRepDefs;
synthesized attribute abstractModifier :: Modifier;
synthesized attribute abstractModifierList :: [ Modifier ];
synthesized attribute abstractFullyQualifiedName :: FullyQualifiedName;
synthesized attribute abstractFullyQualifiedNameList :: [ FullyQualifiedName ];

terminal Abstract_modTerm   'abstract_mod'   lexer classes = { defs_kwd } ;

terminal Defs_file_infoTerm 'defs_file_info' lexer classes = { defs_kwd } ;
terminal EnvItemTerm        'envItem'        lexer classes = { defs_kwd } ;
terminal MkErrorTerm        'mkError'        lexer classes = { defs_kwd } ;
terminal Id_tTerm           'Id_t'           lexer classes = { defs_kwd } ;
terminal TerminalTerm       'terminal'       lexer classes = { defs_kwd } ;

terminal Dcl_rep_errorTerm       'dcl_rep_error'       lexer classes = { defs_kwd } ;
terminal Dcl_rep_packageTerm     'dcl_rep_package'     lexer classes = { defs_kwd } ;
terminal Dcl_rep_classTerm       'dcl_rep_class'       lexer classes = { defs_kwd } ;
terminal Dcl_rep_interfaceTerm   'dcl_rep_interface'   lexer classes = { defs_kwd } ;
terminal Dcl_rep_fieldTerm       'dcl_rep_field'       lexer classes = { defs_kwd } ;
terminal Dcl_rep_methodTerm      'dcl_rep_method'      lexer classes = { defs_kwd } ;
terminal Dcl_rep_constructorTerm 'dcl_rep_constructor' lexer classes = { defs_kwd } ;
terminal Dcl_rep_localTerm       'dcl_rep_local'       lexer classes = { defs_kwd } ;
terminal Dcl_rep_paramTerm       'dcl_rep_param'       lexer classes = { defs_kwd } ;

terminal Error_dcl_repTerm       'error_dcl_rep'       lexer classes = { defs_kwd } ;
terminal Package_dcl_repTerm     'package_dcl_rep'     lexer classes = { defs_kwd } ;
terminal Class_dcl_repTerm       'class_dcl_rep'       lexer classes = { defs_kwd } ;
terminal Interface_dcl_repTerm   'interface_dcl_rep'   lexer classes = { defs_kwd } ;
terminal Field_dcl_repTerm       'field_dcl_rep'       lexer classes = { defs_kwd } ;
terminal Method_dcl_repTerm      'method_dcl_rep'      lexer classes = { defs_kwd } ;
terminal Constructor_dcl_repTerm 'constructor_dcl_rep' lexer classes = { defs_kwd } ;
terminal Local_dcl_repTerm       'local_dcl_rep'       lexer classes = { defs_kwd } ;
terminal Param_dcl_repTerm       'param_dcl_rep'       lexer classes = { defs_kwd } ;

terminal CharTypeRepTerm               'charTypeRep'               lexer classes = { defs_kwd } ;
terminal ByteTypeRepTerm               'byteTypeRep'               lexer classes = { defs_kwd } ;
terminal ShortTypeRepTerm              'shortTypeRep'              lexer classes = { defs_kwd } ;
terminal IntTypeRepTerm                'intTypeRep'                lexer classes = { defs_kwd } ;
terminal LongTypeRepTerm               'longTypeRep'               lexer classes = { defs_kwd } ;
terminal FloatTypeRepTerm              'floatTypeRep'              lexer classes = { defs_kwd } ;
terminal DoubleTypeRepTerm             'doubleTypeRep'             lexer classes = { defs_kwd } ;
terminal BooleanTypeRepTerm            'booleanTypeRep'            lexer classes = { defs_kwd } ;
terminal ArrayTypeRepTerm              'arrayTypeRep'              lexer classes = { defs_kwd } ;
terminal VoidTypeRepTerm               'voidTypeRep'               lexer classes = { defs_kwd } ;
terminal ClassTypeRepDefsTerm          'classTypeRepDefs'          lexer classes = { defs_kwd } ;
terminal ObjectClassTypeRepDefsTerm    'objectClassTypeRepDefs'    lexer classes = { defs_kwd } ;
terminal InterfaceTypeRepDefsTerm      'interfaceTypeRepDefs'      lexer classes = { defs_kwd } ;
terminal UnknownTypeRepTerm            'unknownTypeRep'            lexer classes = { defs_kwd } ;
terminal FullyQualifiedNameTypeRepTerm 'fullyQualifiedNameTypeRep' lexer classes = { defs_kwd } ;

terminal Class_type_rep_defsTerm        'class_type_rep_defs'        lexer classes = { defs_kwd } ;
terminal Object_class_type_rep_defsTerm 'object_class_type_rep_defs' lexer classes = { defs_kwd } ;
terminal Interface_type_rep_defsTerm    'interface_type_rep_defs'    lexer classes = { defs_kwd } ;

terminal Fully_qualified_name_noneTerm      'fully_qualified_name_none'      lexer classes = { defs_kwd } ;
terminal Fully_qualified_name_unknownTerm   'fully_qualified_name_unknown'   lexer classes = { defs_kwd } ;
terminal Fully_qualified_name_simpleTerm    'fully_qualified_name_simple'    lexer classes = { defs_kwd } ;
terminal Fully_qualified_name_qualifiedTerm 'fully_qualified_name_qualified' lexer classes = { defs_kwd } ;

concrete production root_defs_file_info_c
top::Root_c ::= dfi::DefsFileInfo_C {
  top.ast_Root = root_defs_file_info (dfi.top.abstractDefsFileInfo);
}

concrete production defs_file_info_c
top::DefsFileInfo_C ::= 'defs_file_info' '(' fullyQualifiedNameList_::FullyQualifiedNameList_C ',' envItemList_::EnvItemList_C ',' errorList_::ErrorList_C ')' {
  top.abstractDefsFileInfo = defs_file_info (fullyQualifiedNameList_.abstractFullyQualifiedNameList, envItemList_.abstractEnvItemList, errorList_.abstractErrorList);
}

concrete production error_list_c
top::ErrorList_C ::= '[' envItems::Errors_C ']' {
  top.abstractErrorList = envItems.abstractErrorList;
}

concrete production error_none_c
top::ErrorList_C ::= '[' ']' {
  top.abstractErrorList = [];
}

concrete production errors_cons_c
top::Errors_C ::= item::Error_C ',' rest::Errors_C {
  top.abstractErrorList = item.abstractError :: rest.abstractErrorList;
}

concrete production errors_one_c
top::Errors_C ::= item::Error_C {
  top.abstractErrorList = [ item.abstractError ];
}

concrete production mkError_c
top::Error_C ::= 'mkError' '(' lin::Intconst_t ',' mess::Stringconst_t ')' {
  top.abstractError = mkError (toInteger (lin.lexeme), removeQuotes (mess.lexeme));
}

concrete production env_item_list_c
top::EnvItemList_C ::= '[' envItems::EnvItems_C ']' {
  top.abstractEnvItemList = envItems.abstractEnvItemList;
}

concrete production env_item_none_c
top::EnvItemList_C ::= '[' ']' {
  top.abstractEnvItemList = [];
}

concrete production env_items_cons_c
top::EnvItems_C ::= item::EnvItem_C ',' rest::EnvItems_C {
  top.abstractEnvItemList = item.abstractEnvItem :: rest.abstractEnvItemList;
}

concrete production env_items_one_c
top::EnvItems_C ::= item::EnvItem_C {
  top.abstractEnvItemList = [ item.abstractEnvItem ];
}

concrete production envItem_c
top::EnvItem_C ::= 'envItem' '(' name_::Stringconst_t ',' qualifiedName_::FullyQualifiedName_C ',' dclRep_::DclRep_C ')' {
  top.abstractEnvItem = envItem (removeQuotes (name_.lexeme), qualifiedName_.abstractFullyQualifiedName, dclRep_.abstractDclRep);
}

concrete production dcl_rep_error_c
top::DclRep_C ::= 'dcl_rep_error' '(' ')' {
  top.abstractDclRep = dcl_rep_error ();
}

concrete production dcl_rep_package_c
top::DclRep_C ::= 'dcl_rep_package' '(' 'package_dcl_rep' '(' id::Stringconst_t ',' qualifiedName_::FullyQualifiedName_C ')' ')' {
  top.abstractDclRep = dcl_rep_package (package_dcl_rep (removeQuotes (id.lexeme), qualifiedName_.abstractFullyQualifiedName));
}

concrete production dcl_rep_class_c
top::DclRep_C ::= 'dcl_rep_class' '(' 'class_dcl_rep' '(' qualifiedName_::FullyQualifiedName_C ',' typeRep_::TypeRep_C ')' ')' {
  top.abstractDclRep =  dcl_rep_class (class_dcl_rep (qualifiedName_.abstractFullyQualifiedName, typeRep_.abstractTypeRep));
}

concrete production dcl_rep_interface_c
top::DclRep_C ::= 'dcl_rep_interface' '(' 'interface_dcl_rep' '(' qualifiedName_::FullyQualifiedName_C ',' typeRep_::TypeRep_C ')' ')' {
  top.abstractDclRep =  dcl_rep_interface (interface_dcl_rep (qualifiedName_.abstractFullyQualifiedName, typeRep_.abstractTypeRep));
}

concrete production dcl_rep_field_c
top::DclRep_C ::= 'dcl_rep_field' '(' 'field_dcl_rep' '(' id::Stringconst_t ',' modifiers_::ModifierList_C ',' typeRep_::TypeRep_C ')' ')' {
  top.abstractDclRep =  dcl_rep_field (field_dcl_rep (removeQuotes (id.lexeme), modifiers_.abstractModifierList, typeRep_.abstractTypeRep));
}

concrete production dcl_rep_method_c
top::DclRep_C ::= 'dcl_rep_method' '(' 'method_dcl_rep' '(' id::Stringconst_t ',' modifiers_::ModifierList_C ',' typeRep_::TypeRep_C ',' typeReps_::TypeRepList_C ')' ')' {
  top.abstractDclRep = dcl_rep_method (method_dcl_rep (removeQuotes (id.lexeme), modifiers_.abstractModifierList, typeRep_.abstractTypeRep, typeReps_.abstractTypeRepList));
}

concrete production dcl_rep_constructor_c
top::DclRep_C ::= 'dcl_rep_constructor' '(' 'constructor_dcl_rep' '(' id::Stringconst_t ',' modifiers_::ModifierList_C ',' typeReps_::TypeRepList_C ')' ')' {
  top.abstractDclRep = dcl_rep_constructor (constructor_dcl_rep (removeQuotes (id.lexeme), modifiers_.abstractModifierList, typeReps_.abstractTypeRepList));
}

concrete production dcl_rep_param_c
top::DclRep_C ::= 'dcl_rep_param' '(' 'param_dcl_rep' '(' id::Stringconst_t ',' typeRep_::TypeRep_C ')' ')' {
  top.abstractDclRep =  dcl_rep_param (param_dcl_rep (removeQuotes (id.lexeme), typeRep_.abstractTypeRep));
}

concrete production dcl_rep_local_c
top::DclRep_C ::= 'dcl_rep_local' '(' 'local_dcl_rep' '(' id::Stringconst_t ',' typeRep_::TypeRep_C ')' ')' {
  top.abstractDclRep =  dcl_rep_local (local_dcl_rep (removeQuotes (id.lexeme), typeRep_.abstractTypeRep));
}

concrete production charTypeRep_c
top::TypeRep_C ::= 'charTypeRep' '(' ')' {
  top.abstractTypeRep = charTypeRep ();
}

concrete production byteTypeRep_c
top::TypeRep_C ::= 'byteTypeRep' '(' ')' {
  top.abstractTypeRep = byteTypeRep ();
}

concrete production shortTypeRep_c
top::TypeRep_C ::= 'shortTypeRep' '(' ')' {
  top.abstractTypeRep = shortTypeRep ();
}

concrete production intTypeRep_c
top::TypeRep_C ::= 'intTypeRep' '(' ')' {
  top.abstractTypeRep = intTypeRep ();
}

concrete production longTypeRep_c
top::TypeRep_C ::= 'longTypeRep' '(' ')' {
  top.abstractTypeRep = longTypeRep ();
}

concrete production floatTypeRep_c
top::TypeRep_C ::= 'floatTypeRep' '(' ')' {
  top.abstractTypeRep = floatTypeRep ();
}

concrete production doubleTypeRep_c
top::TypeRep_C ::= 'doubleTypeRep' '(' ')' {
  top.abstractTypeRep = doubleTypeRep ();
}

concrete production booleanTypeRep_c
top::TypeRep_C ::= 'booleanTypeRep' '(' ')' {
  top.abstractTypeRep = booleanTypeRep ();
}

concrete production arrayTypeRep_c
top::TypeRep_C ::= 'arrayTypeRep' '(' typeRep_::TypeRep_C ',' dims::Intconst_t ')' {
  top.abstractTypeRep = arrayTypeRep (typeRep_.abstractTypeRep, toInteger (dims.lexeme));
}

concrete production voidTypeRep_c
top::TypeRep_C ::= 'voidTypeRep' '(' ')' {
  top.abstractTypeRep = voidTypeRep ();
}

concrete production classTypeRepDefs_c
top::TypeRep_C ::= 'classTypeRepDefs' '(' classTypeRep_::ClassTypeRepDefs_C ')' {
  top.abstractTypeRep = classTypeRepDefs (classTypeRep_.abstractClassTypeRepDefs);
}

concrete production interfaceTypeRepDefs_c
top::TypeRep_C ::= 'interfaceTypeRepDefs' '(' interfaceTypeRep_::InterfaceTypeRepDefs_C ')' {
  top.abstractTypeRep = interfaceTypeRepDefs (interfaceTypeRep_.abstractInterfaceTypeRepDefs);
}

concrete production unknownTypeRep_c
top::TypeRep_C ::= 'unknownTypeRep' '(' ')' {
  top.abstractTypeRep = unknownTypeRep ();
}

concrete production fullyQualifiedNameTypeRep_c
top::TypeRep_C ::= 'fullyQualifiedNameTypeRep' '(' fqn_::FullyQualifiedName_C ')' {
  top.abstractTypeRep = fullyQualifiedNameTypeRep (fqn_.abstractFullyQualifiedName);
}

concrete production class_type_rep_defs_c
top::ClassTypeRepDefs_C ::= 'class_type_rep_defs' '(' cname::Stringconst_t ',' qualifiedName_::Stringconst_t ',' modifiers_::ModifierList_C ',' 
				parent_::FullyQualifiedName_C ',' interfaces_::FullyQualifiedNameList_C ',' fields_::EnvItemList_C ',' methods_::EnvItemList_C ',' 
				constructors_::EnvItemList_C ',' innerTypes_::EnvItemList_C ')' {
  top.abstractClassTypeRepDefs = class_type_rep_defs (removeQuotes (cname.lexeme), removeQuotes (qualifiedName_.lexeme), modifiers_.abstractModifierList, 
							parent_.abstractFullyQualifiedName, interfaces_.abstractFullyQualifiedNameList, fields_.abstractEnvItemList, 
							methods_.abstractEnvItemList, constructors_.abstractEnvItemList, innerTypes_.abstractEnvItemList);
}

concrete production object_class_type_rep_defs_c
top::ClassTypeRepDefs_C ::= 'object_class_type_rep_defs' '(' modifiers_::ModifierList_C ',' interfaces_::FullyQualifiedNameList_C ','
	fields_::EnvItemList_C ',' methods_::EnvItemList_C ',' constructors_::EnvItemList_C ',' innerTypes_::EnvItemList_C ')' {
  top.abstractClassTypeRepDefs = object_class_type_rep_defs (modifiers_.abstractModifierList, interfaces_.abstractFullyQualifiedNameList, 
					fields_.abstractEnvItemList, methods_.abstractEnvItemList, constructors_.abstractEnvItemList, innerTypes_.abstractEnvItemList);
}

concrete production interface_type_rep_defs_c
top::InterfaceTypeRepDefs_C ::= 'interface_type_rep_defs' '(' cname::Stringconst_t ',' qualifiedName_::Stringconst_t ',' modifiers_::ModifierList_C ','
	interfaces_::FullyQualifiedNameList_C ',' fields_::EnvItemList_C ',' methods_::EnvItemList_C ',' innerTypes_::EnvItemList_C ')' {
  top.abstractInterfaceTypeRepDefs = interface_type_rep_defs (removeQuotes (cname.lexeme), removeQuotes (qualifiedName_.lexeme), modifiers_.abstractModifierList,
							interfaces_.abstractFullyQualifiedNameList, fields_.abstractEnvItemList, methods_.abstractEnvItemList,
							innerTypes_.abstractEnvItemList);
}

concrete production type_rep_list_c
top::TypeRepList_C ::= '[' typeReps_::TypeReps_C ']' {
  top.abstractTypeRepList = typeReps_.abstractTypeRepList;
}

concrete production type_rep_list_none_c
top::TypeRepList_C ::= '[' ']' {
  top.abstractTypeRepList = [];
}

concrete production type_reps_cons_c
top::TypeReps_C ::= tr::TypeRep_C ',' rest::TypeReps_C {
  top.abstractTypeRepList = tr.abstractTypeRep :: rest.abstractTypeRepList;
}

concrete production type_reps_one_c
top::TypeReps_C ::= tr::TypeRep_C {
  top.abstractTypeRepList = [ tr.abstractTypeRep ];
}

concrete production modifier_list_c
top::ModifierList_C ::= '[' mods::Modifiers_C ']' {
  top.abstractModifierList = mods.abstractModifierList;
}

concrete production modifier_list_none_c
top::ModifierList_C ::= '[' ']' {
  top.abstractModifierList = [];
}

concrete production modifiers_cons_c
top::Modifiers_C ::= m::Modifier_C ',' rest::Modifiers_C {
  top.abstractModifierList = m.abstractModifier :: rest.abstractModifierList;
}

concrete production modifiers_one_c
top::Modifiers_C ::= m::Modifier_C {
  top.abstractModifierList = [ m.abstractModifier ];
}

concrete production modifier_public_c
top::Modifier_C ::= 'public' '(' ')' {
  top.abstractModifier = public ();
}

concrete production modifier_protected_c
top::Modifier_C ::= 'protected' '(' ')' {
  top.abstractModifier = protected ();
}

concrete production modifier_private_c
top::Modifier_C ::= 'private' '(' ')' {
  top.abstractModifier = private ();
}

concrete production modifier_static_c
top::Modifier_C ::= 'static' '(' ')' {
  top.abstractModifier = static_mod ();
}

concrete production modifier_abstract_c
top::Modifier_C ::= 'abstract_mod' '(' ')' {
  top.abstractModifier = abstract_mod ();
}

concrete production modifier_final_c
top::Modifier_C ::= 'final' '(' ')' {
  top.abstractModifier = final ();
}

concrete production modifier_native_c
top::Modifier_C ::= 'native' '(' ')' {
  top.abstractModifier = native ();
}

concrete production modifier_synchronized_c
top::Modifier_C ::= 'synchronized' '(' ')' {
  top.abstractModifier = synchronized_mod ();
}

concrete production modifier_transient_c
top::Modifier_C ::= 'transient' '(' ')' {
  top.abstractModifier = transient ();
}

concrete production modifier_volatile_c
top::Modifier_C ::= 'volatile' '(' ')' {
  top.abstractModifier = volatile ();
}

concrete production modifier_strictfp_c
top::Modifier_C ::= 'strictfp' '(' ')' {
  top.abstractModifier = strictfp ();
}

concrete production fully_qualified_name_none_c
top::FullyQualifiedName_C ::= 'fully_qualified_name_none' '(' ')' {
  top.abstractFullyQualifiedName = fully_qualified_name_none ();
}

concrete production fully_qualified_name_unknown_c
top::FullyQualifiedName_C ::= 'fully_qualified_name_unknown' '(' ')' {
  top.abstractFullyQualifiedName = fully_qualified_name_unknown ();
}

concrete production fully_qualified_name_simple_c
top::FullyQualifiedName_C ::= 'fully_qualified_name_simple' '(' 'terminal' '(' 'Id_t' ',' id::Stringconst_t ')' ')' {
  top.abstractFullyQualifiedName = fully_qualified_name_simple (terminal (Id_t, removeQuotes (id.lexeme)));
}

concrete production fully_qualified_name_qualified_c
top::FullyQualifiedName_C ::= 'fully_qualified_name_qualified' '(' fqn::FullyQualifiedName_C ',' 'terminal' '(' 'Id_t' ',' id::Stringconst_t ')' ')' {
  top.abstractFullyQualifiedName = fully_qualified_name_qualified (fqn.abstractFullyQualifiedName, terminal (Id_t, removeQuotes (id.lexeme)));
}

concrete production fully_qualified_name_list
top::FullyQualifiedNameList_C ::= '[' fqns::FullyQualifiedNames_C ']' {
  top.abstractFullyQualifiedNameList = fqns.abstractFullyQualifiedNameList;
}

concrete production fully_qualified_name_list_none
top::FullyQualifiedNameList_C ::= '[' ']' {
  top.abstractFullyQualifiedNameList = [];
}

concrete production fully_qualified_names_cons
top::FullyQualifiedNames_C ::= fqn::FullyQualifiedName_C ',' rest::FullyQualifiedNames_C {
  top.abstractFullyQualifiedNameList = fqn.abstractFullyQualifiedName :: rest.abstractFullyQualifiedNameList;
}

concrete production fully_qualified_names_one
top::FullyQualifiedNames_C ::= fqn::FullyQualifiedName_C {
  top.abstractFullyQualifiedNameList = [ fqn.abstractFullyQualifiedName ];
}

function removeQuotes
String ::= s::String {
  return substring (1, length (s) - 1, s);
}
