grammar edu:umn:cs:melt:ableJ14:exts:pizza:algebraic;
import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:concretesyntax;

terminal Algebraic_class_type_rep_defsTerm 'algebraic_class_type_rep_defs' lexer classes { java_kwd } ;
terminal Case_repTerm       'case_rep'       lexer classes { java_kwd } ;
terminal Case_paramTerm     'case_param'     lexer classes { java_kwd } ;

nonterminal CaseRep_C with abstractCaseRep;
nonterminal CaseReps_C with abstractCaseRepList;
nonterminal CaseRepList_C with abstractCaseRepList;

nonterminal CaseParam_C with abstractCaseParam;
nonterminal CaseParams_C with abstractCaseParamList;
nonterminal CaseParamList_C with abstractCaseParamList;

synthesized attribute abstractCaseRep :: CaseRep;
synthesized attribute abstractCaseRepList :: [ CaseRep ];

synthesized attribute abstractCaseParam :: CaseParam;
synthesized attribute abstractCaseParamList :: [ CaseParam ];

concrete production algebraic_class_type_rep_defs_c
top::ClassTypeRepDefs_C ::= 'algebraic_class_type_rep_defs' '(' cname::Stringconst_t ',' qualifiedName_::Stringconst_t ',' modifiers_::ModifierList_C ',' 
				parent_::FullyQualifiedName_C ',' interfaces_::FullyQualifiedNameList_C ',' fields_::EnvItemList_C ',' methods_::EnvItemList_C ',' 
				constructors_::EnvItemList_C ',' innerTypes_::EnvItemList_C ',' caseDefs_::CaseRepList_C ')' {
  top.abstractClassTypeRepDefs = algebraic_class_type_rep_defs (removeQuotes (cname.lexeme), removeQuotes (qualifiedName_.lexeme), modifiers_.abstractModifierList, 
					parent_.abstractFullyQualifiedName, interfaces_.abstractFullyQualifiedNameList, fields_.abstractEnvItemList, 
					methods_.abstractEnvItemList, constructors_.abstractEnvItemList, innerTypes_.abstractEnvItemList, caseDefs_.abstractCaseRepList);
}

concrete production case_rep_list
top::CaseRepList_C ::= '[' crs::CaseReps_C ']' {
  top.abstractCaseRepList = crs.abstractCaseRepList;
}

concrete production case_rep_list_none
top::CaseRepList_C ::= '[' ']' {
  top.abstractCaseRepList = [];
}

concrete production case_reps_cons
top::CaseReps_C ::= cr::CaseRep_C ',' rest::CaseReps_C {
  top.abstractCaseRepList = cr.abstractCaseRep :: rest.abstractCaseRepList;
}

concrete production case_reps_one
top::CaseReps_C ::= cr::CaseRep_C {
  top.abstractCaseRepList = [ cr.abstractCaseRep ];
}

concrete production case_rep_c
top::CaseRep_C ::= 'case_rep' '('id::Stringconst_t ',' t::Intconst_t ',' cps::CaseParamList_C ')' {
  top.abstractCaseRep = case_rep (removeQuotes (id.lexeme), toInteger (t.lexeme), cps.abstractCaseParamList);
}

concrete production case_param_list
top::CaseParamList_C ::= '[' crs::CaseParams_C ']' {
  top.abstractCaseParamList = crs.abstractCaseParamList;
}

concrete production case_param_list_none
top::CaseParamList_C ::= '[' ']' {
  top.abstractCaseParamList = [];
}

concrete production case_params_cons_defs
top::CaseParams_C ::= cr::CaseParam_C ',' rest::CaseParams_C {
  top.abstractCaseParamList = cr.abstractCaseParam :: rest.abstractCaseParamList;
}

concrete production case_params_one_defs
top::CaseParams_C ::= cr::CaseParam_C {
  top.abstractCaseParamList = [ cr.abstractCaseParam ];
}

concrete production case_param_c
top::CaseParam_C ::= 'case_param' '(' 'terminal' '(' 'Id_t' ',' id::Stringconst_t ')' ',' tr::TypeRep_C ')' {
  top.abstractCaseParam = case_param (terminal (Id_t, removeQuotes (id.lexeme)), tr.abstractTypeRep);
}
