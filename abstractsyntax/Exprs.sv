grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

import  edu:umn:cs:melt:ableJ14:terminals;

nonterminal LHS with       enclosingType, env, type_env, errors, pp, pp_indent, basepp, typerep;
nonterminal Expr with      enclosingType, env, type_env, errors, pp, pp_indent, basepp, typerep;
nonterminal Exprs with     enclosingType, env, type_env, errors, pp, pp_indent, basepp, typerepList, count;
nonterminal Dim_Exprs with enclosingType, env, type_env, errors, pp, pp_indent, basepp, count;
nonterminal Stmt_Expr with enclosingType, env, type_env, errors, pp, pp_indent, basepp, typerep;

-- Identifiers are defined in IdentifierReferences.sv --
--------------------------------------------------------


synthesized attribute count :: Integer;

function getLHS
LHS ::= s::String {
 return lhs_name (simple_expr_name (terminal (Id_t, s)));
}

function getExpr
Expr ::= s::String {
 return expr_lhs (getLHS (s));
}

abstract production expr_lhs
expr::Expr ::= lhs::LHS {
  expr.pp = lhs.pp;
  expr.basepp = lhs.basepp;
  expr.errors := lhs.errors;
  expr.typerep = lhs.typerep;
}

abstract production expr_stmt_expr
e::Expr ::= se::Stmt_Expr {
  e.pp = se.pp;
  e.basepp = se.basepp;
  e.errors := se.errors;
  e.typerep = se.typerep ;
}

abstract production this
e::Expr ::= {
  e.pp = "this";
  e.basepp = "this";
  e.errors := [];
  e.typerep = e.enclosingType;
}

--------------------------------------------------------------------------
----- FIELD ACCESS
--------------------------------------------------------------------------

---- Field accesses that are single names, the object is not provided.

abstract production simple_field_access 
lhs::LHS ::= id::Id_t fd::FieldDclRep {
  lhs.pp = id.lexeme;
  lhs.basepp = id.lexeme;
  lhs.typerep = fd.typerep ;
  lhs.errors := [];
}

abstract production disambiguated_field_access
lhs::LHS ::= e1::Expr id::Id_t fd::FieldDclRep {
  lhs.pp = e1.pp ++ "." ++ id.lexeme;
  lhs.basepp = e1.basepp ++ "." ++ id.lexeme;
  lhs.typerep = fd.typerep ;
  lhs.errors := e1.errors;
}

abstract production array_length_access
lhs::LHS ::= e1::Expr {
  lhs.pp = e1.pp ++ ".length";
  lhs.basepp = e1.basepp ++ ".length";
  lhs.typerep = intTypeRep () ;
  lhs.errors := [];
}

abstract production disambiguated_static_field_access
lhs::LHS ::= t::TypeRep id::Id_t fd::FieldDclRep {
  lhs.pp = t.pp ++ "." ++ id.lexeme;
  lhs.basepp = t.pp ++ "." ++ id.lexeme; -- should be t.basepp
  lhs.typerep = fd.typerep ;
  lhs.errors := [] ;
}

abstract production expr_field_access
lhs::LHS ::= e1::Expr id::Id_t {
 lhs.pp = e1.pp ++ "." ++ id.lexeme;

 local attribute fieldsToSearch :: [ ScopeEnv ];
 fieldsToSearch = case e1.typerep of
                    classTypeRep (ctr) -> ctr.fields |
                    interfaceTypeRep (itr) -> itr.fields |
                    _ -> error ("Internal compiler error 1 production expr_field_access " ++ lhs.pp)
		  end;

 local attribute fieldSearchResult :: [ DclInfo ];
 fieldSearchResult = lookupId (id, fieldsToSearch);

 local attribute firstField :: FieldDclRep;
 firstField = case (head (fieldSearchResult)).dclrep of
                  dcl_rep_field (fdr) -> fdr'' |
                  _ -> error ("Internal compiler error 2 production expr_field_access " ++ lhs.pp)
              end;

 forwards to case e1.typerep of 
		unknownTypeRep () ->
                  erroneous_LHS (lhs, e1.errors) |

		errorTypeRep (errs) ->
                  erroneous_LHS (lhs, errs) |

	        classTypeRep (ctr) ->
	          if null (fieldSearchResult)
	             then (if hasUnknownSupers (e1.typerep)
			   then erroneous_LHS (lhs, e1.errors)
			   else erroneous_LHS (lhs, [mkError (id.line, "Class " ++  ctr.name ++ " does not have field " ++ id.lexeme) ])
			   )
	          --else if length (fieldSearchResult) > 1
	            -- then erroneous_LHS (lhs, [mkError (id.line, "Class " ++  ctr.name ++ " has multiple fields " ++ id.lexeme) ])

	      -- check access

	      -- check if final, requires a variable instead of value 

	          else disambiguated_field_access ( e1, id, firstField) |

	        interfaceTypeRep (itr) ->
	          if null (fieldSearchResult)
	             then (if hasUnknownSupers (e1.typerep) -- unknown superinterfaces
			   then erroneous_LHS (lhs, e1.errors)
			   else erroneous_LHS (lhs, [mkError (id.line, "Interface " ++  itr.name ++ " does not have field " ++ id.lexeme) ])
			   )
	          --else if length (fieldSearchResult) > 1
	            -- then erroneous_LHS (lhs, [mkError (id.line, "Interface " ++  itr.name ++ " has multiple fields " ++ id.lexeme) ])

	          -- check access
	      
	          -- check if the context requires a variable instead of value 

	          else disambiguated_field_access ( e1, id, firstField) |

	        arrayTypeRep (_, _) ->
	          if id.lexeme == "length"
	          then disambiguated_field_access ( e1, id, firstField )
	      
	          -- check if the context requires a variable instead of value 

	          else erroneous_LHS (lhs, [mkError (id.line, "Only length can be accessed off arrays " ++ lhs.pp) ]) |

	         _ -> 
	          erroneous_LHS (lhs, [mkError (id.line, "Type that is not a class, interface or array in ExprName " ++ lhs.pp)])
               end;
}

--------------------------------------------------------------------------
----- METHOD CALL
--------------------------------------------------------------------------

abstract production method_call
e::Stmt_Expr ::= name1::MethodName args::Exprs {
  e.pp = name1.pp ++ "(" ++ args.pp ++ ")";

  forwards to case name1.disambiguatedName of
                disambiguated_method_name (mreps) -> disambiguated_method_call (name1, args, mreps) |
                disambiguated_error_name (errs) -> erroneous_Stmt_Expr (e, errs) |
                _ -> error ("Internal compiler error in production name_method_call " ++ e.pp)
              end;
}

abstract production expr_method_call
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs {
 e.pp = obj.pp ++ "." ++ id.lexeme ++ "(" ++ params.pp ++ ")";

 local attribute methodsToSearch :: [ ScopeEnv ];
 methodsToSearch = case obj.typerep of
                       classTypeRep (ctr) -> ctr.methods |
                       interfaceTypeRep (itr) -> itr.methods |
                       _ -> error ("Internal compiler error 1 in production method_call " ++ e.pp)
                   end;

 local attribute methodSearchResult :: [ DclInfo ];
 methodSearchResult = lookupId (id, methodsToSearch);

 local attribute firstMethod :: MethodDclRep;
 firstMethod = case (head (methodSearchResult)).dclrep of
                  dcl_rep_method (mdr) -> mdr'' |
                  _ -> error ("Internal compiler error 2 in production method_call " ++ e.pp)
               end;

 forwards to case obj.typerep of
	      unknownTypeRep () -> 
	         erroneous_Stmt_Expr (e, obj.errors ++ params.errors) |

	      errorTypeRep (errs) -> 
	         erroneous_Stmt_Expr (e, errs ++ params.errors) |

	      classTypeRep (ctr) ->
		 if null (methodSearchResult)
	              then (if hasUnknownSupers (obj.typerep)
			    then erroneous_Stmt_Expr (e, obj.errors ++ params.errors)

			    else erroneous_Stmt_Expr (e, [mkError (id.line, "Class " ++ ctr.name ++ " does not have method " ++ id.lexeme) ])
			    )
		      else resolved_method_call (obj, id, params, [ firstMethod ]) |
	      
	      interfaceTypeRep (itr) ->
		 if null (methodSearchResult)
	              then (if hasUnknownSupers (obj.typerep) -- unknown superinterfaces
			    then erroneous_Stmt_Expr (e, obj.errors ++ params.errors)

			    else erroneous_Stmt_Expr (e, [mkError (id.line, "Interface " ++ itr.name ++ " does not have method " ++ id.lexeme) ])
			    )
	         else resolved_method_call (obj, id, params, [ firstMethod ]) |
	      
	      _ ->
	         erroneous_Stmt_Expr (e,  [mkError (id.line, "Type that is not a class or an interface in MethodName " ++ e.pp)]) 
	     end;
}

abstract production resolved_method_call
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs mdrs::[ MethodDclRep ] {
  e.pp = obj.pp ++ "." ++ id.lexeme ++ "(" ++ params.pp ++ ")";

  local attribute bestMethodSearch :: [ Exprs_MethodDclRep ];
  bestMethodSearch = findMostSpecificMethod (findAllMatchingMethods (params.expr_list, mdrs, e.env, e.type_env,
		e.availableLocalTypes, e.availableImportedSingleTypes, e.availableCurrentPackageTypes, e.availableImportedOnDemandTypes, 
		e.availableFullyQualifiedTypes, e.enclosingType, e.thisPackage));

  local attribute myErrors :: [ Error ];
  myErrors = if null (bestMethodSearch)
		then [ mkError (id.line, "No compatible method found for call to " ++ id.lexeme) ]
	     else if length (bestMethodSearch) > 1
		then [ mkError (id.line, "Ambiguous call to " ++ id.lexeme) ]
	     else [];

  forwards to if !length (bestMethodSearch) == 1
		then erroneous_Stmt_Expr (e, myErrors)
		else resolved_method_call_copy (obj, id, head (bestMethodSearch).expressions, head (bestMethodSearch).method_rep.return_type);
}

abstract production resolved_method_call_copy
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs rettr::TypeRep {
  e.pp = obj.pp ++ "." ++ id.lexeme ++ "(" ++ params.pp ++ ")";
  e.basepp = obj.basepp ++ "." ++ id.lexeme ++ "(" ++ params.basepp ++ ")";
  e.typerep = rettr;
  e.errors := obj.errors ++ params.errors;
}

abstract production disambiguated_method_call
e::Stmt_Expr ::= mn::MethodName params::Exprs mdrs::[ MethodDclRep ] {
  e.pp = mn.pp ++ " (" ++ params.pp ++ ")";

  local attribute bestMethodSearch :: [ Exprs_MethodDclRep ];
  bestMethodSearch = findMostSpecificMethod (findAllMatchingMethods (params.expr_list, mdrs, e.env, e.type_env,
                e.availableLocalTypes, e.availableImportedSingleTypes, e.availableCurrentPackageTypes, e.availableImportedOnDemandTypes, 
		e.availableFullyQualifiedTypes, e.enclosingType, e.thisPackage));

  local attribute myErrors :: [ Error ];
  myErrors = if null (bestMethodSearch)
		then [ mkError (mn.line_no, "No compatible method found for call to " ++ mn.pp) ]
	     else if length (bestMethodSearch) > 1
		then [ mkError (mn.line_no, "Ambiguous call to " ++ mn.pp) ]
	     else [];

  forwards to if !length (bestMethodSearch) == 1
		then erroneous_Stmt_Expr (e, myErrors)
		else disambiguated_method_call_copy (mn, head (bestMethodSearch).expressions, head (bestMethodSearch).method_rep.return_type);
}

abstract production disambiguated_method_call_copy
e::Stmt_Expr ::= mn::MethodName params::Exprs rettr::TypeRep {
  e.pp = mn.pp ++ " (" ++ params.pp ++ ")";
  e.basepp = mn.basepp ++ " (" ++ params.basepp ++ ")";
  e.typerep = rettr;
  e.errors := params.errors;
}

nonterminal Exprs_MethodDclRep with expressions, method_rep;
synthesized attribute expressions :: Exprs;

abstract production exprs_methoddclrep
top::Exprs_MethodDclRep ::= expressions_::Exprs method_rep_::MethodDclRep {
  top.expressions = expressions_;
  top.method_rep = method_rep_;
}

function findAllMatchingMethods
[ Exprs_MethodDclRep ] ::= params::[ Expr ] mdrs::[ MethodDclRep ] environment::[ ScopeEnv ] type_environment::[ ScopeEnv ]
locals::[ String ] singles::[ LFQN ] currents::[ LFQN ] demands::[ LFQN ] fulls::[ LFQN ] enclType::TypeRep thisPack::FullyQualifiedName {
 return
  if null (mdrs)
	then []
  else (if firstMatch.booleanValue
	then [ exprs_methoddclrep (firstMatch.expressions, head (mdrs)) ]
	else []) 
		++ findAllMatchingMethods (params, tail (mdrs), environment, type_environment, locals, singles, currents, demands, fulls, enclType, thisPack);

  local attribute firstMatch :: Boolean_Exprs;
  firstMatch = checkMatch (params, head (mdrs).param_types, environment, type_environment, locals, singles, currents, demands, fulls, enclType, thisPack);
}

nonterminal Boolean_Exprs with booleanValue, expressions;
synthesized attribute booleanValue :: Boolean;

abstract production boolean_exprs
top::Boolean_Exprs ::= booleanValue_::Boolean expressions_::Exprs {
  top.booleanValue = booleanValue_;
  top.expressions = expressions_;
}

function checkMatch
Boolean_Exprs ::= exprs::[ Expr ] paramTypes::[ TypeRep ] environment::[ ScopeEnv ] type_environment::[ ScopeEnv ] 
locals::[ String ] singles::[ LFQN ] currents::[ LFQN ] demands::[ LFQN ] fulls::[ LFQN ] enclType::TypeRep thisPack::FullyQualifiedName {
  return if null (exprs) && null (paramTypes)
		then boolean_exprs (true, exprs_none ())
	else if null (exprs) || null (paramTypes)
		then boolean_exprs (false, exprs_none ())
	else if copyExpr.copyWorked
		then boolean_exprs (rest.booleanValue, exprs_cons (copyExpr, rest.expressions))
	else boolean_exprs (false, exprs_none ());

  local attribute rest :: Boolean_Exprs;
  rest = checkMatch (tail (exprs), tail (paramTypes), environment, type_environment, locals, singles, currents, demands, fulls, enclType, thisPack);

  local attribute copyExpr :: Expr;
  copyExpr = copy (head (exprs), head (paramTypes), -1);

  copyExpr.env = environment;
  copyExpr.type_env = type_environment;
  copyExpr.availableLocalTypes = locals;
  copyExpr.availableImportedSingleTypes = singles;
  copyExpr.availableCurrentPackageTypes = currents;
  copyExpr.availableImportedOnDemandTypes = demands;
  copyExpr.availableFullyQualifiedTypes = fulls;
  copyExpr.enclosingType = enclType;
  copyExpr.thisPackage = thisPack;
}

function findMostSpecificMethod
[ Exprs_MethodDclRep ] ::= emdrs::[ Exprs_MethodDclRep ] {
  return if null (emdrs) then [] else [ head (emdrs) ];
}

--------------------------------------------------------------------------
----- OBJECT CREATION
--------------------------------------------------------------------------

abstract production new_class
e::Stmt_Expr ::= t::TypeName params::Exprs {
  e.pp = "new " ++ t.pp ++ "(" ++ params.pp ++ ")";

  forwards to case t.disambiguatedName of
                disambiguated_type_name (tr) -> disambiguated_new_class (t, params, tr) |
                disambiguated_error_name (errs) -> erroneous_Stmt_Expr (e, errs) |
                _ -> error ("Internal compiler error in production new_class " ++ e.pp)
              end;
}

abstract production disambiguated_new_class
e::Stmt_Expr ::= t::TypeName params::Exprs tr::TypeRep {
  e.pp = "new " ++ t.pp ++ "(" ++ params.pp ++ ")";
  e.basepp = "new " ++ t.basepp ++ "(" ++ params.basepp ++ ")";
  e.typerep = tr;
  e.errors := params.errors;
}

abstract production new_class_body
e::Stmt_Expr ::= t::TypeName params::Exprs cb::Class_Body {
  e.pp = "new " ++ t.pp ++ "(" ++ params.pp ++ ") " ++ cb.pp;

  forwards to case t.disambiguatedName of
		disambiguated_type_name (tr) -> disambiguated_new_class_body (t, params, cb, t.fullyQualifiedName, tr) |
		disambiguated_error_name (errs) -> erroneous_Stmt_Expr (e, errs) |
		_ -> error ("Internal compiler error in production new_class_body " ++ e.pp)
	      end;
}

abstract production disambiguated_new_class_body
e::Stmt_Expr ::= t::TypeName params::Exprs cb::Class_Body typeFQN::FullyQualifiedName tr::TypeRep {
  e.pp = "new " ++ t.pp ++ "(" ++ params.pp ++ ") " ++ cb.pp;
  e.basepp = "new " ++ t.basepp ++ "(" ++ params.basepp ++ ") " ++ cb.basepp;
  e.typerep = this_typerep;
  e.errors := params.errors ++ cb.errors;

  cb.pp_indent = e.pp_indent + 3;
  cb.enclosingType = this_typerep;
  cb.qualifiersSoFar = fqn;
  cb.type_env = this_typerep.classtyperep.innerTypes ++ e.type_env;

  local attribute anon_class_name :: String;
  anon_class_name = "#AnonymousClass" ++ toString (genInt ());

  local attribute fqn :: FullyQualifiedName;
  fqn = getQualifiedFQN (e.qualifiersSoFar, anon_class_name);

  local attribute this_typerepdefs :: TypeRep ;
  this_typerepdefs = case tr'' of
		      classTypeRep (_) -> classTypeRepDefs (class_type_rep_defs (anon_class_name, fqn.qualifiedName, [ :: Modifier ], typeFQN, [ :: FullyQualifiedName ], 
								cb.field_defs, cb.method_defs,cb.constructor_defs, cb.inner_type_defs)) |
		      interfaceTypeRep (_) -> classTypeRepDefs (class_type_rep_defs (anon_class_name, fqn.qualifiedName, [ :: Modifier ], 
								getQualifiedFQN (getQualifiedFQN (getSimpleFQN ("java"), "lang"), "Object"), [ typeFQN ], 
								cb.field_defs, cb.method_defs,cb.constructor_defs, cb.inner_type_defs))
		     end;

  local attribute this_typerep :: TypeRep ;
  this_typerep = convert_type_rep (this_typerepdefs, e.type_env).typerep;
}

abstract production resolved_new_class
e::Stmt_Expr ::= tr::TypeRep params::Exprs {
  e.pp = "new " ++ tr.eqName ++ "(" ++ params.pp ++ ")";
  e.basepp = "new " ++ tr.eqName ++ "(" ++ params.basepp ++ ")";
  e.typerep = tr;
  e.errors := params.errors;
}

abstract production resolved_new_class_body
e::Stmt_Expr ::= tr::TypeRep params::Exprs cb::Class_Body {
  e.pp = "new " ++ tr.eqName ++ "(" ++ params.pp ++ ") " ++ cb.pp;
  e.basepp = "new " ++ tr.eqName ++ "(" ++ params.basepp ++ ") " ++ cb.basepp;
  e.typerep = tr;
  e.errors := params.errors ++ cb.errors;
}

--------------------------------------------------------------------------
----- CASTING
--------------------------------------------------------------------------

abstract production cast_prod
e::Expr ::= name1::TypeName e1::Expr {
  e.pp = "((" ++ name1.pp ++ ") " ++ e1.pp ++ ")";

  forwards to case name1.disambiguatedName of
		disambiguated_type_name (tr) -> disambiguated_cast (name1, e1, tr) |
		disambiguated_error_name (errs) -> erroneous_Expr (e, errs) |
		_ -> error ("Internal compiler error in production cast_name " ++ e.pp)
	      end;
}

abstract production disambiguated_cast
e::Expr ::= t::TypeName e1::Expr tr::TypeRep {
  e.pp = "((" ++ t.pp ++ ") " ++ e1.pp ++ ")";
  e.basepp = "((" ++ t.basepp ++ ") " ++ e1.basepp ++ ")";
  e.errors := e1.errors;
  e.typerep = tr;
}

abstract production cast_primitive
e::Expr ::= t::Primitive_Type e1::Expr {
  e.pp = "((" ++ t.pp ++ ") " ++ e1.pp ++ ")";
  e.basepp = "((" ++ t.basepp ++ ") " ++ e1.basepp ++ ")";
  e.errors := t.errors ++ e1.errors;
  e.typerep = t.typerep;
}

abstract production cast_simple
e::Expr ::= t::Expr e1::Expr {
  e.pp = "((" ++ t.pp ++ ") " ++ e1.pp ++ ")";

  forwards to case t'' of
		expr_lhs (lhs_name (en)) -> cast_prod (getTypeNameFromExprName (en), e1) |
		_ -> cast_expression (t, e1)
	      end;
}

function getTypeNameFromExprName
TypeName ::= en::ExprName {
  return case en'' of
		simple_expr_name (id) -> simple_type_name (id) |
		qualified_expr_name (an, id) -> qualified_type_name (getPackageOrTypeNameFromAmbiguousName (an), id) |
		_ -> error ("Internal error in getTypeNameFromExprName " ++ en.pp)
	 end;
}

function getPackageOrTypeNameFromAmbiguousName
PackageOrTypeName ::= an::AmbiguousName {
  return case an'' of
		simple_ambiguous_name (id) -> simple_package_or_type_name (id) |
		qualified_ambiguous_name (a, id) -> qualified_package_or_type_name (getPackageOrTypeNameFromAmbiguousName (a), id) |
		_ -> error ("Internal error in getPackageOrTypeNameFromAmbiguousName " ++ an.pp)
	 end;
}

abstract production cast_expression
e::Expr ::= t::Expr e1::Expr {
  e.pp = "((" ++ t.pp ++ ") " ++ e1.pp ++ ")";
  e.basepp = "((" ++ t.basepp ++ ") " ++ e1.basepp ++ ")";
  e.errors := t.errors ++ e1.errors;
  e.typerep = t.typerep;
}

abstract production cast_primitive_array
e::Expr ::= t::Primitive_Type dims::Integer e1::Expr {
  e.pp = "((" ++ t.pp ++ " " ++ printDims (dims) ++ ") " ++ e1.pp ++ ")";
  e.basepp = "((" ++ t.basepp ++ " " ++ printDims (dims) ++ ") " ++ e1.basepp ++ ")";
  e.errors := t.errors ++ e1.errors;
  e.typerep = arrayTypeRep (t.typerep, dims);
}

abstract production cast_name_array
e::Expr ::= n::TypeName dims::Integer e1::Expr {
  e.pp = "((" ++ n.pp ++ " " ++ printDims (dims) ++ ") " ++ e1.pp ++ ")";

  forwards to case n.disambiguatedName of
		disambiguated_type_name (tr) -> disambiguated_cast_array (n, dims, e1, arrayTypeRep (tr, dims)) |
		disambiguated_error_name (errs) -> erroneous_Expr (e, errs) |
		_ -> error ("Internal compiler error 2 in production name_array " ++ e.pp)
	      end;
}

abstract production disambiguated_cast_array
e::Expr ::= tn::TypeName dims::Integer e1::Expr tr::TypeRep {
  e.pp = "((" ++ tn.pp ++ " " ++ printDims (dims) ++ ") " ++ e1.pp ++ ")";
  e.basepp = "((" ++ tn.basepp ++ " " ++ printDims (dims) ++ ") " ++ e1.basepp ++ ")";
  e.errors := e1.errors;
  e.typerep = tr;
}

abstract production resolved_cast
e::Expr ::= t::TypeRep e1::Expr {
  e.pp = "((" ++ t.eqName ++ ") " ++ e1.pp ++ ")";
  e.basepp = "((" ++ t.eqName ++ ") " ++ e1.basepp ++ ")";
  e.errors := e1.errors;
  e.typerep = t;
}

--------------------------------------------------------------------------
----- EXPRS
--------------------------------------------------------------------------

synthesized attribute expr_list :: [ Expr ];
attribute expr_list occurs on Exprs;
synthesized attribute typerepList :: [ TypeRep ] ;
abstract production exprs_none
es::Exprs ::= {
  es.pp = "";
  es.basepp = "";
  es.count = 0;
  es.errors := [];
  es.typerepList = [];
  es.expr_list = [];
}

abstract production exprs_one
es::Exprs ::= e::Expr {
  es.pp = e.pp;
  es.basepp = e.basepp;
  es.count = 1;
  es.errors := e.errors;
  es.typerepList = [e.typerep];
  es.expr_list = [e];
}

abstract production exprs_snoc
es::Exprs ::= es1::Exprs e::Expr {
  es.pp = es1.pp ++ (if es1.count == 0 then "" else ", ") ++ e.pp;
  es.basepp = es1.basepp ++ (if es1.count == 0 then "" else ", ") ++ e.basepp;
  es.count = es1.count + 1;
  es.errors := es1.errors ++ e.errors;
  es.typerepList = es1.typerepList ++ [e.typerep];
  es.expr_list = es1.expr_list ++ [e];
}

abstract production exprs_cons
es::Exprs ::= e::Expr es1::Exprs {
  es.pp = e.pp ++ (if es1.count == 0 then "" else ", ") ++ es1.pp;
  es.basepp = e.basepp ++ (if es1.count == 0 then "" else ", ") ++ es1.basepp;
  es.count = 1+ es1.count;
  es.errors := e.errors ++ es1.errors;
  es.typerepList = [e.typerep] ++ es1.typerepList;
  es.expr_list = [e] ++ es1.expr_list;
}

--------------------------------------------------------------------------
----- CONSTANTS
--------------------------------------------------------------------------

abstract production byte_const
e::Expr ::= t::String {
  e.pp = t;
  e.basepp = t;
  e.errors := [];
  e.typerep = byteTypeRep();
}

abstract production short_const
e::Expr ::= t::String {
  e.pp = t;
  e.basepp = t;
  e.errors := [];
  e.typerep = shortTypeRep();
}

abstract production char_const
e::Expr ::= t::String {
  e.pp = t;
  e.basepp = t;
  e.errors := [];
  e.typerep = charTypeRep();
}

abstract production int_const
e::Expr ::= t::String {
  e.pp = t;
  e.basepp = t;
  e.errors := [];
  e.typerep = intTypeRep();
}

abstract production long_const
e::Expr ::= t::String {
  e.pp = t;
  e.basepp = t;
  e.errors := [];
  e.typerep = longTypeRep();
}

abstract production float_const
e::Expr ::= t::String {
  e.pp = t;
  e.basepp = t;
  e.errors := [];
  e.typerep = floatTypeRep();
}

abstract production double_const
e::Expr ::= t::String {
  e.pp = t;
  e.basepp = t;
  e.errors := [];
  e.typerep = doubleTypeRep();
}

abstract production true_const
e::Expr ::= {
  e.pp = "true";
  e.basepp = "true";
  e.errors := [];
  e.typerep = booleanTypeRep();
}

abstract production false_const
e::Expr ::= {
  e.pp = "false";
  e.basepp = "false";
  e.errors := [];
  e.typerep = booleanTypeRep();
}

abstract production string_const
e::Expr ::= t::String {
  e.pp = t;
  e.basepp = t;
  e.errors := [];
  e.typerep = retrieveTypeRep ("java.lang.String", e.type_env);
}

abstract production null_const
e::Expr ::= {
  e.pp = "null";
  e.basepp = "null";
  e.errors := [];
  e.typerep = nullTypeRep();
}

--------------------------------------------------------------------------
----- OPERATORS
--------------------------------------------------------------------------

abstract production not
e::Expr ::= e1::Expr {
  e.pp = "(! " ++ e1.pp ++ ")";
  e.basepp = "(! " ++ e1.basepp ++ ")";
  e.errors := e1.errors ;
  e.typerep = booleanTypeRep();
}

abstract production or_or
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " || " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " || " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = booleanTypeRep();
}

abstract production and_and
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " && " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " && " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors ;
  e.typerep = booleanTypeRep();
}

abstract production super_field_access
lhs::LHS ::= id::Id_t {
  lhs.pp = "super." ++ id.lexeme;
  lhs.basepp = "super." ++ id.lexeme;
  lhs.errors := [];
  lhs.typerep = unknownTypeRep ();
}

abstract production name_super_field_access
lhs::LHS ::= n::TypeName id::Id_t {
  lhs.pp = n.pp ++ ".super." ++ id.lexeme;
  lhs.basepp = n.basepp ++ ".super." ++ id.lexeme;
  lhs.errors := [];
  lhs.typerep = unknownTypeRep ();
}

abstract production super_method_call
e::Stmt_Expr ::= id::Id_t es::Exprs {
  e.pp = "super." ++ id.lexeme ++ "(" ++ es.pp ++ ")";
  e.basepp = "super." ++ id.lexeme ++ "(" ++ es.basepp ++ ")";
  e.errors := es.errors;
  e.typerep = unknownTypeRep ();
}

abstract production name_super_method_call
e::Stmt_Expr ::= n::TypeName id::Id_t es::Exprs {
  e.pp = n.pp ++ ".super." ++ id.lexeme ++ "(" ++ es.pp ++ ")";
  e.basepp = n.basepp ++ ".super." ++ id.lexeme ++ "(" ++ es.basepp ++ ")";
  e.errors := es.errors;
  e.typerep = unknownTypeRep ();
}

abstract production array_access
lhs::LHS ::= n::ExprName e1::Expr {
  lhs.pp = n.pp ++ "[" ++ e1.pp ++ "]";

  forwards to case n.disambiguatedName of
                  disambiguated_expr_name (l) -> array_access_general (expr_lhs (l), e1) |
                  disambiguated_error_name (errs) -> erroneous_LHS (lhs, errs) |
                  _ -> error ("Internal compiler error in production array_access " ++ lhs.pp)
             end ;
}

abstract production array_access_general
lhs::LHS ::= e1::Expr e2::Expr {
  lhs.pp = e1.pp ++ "[" ++ e2.pp ++ "]";
  lhs.basepp = e1.basepp ++ "[" ++ e2.basepp ++ "]";
  lhs.errors := my_errors ++ e1.errors ++ e2.errors; 
  lhs.typerep = oneLessDimension (e1.typerep);

  local attribute my_errors :: [ Error ];
  my_errors = case e1.typerep of
		arrayTypeRep (_, _) -> [ :: Error ] |
		_ -> [ mkError (-1, "Array access " ++ lhs.pp ++ " requires an array") ]
	      end;
}

abstract production mul_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " *= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " *= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production div_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " /= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " /= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production mod_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " %= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " %= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production plus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " += " ++ expr.pp;
  e.basepp = lhs.basepp ++ " += " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production minus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " -= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " -= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production lshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " <<= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " <<= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production rshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " >>= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " >>= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production urshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " >>>= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " >>>= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production and_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " &= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " &= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production xor_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " ^= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " ^= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production or_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
  e.pp = lhs.pp ++ " |= " ++ expr.pp;
  e.basepp = lhs.basepp ++ " |= " ++ expr.basepp;
  e.errors := lhs.errors ++ expr.errors;
  e.typerep = lhs.typerep;
}

abstract production new_class_expr
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs {
  e.pp = expr.pp ++ ".new " ++ id.lexeme ++ "(" ++ es.pp ++ ")";
  e.basepp = expr.basepp ++ ".new " ++ id.lexeme ++ "(" ++ es.basepp ++ ")";
  e.errors := expr.errors ++ es.errors;
  e.typerep = unknownTypeRep ();
}

abstract production new_class_expr_body
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs cb::Class_Body {
  e.pp = expr.pp ++ ".new " ++ id.lexeme ++ "(" ++ es.pp ++ ")" ++ cb.pp;
  e.basepp = expr.basepp ++ ".new " ++ id.lexeme ++ "(" ++ es.basepp ++ ")" ++ cb.basepp;
  e.errors := expr.errors ++ es.errors ++ cb.errors;
  e.typerep = unknownTypeRep ();
}

abstract production new_class_name
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs {
  e.pp = nam.pp ++ ".new " ++ id.lexeme ++ "(" ++ es.pp ++ ")";
  e.basepp = nam.basepp ++ ".new " ++ id.lexeme ++ "(" ++ es.basepp ++ ")";
  e.errors := es.errors;
  e.typerep = unknownTypeRep ();
}

abstract production new_class_name_body
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs cb::Class_Body {
  e.pp = nam.pp ++ ".new " ++ id.lexeme ++ "(" ++ es.pp ++ ")" ++ cb.pp;
  e.basepp = nam.basepp ++ ".new " ++ id.lexeme ++ "(" ++ es.basepp ++ ")" ++ cb.basepp;
  e.errors := es.errors ++ cb.errors;
  e.typerep = unknownTypeRep ();
}

abstract production conditional
e::Expr ::= cond::Expr thenexpr::Expr elseexpr::Expr {
  e.pp = "(" ++ cond.pp ++ " ? " ++ thenexpr.pp ++ " : " ++ elseexpr.pp ++ ")";
  e.basepp = "(" ++ cond.basepp ++ " ? " ++ thenexpr.basepp ++ " : " ++ elseexpr.basepp ++ ")";
  e.errors := cond.errors ++ thenexpr.errors ++ elseexpr.errors;
  e.typerep = thenexpr.typerep;
}

abstract production or
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " | " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " | " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep =  e1.typerep; --booleanTypeRep ();
}

abstract production xor
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " ^ " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " ^ " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep =  e1.typerep; --booleanTypeRep ();
}

abstract production and
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " & " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " & " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = e1.typerep; --booleanTypeRep ();
}

abstract production eq
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " == " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " == " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = booleanTypeRep ();
}

abstract production not_eq
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " != " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " != " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = booleanTypeRep ();
}

abstract production lt
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " < " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " < " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = booleanTypeRep ();
}

abstract production gt
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " > " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " > " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = booleanTypeRep ();
}

abstract production lteq
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " <= " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " <= " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = booleanTypeRep ();
}

abstract production gteq
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " >= " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " >= " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = booleanTypeRep ();
}
abstract production instanceof
e::Expr ::= e1::Expr t1::Reference_Type {
  e.pp = "(" ++ e1.pp ++ " instanceof " ++ t1.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " instanceof " ++ t1.basepp ++ ")";
  e.errors := e1.errors ++ t1.errors;
  e.typerep = booleanTypeRep ();
}

abstract production lshift
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " << " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " << " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = e1.typerep;
}

abstract production rshift
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " >> " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " >> " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = e1.typerep;
}

abstract production urshift
e::Expr ::= e1::Expr e2::Expr {
  e.pp = "(" ++ e1.pp ++ " >>> " ++ e2.pp ++ ")";
  e.basepp = "(" ++ e1.basepp ++ " >>> " ++ e2.basepp ++ ")";
  e.errors := e1.errors ++ e2.errors;
  e.typerep = e1.typerep;
}


abstract production unary_plus
e::Expr ::= e1::Expr {
  e.pp = "(+ " ++ e1.pp ++ ")";
  e.basepp = "(+ " ++ e1.basepp ++ ")";
  e.errors := e1.errors;
  e.typerep = e1.typerep;
}

abstract production unary_minus
e::Expr ::= e1::Expr {
  e.pp = "(- " ++ e1.pp ++ ")";
  e.basepp = "(- " ++ e1.basepp ++ ")";
  e.errors := e1.errors;
  e.typerep = e1.typerep;
}

abstract production pre_inc
e::Stmt_Expr ::= e1::Expr {
  e.pp = "++" ++ e1.pp;
  e.basepp = "++" ++ e1.basepp;
  e.errors := e1.errors;
  e.typerep = e1.typerep;
}

abstract production pre_dec
e::Stmt_Expr ::= e1::Expr {
  e.pp = "--" ++ e1.pp ;
  e.basepp = "--" ++ e1.basepp;
  e.errors := e1.errors;
  e.typerep = e1.typerep;
}

abstract production comp
e::Expr ::= e1::Expr {
  e.pp = "(~ " ++ e1.pp ++ ")";
  e.basepp = "(~ " ++ e1.basepp ++ ")";
  e.errors := e1.errors;
  e.typerep = e1.typerep;
}

abstract production post_inc
e::Stmt_Expr ::= e1::Expr {
  e.pp = e1.pp ++ "++";
  e.basepp = e1.basepp ++ "++";
  e.errors := e1.errors;
  e.typerep = e1.typerep;
}

abstract production post_dec
e::Stmt_Expr ::= e1::Expr {
  e.pp = e1.pp ++ "--";
  e.basepp = e1.basepp ++ "--";
  e.errors := e1.errors;
  e.typerep = e1.typerep;
}

abstract production new_array_init_primitive
e::Expr ::= t1::Primitive_Type dims::Integer ai::Array_Init {
  e.pp = "(new " ++ t1.pp ++ printDims (dims) ++ ai.pp ++ ")";
  e.basepp = "(new " ++ t1.basepp ++ printDims (dims) ++ ai.basepp ++ ")";
  e.errors := t1.errors ++ ai.errors;
  e.typerep = arrayTypeRep (t1.typerep, dims);
}

abstract production new_array_init_name
e::Expr ::= n::TypeName dims::Integer ai::Array_Init {
  e.pp = "(new " ++ n.pp ++ printDims (dims) ++ ai.pp ++ ")";

  forwards to case n.disambiguatedName of
		disambiguated_type_name (tr) -> resolved_new_array_init_name (n, dims, ai, tr) |
		disambiguated_error_name (errs) -> erroneous_Expr (e, errs) |
		_ -> error ("Internal compiler error in production name_array_init_name " ++ e.pp)
	      end;
}

abstract production resolved_new_array_init_name
e::Expr ::= n::TypeName dims::Integer ai::Array_Init tr::TypeRep {
  e.pp = "(new " ++ n.pp ++ printDims (dims) ++ ai.pp ++ ")";
  e.basepp = "(new " ++ n.basepp ++ printDims (dims) ++ ai.basepp ++ ")";
  e.errors := ai.errors;
  e.typerep = arrayTypeRep (tr, dims);
}

abstract production new_array_no_init_primitive
e::Expr ::= t1::Primitive_Type d1::Dim_Exprs dims::Integer {
  e.pp = "(new " ++ t1.pp ++ d1.pp ++ printDims (dims) ++ ")";
  e.basepp = "(new " ++ t1.basepp ++ d1.basepp ++ printDims (dims) ++ ")";
  e.errors := t1.errors ++ d1.errors;
  e.typerep = arrayTypeRep (t1.typerep, d1.count + dims);
}

abstract production new_array_no_init_name
e::Expr ::= n::TypeName d1::Dim_Exprs dims::Integer {
  e.pp = "(new " ++ n.pp ++ d1.pp ++ printDims (dims) ++ ")";

  forwards to case n.disambiguatedName of
		disambiguated_type_name (tr) -> resolved_new_array_no_init_name (n, d1, dims, tr) |
		disambiguated_error_name (errs) -> erroneous_Expr (e, errs) |
		_ -> error ("Internal compiler error in production name_array_no_init_name " ++ e.pp)
	      end;
}

abstract production resolved_new_array_no_init_name
e::Expr ::= n::TypeName d1::Dim_Exprs dims::Integer tr::TypeRep {
  e.pp = "(new " ++ n.pp ++ d1.pp ++ printDims (dims) ++ ")";
  e.basepp = "(new " ++ n.basepp ++ d1.basepp ++ printDims (dims) ++ ")";
  e.errors := d1.errors;
  e.typerep = arrayTypeRep (tr, d1.count + dims);
}

abstract production primitive_dot_class
e::Expr ::= t::Primitive_Type {
  e.pp = t.pp ++ ".class";
  e.basepp = t.basepp ++ ".class";
  e.errors := t.errors;
  e.typerep = unknownTypeRep ();
}

abstract production void_dot_class
e::Expr ::= {
  e.pp = "void.class";
  e.basepp = "void.class";
  e.errors := [];
  e.typerep = unknownTypeRep ();
}

abstract production array_dot_class
e::Expr ::= t::Array_Type {
  e.pp = t.pp ++ ".class";
  e.basepp = t.basepp ++ ".class";
  e.errors := t.errors;
  e.typerep = unknownTypeRep ();
}

abstract production name_dot_class
e::Expr ::= n::TypeName {
  e.pp = n.pp ++ ".class";
  e.basepp = n.basepp ++ ".class";
  e.errors := [];
  e.typerep = unknownTypeRep ();
}

abstract production name_dot_this
e::Expr ::= n::TypeName {
  e.pp = n.pp ++ ".this";
  e.basepp = n.basepp ++ ".this";
  e.errors := [];
  e.typerep = unknownTypeRep ();
}

abstract production dim_exprs_one
d::Dim_Exprs ::= e::Expr {
  d.pp = "[" ++ e.pp ++ "]";
  d.basepp = "[" ++ e.basepp ++ "]";
  d.errors := e.errors;
  d.count = 1;
}

abstract production dim_exprs_snoc
d::Dim_Exprs ::= d1::Dim_Exprs e::Expr {
  d.pp = d1.pp ++ "[" ++ e.pp ++ "]";
  d.basepp = d1.basepp ++ "[" ++ e.basepp ++ "]";
  d.errors := d1.errors ++ e.errors;
  d.count = d1.count + 1;
}

function printDims
String ::= dims::Integer {
  return (if dims < 1 then "" else "[]" ++ printDims (dims - 1));
}
