grammar edu:umn:cs:melt:ableJ14:exts:foreach;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;

nonterminal Enhanced_For_Stmt_c     with ast_Stmt, labels;
nonterminal Enhanced_For_Stmt_NSI_c with ast_Stmt, labels;

inherited attribute labels :: [ Id_t ];
attribute labels occurs on Stmt;

terminal NewFor_t 'newfor' dominates { Id_t } ;

concrete production stmt_enhanced_for_c
top::statement ::= fors::Enhanced_For_Stmt_c          
precedence = 10 
{ top.ast_Stmt = fors.ast_Stmt;
  fors.labels = [];
}

concrete production enhanced_for_c
top::Enhanced_For_Stmt_c ::= f::For_t '(' t::type id::Id_t ':' e::expression ')' body::statement { 
 top.ast_Stmt = enhanced_for_dispatcher (f, t.ast_Type, id, e.ast_Expr, body.ast_Stmt, top.labels);
}

concrete production labelled_enhanced_for_c
top::Enhanced_For_Stmt_c ::= label::Id_t ':'  fors::Enhanced_For_Stmt_c
precedence = 100 
{
 top.ast_Stmt = fors.ast_Stmt;
 fors.labels = top.labels ++ [ label ];
}

abstract production enhanced_for_dispatcher
forstmt::Stmt ::= f::For_t t::Type id::Id_t e::Expr body::Stmt labs::[ Id_t ] {
 production attribute dispatches :: [ Stmt ] with ++ ;
 dispatches := [ ];

 forwards to if	length (dispatches) == 1
		then head (dispatches)
--	     else if ! null (t.errors ++ e.errors ++ body.errors)
--		then erroneous_Stmt (forstmt, t.errors ++ e.errors ++ body.errors)
	     else if length (dispatches) > 1
		then erroneous_Stmt (forstmt, [ mkError (f.line,  "Internal Compiler Error:\n" ++ 
                     " Multiple enhanced for dispatchers defined on type " ++ t.typerep.eqName) ] )
             else erroneous_Stmt (forstmt, [ mkError (f.line,  "Internal Compiler Error:\n" ++ 
                     " No enhanced for dispatchers defined on type " ++ t.typerep.eqName) ] );

  forstmt.neededImportedSingleTypes = t.neededImportedSingleTypes ++ e.neededImportedSingleTypes ++ body.neededImportedSingleTypes; 

  forstmt.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ e.neededCurrentPackageTypes ++ body.neededCurrentPackageTypes;
  forstmt.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ e.neededImportedOnDemandTypes ++ body.neededImportedOnDemandTypes;
  forstmt.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes ++ body.neededFullyQualifiedTypes
					++ [ getQualifiedFQN ( getQualifiedFQN ( getSimpleFQN ("java"), "util"), "Collection") ]
					++ [ getQualifiedFQN ( getQualifiedFQN ( getSimpleFQN ("java"), "sql"), "ResultSet") ];
  forstmt.localTypes = [];
  forstmt.type_defs = [];
  forstmt.defs = [];
}

aspect production enhanced_for_dispatcher
forstmt::Stmt ::= f::For_t t::Type id::Id_t e::Expr body::Stmt labs::[ Id_t ] {
  local attribute isAnArray :: Boolean;
  isAnArray = e.typerep.isArrayType;

  local attribute isCollection :: Boolean;
  isCollection = subtype_check (e.typerep, retrieveTypeRep ("java.util.Collection", forstmt.type_env)).isSubType;

  dispatches <-	if isAnArray || isCollection
		then [ enhanced_for (f, t, id, e, body, labs) ]
		else [ ];
}

abstract production enhanced_for
forstmt::Stmt ::= f::For_t t::Type id::Id_t e::Expr body::Stmt labs::[ Id_t ] {
  forstmt.pp = "for (" ++  t.pp ++ " " ++ id.lexeme ++ ":" ++ e.pp ++ ")\n" ++ space (body.pp_indent) ++ body.pp;

  forstmt.errors := (if isAnArray || isCollection
		      then []
		      else [mkError (f.line, "Error: Expression in enhanced for loop must be an array or of type Collection")])
			++ t.errors ++ e.errors ++ body.errors ;
		    
  forstmt.neededImportedSingleTypes = t.neededImportedSingleTypes ++ e.neededImportedSingleTypes ++ body.neededImportedSingleTypes; 

  forstmt.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ e.neededCurrentPackageTypes ++ body.neededCurrentPackageTypes;
  forstmt.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ e.neededImportedOnDemandTypes ++ body.neededImportedOnDemandTypes;
  forstmt.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes ++ body.neededFullyQualifiedTypes
					++ [ getQualifiedFQN ( getQualifiedFQN ( getSimpleFQN ("java"), "util"), "Collection") ];
  forstmt.localTypes = [];
  forstmt.type_defs = [];
  forstmt.defs = [];

  local attribute isAnArray :: Boolean;
  isAnArray = e.typerep.isArrayType;

  local attribute isCollection :: Boolean;
  isCollection = subtype_check (e.typerep, retrieveTypeRep ("java.util.Collection", forstmt.type_env)).isSubType;

  forwards to if isAnArray
	      	then arrayTranslation''
	      else if isCollection
		then collectionTranslation''
	      else empty_stmt ();

  body.pp_indent = forstmt.pp_indent + 3;

  body.env = appendNewScope (newDefs, forstmt.env);
  e.env = appendNewScope (newDefs, forstmt.env);

  local attribute newDefs :: [ EnvItem ];
  newDefs = [ envItem ( id.lexeme, fully_qualified_name_none (), localDcl (id.lexeme, t.typerep) ) ]; --not resolved, since we are not recording this in the defs file

-- for (Cow c: h)
--	S

-- =>

-- for (Iterator __it = h.iterator (); __it.hasNext (); ) {
--	Cow c = __it.next ();
--	S
-- }

  local attribute collectionTranslation :: Stmt;
  collectionTranslation = applyLabels (labs, collectionLoop);

  local attribute collectionLoop :: Stmt;
  collectionLoop = for (collectionInit, collectionTest, collectionUpdate, collectionBody);

  local attribute collectionIterName :: String;
  collectionIterName = "__it" ++ toString (genInt ());

  local attribute collectionInit :: For_Init;
  collectionInit = for_init_dcl (local_var_dcl (getType ("Iterator"),
					 	var_declarators_one (var_declarator_init (var_declarator_id (terminal(Id_t, collectionIterName)),
						var_init_expr (expr_stmt_expr (expr_method_call (e, terminal (Id_t, "iterator"), 
											    exprs_none ())))))));

  local attribute collectionTest :: For_Test;
  collectionTest = for_test_one (expr_stmt_expr (expr_method_call (getExpr (collectionIterName), terminal(Id_t, "hasNext"), 
							      exprs_none())));

  local attribute collectionUpdate :: For_Update;
  collectionUpdate = for_update_empty ();

  local attribute collectionBody :: Stmt;
  collectionBody = stmt_block (block ( 	stmt_seq (stmt_dcl (local_var_dcl (
								t, 
								var_declarators_one (var_declarator_init (var_declarator_id (id), 
								var_init_expr (resolved_cast (t.typerep, expr_stmt_expr (expr_method_call (getExpr (collectionIterName), 
																terminal(Id_t, "next"), exprs_none ())))))))), 
					body)));

-- for (int j: js)
--	S

-- =>

-- int [] __ex = js;
-- for (int __it = 0; __it < __ex.length; __it++) {
--	int j = __ex [__it];
--	S
-- }

  local attribute arrayTranslation :: Stmt;
  arrayTranslation = stmt_seq (exprInit, applyLabels (labs, arrayLoop));

  local attribute arrayIterName :: String;
  arrayIterName = "__it" ++ toString (genInt ());

  local attribute arrayExprName :: String;
  arrayExprName = "__ex" ++ toString (genInt ());

  local attribute exprInit :: Stmt;
  exprInit = stmt_dcl (local_var_dcl (reference_type (array_type (primitive_array (int_type (), 1))), 
					  var_declarators_one (var_declarator_init (var_declarator_id (terminal (Id_t, arrayExprName)),
										    var_init_expr (e)))));

  local attribute arrayLoop :: Stmt;
  arrayLoop = for (arrayInit, arrayTest, arrayUpdate, arrayBody);

  local attribute arrayInit :: For_Init;
  arrayInit = for_init_dcl (local_var_dcl (primitive_type (int_type ()),
					   var_declarators_one (var_declarator_init (var_declarator_id (terminal(Id_t, arrayIterName)),
										     var_init_expr (int_const ("0"))))));

  local attribute arrayTest :: For_Test;
  arrayTest = for_test_one (lt (getExpr (arrayIterName), 
				expr_lhs (expr_field_access (getExpr (arrayExprName), terminal (Id_t, "length")))));

  local attribute arrayUpdate :: For_Update;
  arrayUpdate = for_update_some (stmt_exprs_one (post_inc (getExpr (arrayIterName))));

  local attribute arrayBody :: Stmt;
  arrayBody = stmt_block (block (stmt_seq (stmt_dcl (local_var_dcl (primitive_type (int_type ()), var_declarators_one (var_declarator_init (var_declarator_id (id), 
								var_init_expr (expr_lhs (array_access_general (getExpr (arrayExprName), getExpr (arrayIterName)))))))), 
				 body)));
}

function applyLabels
Stmt ::= labs::[ Id_t ] s::Stmt {
  return if null (labs)
	 then s
	 else label_prod (head (labs), applyLabels (tail (labs), s));
}
