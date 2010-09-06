grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:exts:foreach;

aspect production enhanced_for_dispatcher
forstmt::Stmt ::= f::For_t t::Type id::Id_t e::Expr body::Stmt labs::[ Id_t ] {
  local attribute isResultSet :: Boolean;
  isResultSet = subtype_check (e.typerep, retrieveTypeRep ("java.sql.ResultSet", forstmt.type_env)).isSubType;

  dispatches <-	if isResultSet
		then [ enhanced_for_sql (f, t, id, e, body, labs) ]
		else [ ];
}

abstract production enhanced_for_sql
forstmt::Stmt ::= f::For_t t::Type id::Id_t e::Expr body::Stmt labs::[ Id_t ] {
  forstmt.pp = "for (" ++  t.pp ++ " " ++ id.lexeme ++ ":" ++ e.pp ++ ")\n" ++ space (body.pp_indent) ++ body.pp;

  forstmt.errors := (if isResultSet
		      then []
		      else [mkError (f.line, "Error: Expression in enhanced for loop must be of type ResultSet")])
			++ t.errors ++ e.errors ++ body.errors ;
		    
  forstmt.neededImportedSingleTypes = t.neededImportedSingleTypes ++ e.neededImportedSingleTypes ++ body.neededImportedSingleTypes; 

  forstmt.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ e.neededCurrentPackageTypes ++ body.neededCurrentPackageTypes;
  forstmt.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ e.neededImportedOnDemandTypes ++ body.neededImportedOnDemandTypes;
  forstmt.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes ++ body.neededFullyQualifiedTypes
					++ [ getQualifiedFQN ( getQualifiedFQN ( getSimpleFQN ("java"), "sql"), "ResultSet") ];
  forstmt.localTypes = [];
  forstmt.type_defs = [];
  forstmt.defs = [];

  local attribute isResultSet :: Boolean;
  isResultSet = subtype_check (e.typerep, retrieveTypeRep ("java.sql.ResultSet", forstmt.type_env)).isSubType;

  forwards to if isResultSet
		then resultSetTranslation''
	      else empty_stmt ();

  body.pp_indent = forstmt.pp_indent + 3;

  body.env = appendNewScope (newDefs, forstmt.env);
  e.env = appendNewScope (newDefs, forstmt.env);

  local attribute newDefs :: [ EnvItem ];
  newDefs = [ envItem ( id.lexeme, fully_qualified_name_none (), localDcl (id.lexeme, t.typerep) ) ]; --not resolved, since we are not recording this in the defs file

-- for (ResultSet r: rs)
--	S

-- =>

-- while (rs.next ()) {
--      ResultSet r = rs;
--      S
-- }

  local attribute resultSetTranslation :: Stmt;
  resultSetTranslation = applyLabels (labs, resultSetLoop);

  local attribute resultSetLoop :: Stmt;
  resultSetLoop = while_prod ('while', resultSetTest, resultSetBody);

  local attribute resultSetTest :: Expr;
  resultSetTest = expr_stmt_expr (expr_method_call (e, terminal(Id_t, "next"), exprs_none()));

  local attribute resultSetBody :: Stmt;
  resultSetBody = stmt_block (block ( 	stmt_seq (stmt_dcl (local_var_dcl (
								t, 
								var_declarators_one (var_declarator_init (var_declarator_id (id), var_init_expr (e))))),
						  body)));
}
