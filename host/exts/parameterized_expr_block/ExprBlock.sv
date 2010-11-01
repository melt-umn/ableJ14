grammar edu:umn:cs:melt:ableJ14:host:exts:parameterized_expr_block ;

import edu:umn:cs:melt:ableJ14:terminals ;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:concretesyntax hiding parse;

-- I've added the return type to the syntax, since we cannot infer the type from the expression.
-- There would be a loop between the defs generated from the method header, and the environment
-- required to lookup the type of the expressions -- LK

--  ({ (boolean p1 = c1, boolean p2 = c2, boolean p3 = c3)
--     Stmts;
--     return {Type} ddd; })
--
--  ==>
--     (new Object() { public int doit(boolean p1, boolean p2, boolean p3){ Stmts; return ddd; } } ).doit(c1,c2,c3);

-- The variables in the initializing expressions are not in scope in the body of statements or return expression.

terminal ParenCurlyLeft_t  '({'  precedence = 10 ;
terminal ParenCurlyRight_t '})'  precedence = 10 ;

-- changed blockStatementsOpt to blockStatements
concrete production exprBlock_c
e::primaryExpression ::= '({' '(' params::ExprBlockParams_c ')' bs::blockStatements 'return' '{' returnType::type '}' re::expression ';' '})' 
{
  e.ast_Expr = exprBlock ( params.ast_ExprBlockParams, bs.ast_Stmt, returnType.ast_Type, re.ast_Expr ) ;
}

nonterminal ExprBlockParam_c with ast_ExprBlockParam;
nonterminal ExprBlockParams_c with ast_ExprBlockParams;
synthesized attribute  ast_ExprBlockParam :: ExprBlockParam ;
synthesized attribute  ast_ExprBlockParams :: ExprBlockParams ;

concrete production exprBlockParam_one_c
ps::ExprBlockParams_c ::= p::ExprBlockParam_c 
{
 ps.ast_ExprBlockParams = exprBlockParam_one (p.ast_ExprBlockParam) ;
}
concrete production exprBlockParam_snoc_c
ps::ExprBlockParams_c ::=   ps2::ExprBlockParams_c ','  p::ExprBlockParam_c
{
 ps.ast_ExprBlockParams = exprBlockParam_snoc (ps2.ast_ExprBlockParams, p.ast_ExprBlockParam) ;
}

concrete production exprBlockParam_c
p::ExprBlockParam_c ::= t::type v::variableDeclaratorId '=' init::initializer 
{
 p.ast_ExprBlockParam = exprBlockParam (t.ast_Type, v.ast_Var_Declarator_Id, init.ast_Var_Init);
}




--- Abstract Syntax ---
-----------------------

nonterminal ExprBlockParam with pp, errors, exprBlockParamExpr, exprBlockFormal, defs, 
					neededImportedSingleTypes, neededCurrentPackageTypes, neededImportedOnDemandTypes, neededFullyQualifiedTypes;
nonterminal ExprBlockParams  with pp, errors, exprBlockParamExprs, exprBlockFormals,defs, 
					neededImportedSingleTypes, neededCurrentPackageTypes, neededImportedOnDemandTypes, neededFullyQualifiedTypes;
synthesized attribute exprBlockParamExpr  :: Expr ;
synthesized attribute exprBlockParamExprs :: Exprs ;
synthesized attribute exprBlockFormal  :: Formal_Param ;
synthesized attribute exprBlockFormals :: Formal_Params ;


abstract production exprBlock
e::Expr ::= params::ExprBlockParams bs::Stmt returnType::Type re::Expr
{
 e.pp = "({ (" ++ params.pp ++ " " ++ bs.pp ++ " return {" ++ returnType.pp ++ "}" ++ re.pp ++ "; })" ;
 e.errors := params.errors ++ bs.errors ++ re.errors ;
 e.typerep = returnType.typerep;

 bs.env = [ scopeEnv (3, params.defs) ]; 
 re.env = [ scopeEnv (3, bs.defs ++ params.defs) ];

 bs.pp_indent = 0 ;

 forwards to 
--     (new Object() { public int doit(boolean p1, boolean p2, boolean p3){ Stmts; return ddd; } } ).doit(c1,c2,c3);

   expr_stmt_expr (
     expr_method_call (
       expr_stmt_expr (new_class_body 	(qualified_type_name (qualified_package_or_type_name (simple_package_or_type_name (terminal (Id_t, "java")), terminal (Id_t, "lang")), terminal(Id_t,"Object")),
                      			 exprs_none(),
					 class_body (class_member_dcls_one (class_method (
						method_dcl_prod (
				                           -- Method_Header
				                           method_header_prod ( modifiers_one (public()),
										returnType'',
										terminal(Id_t,"doit"),
										params.exprBlockFormals,
										throws_none() ),
                				           -- Block
				                           block(bs''))))))),
	terminal(Id_t,"doit") ,
	params.exprBlockParamExprs));

 e.neededImportedSingleTypes = params.neededImportedSingleTypes ++ bs.neededImportedSingleTypes ++ returnType.neededImportedSingleTypes ++ re.neededImportedSingleTypes;
 e.neededCurrentPackageTypes = params.neededCurrentPackageTypes ++ bs.neededCurrentPackageTypes ++ returnType.neededCurrentPackageTypes ++ re.neededCurrentPackageTypes;
 e.neededImportedOnDemandTypes = params.neededImportedOnDemandTypes ++ bs.neededImportedOnDemandTypes ++returnType.neededImportedOnDemandTypes++re.neededImportedOnDemandTypes;
 e.neededFullyQualifiedTypes = [ getQualifiedFQN ( getQualifiedFQN ( getSimpleFQN ("java"), "lang"), "Object") ] ++ params.neededFullyQualifiedTypes ++ bs.neededFullyQualifiedTypes ++ returnType.neededFullyQualifiedTypes ++ re.neededFullyQualifiedTypes;
} 


abstract production exprBlockParam_snoc
ps::ExprBlockParams ::= ps2::ExprBlockParams  p::ExprBlockParam
{
 ps.pp = ps2.pp ++ ", " ++ p.pp ;
 ps.errors := ps2.errors ++ p.errors;
 ps.exprBlockParamExprs = exprs_snoc (ps2.exprBlockParamExprs, p.exprBlockParamExpr );
 ps.exprBlockFormals = formal_params_snoc (ps2.exprBlockFormals, p.exprBlockFormal );
 ps.defs = ps2.defs ++ p.defs;

 ps.neededImportedSingleTypes = ps2.neededImportedSingleTypes ++ p.neededImportedSingleTypes;
 ps.neededCurrentPackageTypes = ps2.neededCurrentPackageTypes ++ p.neededCurrentPackageTypes;
 ps.neededImportedOnDemandTypes = ps2.neededImportedOnDemandTypes ++ p.neededImportedOnDemandTypes;
 ps.neededFullyQualifiedTypes = ps2.neededFullyQualifiedTypes ++ p.neededFullyQualifiedTypes;
}
abstract production exprBlockParam_one
ps::ExprBlockParams ::= p::ExprBlockParam 
{
 ps.pp = p.pp ;
 ps.errors := p.errors;
 ps.exprBlockParamExprs = exprs_one (p.exprBlockParamExpr);
 ps.exprBlockFormals = formal_params_one (p.exprBlockFormal );
 ps.defs = p.defs;

 ps.neededImportedSingleTypes = p.neededImportedSingleTypes;
 ps.neededCurrentPackageTypes = p.neededCurrentPackageTypes;
 ps.neededImportedOnDemandTypes = p.neededImportedOnDemandTypes;
 ps.neededFullyQualifiedTypes = p.neededFullyQualifiedTypes;
}

abstract production exprBlockParam
p::ExprBlockParam ::= t::Type v::Var_Declarator_Id init::Var_Init 
{
 p.pp = t.pp ++ " " ++ v.pp ++ " = " ++ init.pp ;
 p.errors := t.errors ++ init.errors ;
 p.exprBlockParamExpr = init.exprBlockParamExpr ;
 p.exprBlockFormal = formal_param(t'',v'') ;

 p.defs = [ envItem (v.name, getSimpleFQN ("NoFQN"), paramDcl (v.name, t.typerep)) ];

 p.neededImportedSingleTypes = t.neededImportedSingleTypes ++ v.neededImportedSingleTypes ++ init.neededImportedSingleTypes;
 p.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ v.neededCurrentPackageTypes ++ init.neededCurrentPackageTypes;
 p.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ v.neededImportedOnDemandTypes ++ init.neededImportedOnDemandTypes;
 p.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ v.neededFullyQualifiedTypes ++ init.neededFullyQualifiedTypes;
}

attribute exprBlockParamExpr occurs on Var_Init ;
aspect production var_init_expr
init::Var_Init ::= e::Expr {
  init.exprBlockParamExpr = e'' ;
}


