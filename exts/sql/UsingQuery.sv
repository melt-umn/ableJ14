grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:terminals ;
import edu:umn:cs:melt:ableJ14:abstractsyntax;


abstract production a_using_query
top::Expr ::= c::Id_t s::SQL_Stmt
{
 top.pp = "using " ++ c.lexeme ++ " query { " ++ s.pp ++ " } " ;

 local attribute idSearchResult :: [ DclInfo ] ;
 idSearchResult = lookupIdOneScope (c, top.env);

 local attribute firstId :: DclRep ;
 firstId = (head (idSearchResult)).dclrep ;

 local attribute td :: [Decorated TableBinding];
 td = case firstId.typerep of
	connectionTypeRep (n, fqn, u, td2, tr) -> td2 |
	_  -> [ ::Decorated TableBinding ]
      end;

 s.tableDefs = if null (idSearchResult) then [] else td;

 top.errors :=
    (if   null (idSearchResult) 
     then [ mkError (c.line, c.lexeme ++ " is not bound in the environment") ]
     else case firstId.typerep of
            connectionTypeRep (_, _, _, _, _) -> [ :: Error ] |
            _  -> [ mkError (c.line, c.lexeme ++ " is not a Connection") ]
          end
    ) ++
    s.sqlErrors;

 local attribute connectionTR :: TypeRep;
 connectionTR = retrieveTypeRep ("java.sql.Connection", top.type_env);

 local attribute connectionExpression :: Expr;
 connectionExpression 
  = expr_lhs (bound_expr_id (c, 
                      localDcl (c.lexeme, connectionTR), connectionTR));

 forwards to   -- c.createStatement().executeQuery( s.baseJava ) 
   expr_stmt_expr(
      expr_method_call(
        expr_stmt_expr(
          expr_method_call(
            connectionExpression,
            terminal(Id_t, "createStatement"), 
            exprs_none() 
        )),
        terminal(Id_t, "executeQuery"),
        exprs_one(s.baseJava)
   )); 


 top.neededImportedSingleTypes = [];
 top.neededCurrentPackageTypes = [];
 top.neededImportedOnDemandTypes = [];
 top.neededFullyQualifiedTypes = [ getQualifiedFQN (getQualifiedFQN ( getSimpleFQN ("java"), "sql"), "Connection") ];
} 
