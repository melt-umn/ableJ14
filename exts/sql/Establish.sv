grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:abstractsyntax;

abstract production establish_a
s::Stmt ::= c::Id_t
{
 s.pp = "establish " ++ c.lexeme ++ ";"  ;

 local attribute idSearchResult :: [ DclInfo ] ;
 idSearchResult = lookupIdOneScope (c, s.env);

 local attribute firstId :: DclRep ;
 firstId = (head (idSearchResult)).dclrep ;

 local attribute u :: String;
 u = if null (idSearchResult) then "\"CONNECTION NOT FOUND\""
	else case firstId.typerep of
		connectionTypeRep (n, fqn, u2, td, tr) -> u2 |
		_  -> "\"STRANGE CONNECTION\""
	     end;

 s.errors := if null (idSearchResult) then [ mkError (c.line, c.lexeme ++ " is not bound in the environment") ]
	     else case firstId.typerep of
		connectionTypeRep (_, _, _, _, _) -> [ ] |
		_  -> [ mkError (c.line, c.lexeme ++ " is not a connection") ]
	     end;


 forwards to stmt_stmt_expr (assign (
				getLHS(c.lexeme), 
				terminal(Eq_t,"="),
				expr_stmt_expr (method_call (
							qualified_method_name (mk_ambig_name (["java","sql","DriverManager"]),
										terminal(Id_t, "getConnection")), 
							exprs_one(string_const(u))))));

-- forwards to '' c = java.sql.DriverManager.getConnection("jdbc:derby:/project/melt/People/evw/Melt/GrammarSpace/edu/umn/cs/melt/java14/demos/derby/db/testdb;create=true"); 

 s.neededImportedSingleTypes = [];
 s.neededCurrentPackageTypes = [];
 s.neededImportedOnDemandTypes = [];
 s.neededFullyQualifiedTypes = [ getQualifiedFQN (getQualifiedFQN ( getSimpleFQN ("java"), "sql"), "DriverManager") ];
}

