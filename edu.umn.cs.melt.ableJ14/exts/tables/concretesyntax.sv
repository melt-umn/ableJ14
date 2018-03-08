grammar edu:umn:cs:melt:ableJ14:exts:tables ;

import edu:umn:cs:melt:ableJ14:terminals ;
import edu:umn:cs:melt:ableJ14:concretesyntax hiding parse ;

--Sample Table
--
--     c4 = table (
--         c1 : T F ,
--         c2 : F * ,
--         c3 : T T );


-- Concrete Syntax --
---------------------
nonterminal TableRows_c with ast_TableRows;
nonterminal TableRow_c  with ast_TableRow;

synthesized attribute ast_TableRows :: TableRows ;
synthesized attribute ast_TableRow  :: TableRow ;

terminal Table_t 'table' dominates { Id_t };--, prefix = true;

concrete production table_c
t::PrimaryExpression ::=  'table' '(' trows::TableRows_c ')' 
{ t.ast_Expr = table (trows.ast_TableRows); 
}

concrete production tableRowSnoc_c
trows::TableRows_c ::= trowstail::TableRows_c ',' trow::TableRow_c 
{ trows.ast_TableRows = tableRowSnoc (trowstail.ast_TableRows, trow.ast_TableRow); 
}

concrete production tableRowOne_c
trows::TableRows_c ::= trow::TableRow_c
{ trows.ast_TableRows = tableRowOne (trow.ast_TableRow); 
}

concrete production tableRow_c
trow::TableRow_c ::= e::Expression ':'  tvs::TruthValueList_c
{ trow.ast_TableRow = tableRow(e.ast_Expr, tvs) ; 
}

--Truth Value List
-----------------------
nonterminal TruthValueList_c ; 

concrete production tvlistCons
tvl::TruthValueList_c ::=  tv::TruthValue_c  tvltail::TruthValueList_c { }

concrete production tvlistOne
tvl::TruthValueList_c ::=  tv::TruthValue_c { }



-- Truth Values
---------------
terminal TrueTV_t   'T'  ; 
terminal FalseTV_t  'F'  ; 

nonterminal TruthValue_c ; 

concrete production tvTrue
tv::TruthValue_c ::= truetv::TrueTV_t { }

concrete production tvFalse
tv::TruthValue_c ::= falsetv::FalseTV_t { }

concrete production tvStar
tv::TruthValue_c ::= startv::Mul_t { }

