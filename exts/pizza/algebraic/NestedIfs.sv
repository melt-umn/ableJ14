grammar edu:umn:cs:melt:ableJ14:exts:pizza:algebraic ;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:concretesyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax;

------------------------------------------------
-- Concrete Syntax
------------------------------------------------

nonterminal Algebraic_Switch_Block_c    with ast_algebraic_switch_block;
nonterminal Algebraic_Switch_Groups_c   with ast_algebraic_switch_groups;
nonterminal Algebraic_Switch_Group_c    with ast_algebraic_switch_group;
nonterminal Pattern_c                   with ast_pattern;
nonterminal Pattern_List_c              with ast_pattern_list;

synthesized attribute ast_algebraic_switch_block   :: Algebraic_Switch_Block;
synthesized attribute ast_algebraic_switch_groups  :: Algebraic_Switch_Groups;
synthesized attribute ast_algebraic_switch_group   :: Algebraic_Switch_Group;
synthesized attribute ast_pattern                  :: Pattern;
synthesized attribute ast_pattern_list             :: Pattern_List;

terminal aswitchTerm        'aswitch'      dominates {Id_t};

concrete production algebraic_switch_c
s::statement ::= t::aswitchTerm '(' e::expression ')' b::Algebraic_Switch_Block_c {
  s.ast_Stmt = algebraic_switch (t'', e.ast_Expr, b.ast_algebraic_switch_block);
}

concrete production algebraic_switch_block_c
s::Algebraic_Switch_Block_c ::= '{' gs::Algebraic_Switch_Groups_c '}' {
  s.ast_algebraic_switch_block = algebraic_switch_block (gs.ast_algebraic_switch_groups);
}

concrete production algebraic_switch_groups_one_c
sgs::Algebraic_Switch_Groups_c ::= sg::Algebraic_Switch_Group_c {
  sgs.ast_algebraic_switch_groups = algebraic_switch_groups_one (sg.ast_algebraic_switch_group);
}

concrete production algebraic_switch_groups_snoc_c
sgs::Algebraic_Switch_Groups_c ::= sgs1::Algebraic_Switch_Groups_c sg::Algebraic_Switch_Group_c {
  sgs.ast_algebraic_switch_groups = algebraic_switch_groups_snoc (sgs1.ast_algebraic_switch_groups, sg.ast_algebraic_switch_group);
}

-- changed blockStatementsOpt to blockStatements
concrete production algebraic_switch_group_c
sg::Algebraic_Switch_Group_c ::= 'case' p::Pattern_c ':' bs::blockStatements {
  sg.ast_algebraic_switch_group = algebraic_switch_group (p.ast_pattern, bs.ast_Stmt);
}

concrete production pattern_c
p::Pattern_c ::= n::Id_t '(' ps::Pattern_List_c ')' {
 p.ast_pattern = pattern (n, ps.ast_pattern_list);
}

concrete production pattern_no_vars_c
p::Pattern_c ::= n::Id_t {
 p.ast_pattern = pattern_no_vars (n);
}

concrete production pattern_default_c
sl::Pattern_c ::= d::Default_t {
  sl.ast_pattern = pattern_default (d);
}

concrete production pattern_list_one_c
pl::Pattern_List_c ::= p::Pattern_c {
 pl.ast_pattern_list = pattern_list_one (p.ast_pattern);
}

concrete production pattern_list_cons_c
pl::Pattern_List_c ::= p::Pattern_c ',' pltail::Pattern_List_c {
 pl.ast_pattern_list = pattern_list_cons (p.ast_pattern, pltail.ast_pattern_list);
}

------------------------------------------------
-- Abstract Syntax
------------------------------------------------

-- switch (this) {
--   case Cons (x, Cons (y, ys)):
--      [A]
--   case Cons (x, xs):
--      [B]
--   case Nil;
--      [C]
--   case default:
--      [D]
-- }

-- List temp = this;
-- boolean temp_done = false;
-- if !temp_done && temp.tag == Cons_tag {
--    char x = ((Cons) temp).head;
--    List temp2 = ((Cons) temp).tail;
--    if !temp_done && temp2.tag == Cons_tag {
--       char y = ((Cons) temp2).head;
--       List ys = ((Cons) temp2).tail;
--       temp_done = true;
--       [A]
--    }
-- }
-- if !temp_done && temp.tag == Cons_tag {
--    char x = ((Cons) temp).head;
--    List xs = ((Cons) temp).tail;
--    temp_done = true;
--    [B]
-- }
-- if !temp_done && temp.tag == Nil_tag {
--    temp_done = true;
--    [C]
-- }
-- if !temp_done {
--    temp_done = true;
--    [D]
-- }

nonterminal Algebraic_Switch_Block      with errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type, ast_Switch_Block;
nonterminal Algebraic_Switch_Groups     with errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type, ast_Switch_Groups;
nonterminal Algebraic_Switch_Group      with errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type, ast_Switch_Group;
nonterminal Pattern                     with errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type, ast_Switch_Label, ast_Stmt, line_no;
nonterminal Pattern_List                with errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type, ast_Stmt;

autocopy attribute switched_expr :: Expr;
attribute switched_expr occurs on Algebraic_Switch_Block, Algebraic_Switch_Groups,  Algebraic_Switch_Group, Pattern, Pattern_List;

autocopy attribute switched_type :: TypeRep;
attribute switched_type occurs on Algebraic_Switch_Block, Algebraic_Switch_Groups,  Algebraic_Switch_Group, Pattern;

autocopy attribute done_expr :: LHS;
attribute done_expr occurs on Algebraic_Switch_Block, Algebraic_Switch_Groups,  Algebraic_Switch_Group, Pattern, Pattern_List;

autocopy attribute set_done_true :: Stmt;
attribute set_done_true occurs on Algebraic_Switch_Block, Algebraic_Switch_Groups,  Algebraic_Switch_Group;

autocopy attribute switch_statement :: Stmt;
attribute switch_statement occurs on Pattern, Pattern_List;

inherited attribute variant_defs_inh :: [CaseParam] occurs on Pattern_List;

attribute ast_Stmt occurs on Algebraic_Switch_Block, Algebraic_Switch_Groups, Algebraic_Switch_Group;

abstract production algebraic_switch
switch::Stmt ::= t::aswitchTerm expr::Expr switchblock::Algebraic_Switch_Block {
 switch.pp = "algebraic switch (" ++ expr.pp ++ ") " ++ switchblock.pp;
 switch.errors := (case expr.typerep of
			classTypeRep (algebraic_class_type_rep (_,_,_,_,_,_,_,_,_,_,_)) -> [ :: Error ] |
                        _ ->  [ mkError (t.line, expr.pp ++ " is not an algebraic datatype") ]
		   end) ++ switchblock.errors;
 switch.defs = [];

 local attribute temp_switched_name :: String;
 temp_switched_name = "temp__" ++ toString (genInt ());

 local attribute temp_switched_expr :: Expr;
 temp_switched_expr = getExpr (temp_switched_name);

 local attribute temp_switched_dcl :: Stmt;
 temp_switched_dcl = stmt_dcl (local_var_dcl (
                                        type_typerep (expr.typerep),
                                        var_declarators_one (var_declarator_init (
                                                        var_declarator_id (terminal (Id_t, temp_switched_name)),
                                                        var_init_expr (expr'')))));

 local attribute temp_done_name :: String;
 temp_done_name = temp_switched_name ++ "__done";

 local attribute temp_done_expr :: LHS;
 temp_done_expr = getLHS (temp_done_name);

 local attribute temp_done_dcl :: Stmt;
 temp_done_dcl = stmt_dcl (local_var_dcl (
                                        primitive_type (boolean_type ()),
                                        var_declarators_one (var_declarator_init (
                                                        var_declarator_id (terminal (Id_t, temp_done_name)),
                                                        var_init_expr (false_const ())))));

 local attribute temp_done_true :: Stmt;
 temp_done_true = stmt_stmt_expr (assign ( temp_done_expr'',
                                           terminal (Eq_t, "="),
                                           true_const ()));

 switchblock.switched_expr = temp_switched_expr'';
 switchblock.switched_type = expr.typerep;
 switchblock.done_expr = temp_done_expr'';
 switchblock.set_done_true = temp_done_true'';

 forwards to stmt_seq ( temp_switched_dcl'',
             stmt_seq ( temp_done_dcl'',
                        switchblock.ast_Stmt));


 switch.neededImportedSingleTypes = [];
 switch.neededCurrentPackageTypes = [];
 switch.neededImportedOnDemandTypes = [];
 switch.neededFullyQualifiedTypes = [];
}

abstract production algebraic_switch_block
switchblock::Algebraic_Switch_Block ::= gs::Algebraic_Switch_Groups {
 switchblock.pp = "{\n" ++ space (gs.pp_indent) ++ gs.pp ++ "\n" ++ space (switchblock.pp_indent) ++ "}";
 switchblock.basepp = "{\n" ++ space (gs.pp_indent) ++ gs.basepp ++ "\n" ++ space (switchblock.pp_indent) ++ "}";
 switchblock.errors := gs.errors;

 switchblock.ast_Stmt = gs.ast_Stmt;

 gs.pp_indent = switchblock.pp_indent + 3;
}

abstract production algebraic_switch_groups_one
sgs::Algebraic_Switch_Groups ::= sg::Algebraic_Switch_Group {
 sgs.pp = sg.pp;
 sgs.basepp = sg.basepp;
 sgs.errors := sg.errors;

 sgs.ast_Stmt = sg.ast_Stmt;
}

abstract production algebraic_switch_groups_snoc
sgs::Algebraic_Switch_Groups ::= list::Algebraic_Switch_Groups item::Algebraic_Switch_Group {
 sgs.pp = list.pp ++ "\n" ++ space (item.pp_indent) ++ item.pp;
 sgs.basepp = list.basepp ++ "\n" ++ space (item.pp_indent) ++ item.basepp;
 sgs.errors := list.errors ++ item.errors;

 sgs.ast_Stmt = stmt_seq (list.ast_Stmt, item.ast_Stmt);
}

abstract production algebraic_switch_group
switch::Algebraic_Switch_Group ::= p::Pattern stmts::Stmt {
 switch.pp = "case " ++ p.pp ++ " :\n" ++ space (stmts.pp_indent) ++ stmts.pp;
 switch.basepp = p.basepp ++ " :\n" ++ space (stmts.pp_indent) ++ stmts.basepp;
 switch.errors := p.errors;

 switch.ast_Stmt = p.ast_Stmt;
 p.switch_statement = stmt_seq (switch.set_done_true, stmts'');

 stmts.pp_indent = switch.pp_indent + 3;
}

abstract production pattern
p::Pattern ::= n::Id_t pl::Pattern_List {
 p.pp = n.lexeme ++ "(" ++ pl.pp ++ ")";
 p.basepp = n.lexeme ++ "(" ++ pl.basepp ++ ")";
 p.errors := (if null (pattern_result)
                then [mkError (n.line, "Unknown pattern " ++ n.lexeme)]
                else [])
                ++ pl.errors;
 p.line_no = n.line;

 p.ast_Stmt = if_then (	 terminal (If_t, "if"),
                                 and_and (not (expr_lhs (p.done_expr)),
                                      eq (expr_lhs (expr_field_access (p.switched_expr, terminal (Id_t, "tag"))),
                                          expr_lhs (lhs_name (qualified_expr_name (simple_ambiguous_name (terminal (Id_t, p.switched_type.eqName)), terminal (Id_t, n.lexeme ++ "_tag")))))),
                                 stmt_block (block (stmt_seq (temp_switched_dcl'', pl.ast_Stmt))));

 local attribute temp_switched_name :: String;
 temp_switched_name = "temp__" ++ toString (genInt ());

 local attribute temp_switched_expr :: Expr;
 temp_switched_expr = getExpr (temp_switched_name);

 local attribute temp_switched_dcl :: Stmt;
 temp_switched_dcl = stmt_dcl (local_var_dcl (
                                              getType (n.lexeme),
                                              var_declarators_one (var_declarator_init (
                                                     var_declarator_id (terminal (Id_t, temp_switched_name)),
                                                     var_init_expr (cast_prod (getTypeName (n.lexeme), p.switched_expr))))));

 pl.switched_expr = temp_switched_expr'';
 pl.variant_defs_inh = if null (pattern_result) then [] else (head (pattern_result)).variant_defs;

 local attribute pattern_result :: [CaseRep];
 pattern_result = case p.switched_type of
			classTypeRep (algebraic_class_type_rep (_,_,_,_,_,_,_,_,_,_,_)) -> lookupPattern (n.lexeme, p.switched_type.classtyperep.caseReps) |
			_ -> [ :: CaseRep ]
		  end;
}

--   case x:
--      [C]
--  ==>
-- if !temp_done {
--    T x = temp;
--    temp_done = true;
--    [C]
-- }

--   case Nil:
--      [C]
--  ==>
-- if !temp_done && temp.tag == T.Nil_tag {
--    temp_done = true;
--    [C]
-- }

abstract production pattern_no_vars
p::Pattern ::= n::Id_t {
 p.pp = n.lexeme;
 p.basepp = "case " ++ n.lexeme;
 p.errors := if null (pattern_result)
                then [mkError (n.line, "Unknown pattern " ++ n.lexeme)]
                else [];
 p.line_no = n.line;

 p.ast_Stmt = if null (pattern_result)
                   -- n is a variable
                   then (if_then (terminal (If_t, "if"),
                                  not (expr_lhs (p.done_expr)),
                                  stmt_block (block (stmt_seq (
					stmt_dcl (local_var_dcl (
                                        type_typerep (p.switched_type),
                                        var_declarators_one (var_declarator_init (
                                                        var_declarator_id (n),
                                                        var_init_expr (p.switched_expr))))),
                                            p.switch_statement)))))

                   else if_then (terminal (If_t, "if"),
                                 and_and (not (expr_lhs (p.done_expr)),
                                      eq (expr_lhs (expr_field_access (p.switched_expr, terminal (Id_t, "tag"))),
                                          expr_lhs (lhs_name (qualified_expr_name (simple_ambiguous_name (terminal (Id_t, p.switched_type.eqName)), terminal (Id_t, n.lexeme ++ "_tag")))))),
                                 stmt_block (block (p.switch_statement)));

 local attribute pattern_result :: [CaseRep];
 pattern_result = case p.switched_type of
			classTypeRep (algebraic_class_type_rep (_,_,_,_,_,_,_,_,_,_,_)) -> lookupPattern (n.lexeme, p.switched_type.classtyperep.caseReps) |
			_ -> [ :: CaseRep ]
		  end;
}

--   case default:
--      [D]
-- ==>
-- if !temp_done {
--    temp_done = true;
--    [D]
-- }

abstract production pattern_default
p::Pattern ::= d::Default_t {
 p.pp = "case default";
 p.basepp = "case default";
 p.errors := [];
 p.line_no = d.line;

 p.ast_Stmt = if_then (terminal (If_t, "if"),
                            not (expr_lhs (p.done_expr)),
                            stmt_block (block (p.switch_statement)));
}

abstract production pattern_list_one
pl::Pattern_List ::= p::Pattern {
 pl.pp = p.pp;
 pl.basepp = p.basepp;
 pl.errors := if length (pl.variant_defs_inh) == 1
                then []
                else [mkError (p.line_no, "Incorrect number of parameters in pattern")];

 pl.ast_Stmt = p.ast_Stmt;
 p.switched_expr = expr_lhs (expr_field_access (pl.switched_expr, my_variant_def.name_id));
 p.switched_type = my_variant_def.typerep;

 local attribute my_variant_def :: CaseParam;
 my_variant_def = if null (pl.variant_defs_inh)
                  then error_case_param ()
                  else head (pl.variant_defs_inh);
}

abstract production pattern_list_cons
pl::Pattern_List ::= p::Pattern pltail::Pattern_List {
 pl.pp = p.pp ++ ", " ++ pltail.pp ;
 pl.basepp = p.basepp ++ ", " ++ pltail.basepp ;
 pl.errors := if length (pl.variant_defs_inh) > 0
                then pltail.errors
                else [mkError (p.line_no, "Incorrect number of parameters in pattern")];

 pl.ast_Stmt = p.ast_Stmt;
 p.switch_statement = pltail.ast_Stmt;
 p.switched_expr = expr_lhs (expr_field_access (pl.switched_expr, my_variant_def.name_id));
 p.switched_type = my_variant_def.typerep;

 local attribute my_variant_def :: CaseParam;
 my_variant_def = if null (pl.variant_defs_inh)
                  then error_case_param ()
                  else head (pl.variant_defs_inh);

 pltail.variant_defs_inh = if null (pl.variant_defs_inh)
                                  then []
                                  else tail (pl.variant_defs_inh);
}
