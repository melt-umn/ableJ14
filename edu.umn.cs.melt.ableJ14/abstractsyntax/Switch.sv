grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;

nonterminal Switch_Block with  errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type;
nonterminal Switch_Groups with errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type;
nonterminal Switch_Group with  errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type;
nonterminal Switch_Labels with errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type;
nonterminal Switch_Label with  errors, env, type_env, pp, basepp, pp_indent, enclosingType, my_return_type;

abstract production switch_prod
switch::Stmt ::= expr::Expr switchblock::Switch_Block {
  switch.pp = "switch (" ++ expr.pp ++ ") " ++ switchblock.pp;
  switch.basepp = "switch (" ++ expr.basepp ++ ") " ++ switchblock.basepp;
  switch.errors := expr.errors ++ switchblock.errors;
  switch.defs = [];
  switch.type_defs = [];
}

abstract production switch_block
switchblock::Switch_Block ::= gs::Switch_Groups ls::Switch_Labels {
  switchblock.pp = "{\n" ++ space (gs.pp_indent) ++ gs.pp ++ "\n" ++ space (ls.pp_indent) ++ ls.pp ++ "\n" ++ space (switchblock.pp_indent) ++ "}";
  switchblock.basepp = "{\n" ++ space (gs.pp_indent) ++ gs.basepp ++ "\n" ++ space (ls.pp_indent) ++ ls.basepp ++ "\n" ++ space (switchblock.pp_indent) ++ "}";
  switchblock.errors := gs.errors ++ ls.errors;

  gs.pp_indent = switchblock.pp_indent + 3;
}

abstract production switch_block_no_labels
switchblock::Switch_Block ::= s::Switch_Groups {
  switchblock.pp = "{\n" ++ space (s.pp_indent) ++ s.pp ++ "\n" ++ space (switchblock.pp_indent) ++ "}";
  switchblock.basepp = "{\n" ++ space (s.pp_indent) ++ s.basepp ++ "\n" ++ space (switchblock.pp_indent) ++ "}";
  switchblock.errors := s.errors;

  s.pp_indent = switchblock.pp_indent + 3;
}

abstract production switch_block_no_groups
switchblock::Switch_Block ::= s::Switch_Labels {
  switchblock.pp = "{\n" ++ space (s.pp_indent) ++ s.pp ++ "\n" ++ space (switchblock.pp_indent) ++ "}";
  switchblock.basepp = "{\n" ++ space (s.pp_indent) ++ s.basepp ++ "\n" ++ space (switchblock.pp_indent) ++ "}";
  switchblock.errors := s.errors;

  s.pp_indent = switchblock.pp_indent + 3;
}

abstract production switch_block_empty
switchblock::Switch_Block ::= {
  switchblock.pp = "{}";
  switchblock.basepp = "{}";
  switchblock.errors := [];
}

abstract production switch_groups_one
switch::Switch_Groups ::= item::Switch_Group {
  switch.pp = item.pp;
  switch.basepp = item.basepp;
  switch.errors := item.errors;
}

abstract production switch_groups_snoc
switch::Switch_Groups ::= list::Switch_Groups item::Switch_Group {
  switch.pp = list.pp ++ "\n" ++ space (item.pp_indent) ++ item.pp;
  switch.basepp = list.basepp ++ "\n" ++ space (item.pp_indent) ++ item.basepp;
  switch.errors := list.errors ++ item.errors;
}

abstract production switch_group
switch::Switch_Group ::= label_list::Switch_Labels stmts::Stmt {
  switch.pp = label_list.pp ++ "\n" ++ space (stmts.pp_indent) ++ stmts.pp;
  switch.basepp = label_list.basepp ++ "\n" ++ space (stmts.pp_indent) ++ stmts.basepp;
  switch.errors := label_list.errors ++ stmts.errors;

  stmts.pp_indent = switch.pp_indent + 3;
}

abstract production switch_labels_one
switchlabels::Switch_Labels ::= switchlabel::Switch_Label {
  switchlabels.pp = switchlabel.pp;
  switchlabels.basepp = switchlabel.basepp;
  switchlabels.errors := switchlabel.errors;
}

abstract production switch_labels_snoc
switchlabels::Switch_Labels ::=  list::Switch_Labels item::Switch_Label {
  switchlabels.pp = list.pp ++ "\n" ++ space (item.pp_indent) ++ item.pp;
  switchlabels.basepp = list.basepp ++ "\n" ++ space (item.pp_indent) ++ item.basepp;
  switchlabels.errors := list.errors ++ item.errors;
}

abstract production switch_label
label::Switch_Label ::= expr::Expr {
  label.pp = "case " ++ expr.pp ++ ": ";
  label.basepp = "case " ++ expr.basepp ++ ": ";
  label.errors := expr.errors;
}

abstract production switch_label_default
label::Switch_Label ::= {
  label.pp = "default: ";
  label.basepp = "default: ";
  label.errors := [];
}
