grammar edu:umn:cs:melt:ableJ14:exts:pizza:algebraic ;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:terminals;

attribute availableLocalTypes, availableImportedSingleTypes, availableCurrentPackageTypes, availableImportedOnDemandTypes, thisPackage, qualifiersSoFar occurs on Algebraic_Switch_Block, Algebraic_Switch_Groups, Algebraic_Switch_Group, Pattern, Pattern_List;
attribute neededImportedSingleTypes, neededCurrentPackageTypes, neededImportedOnDemandTypes, localTypes, neededFullyQualifiedTypes occurs on Algebraic_Switch_Block, Algebraic_Switch_Groups, Algebraic_Switch_Group;

------

aspect production algebraic_switch
switch::Stmt ::= t::aswitchTerm expr::Expr switchblock::Algebraic_Switch_Block {
-- switch.neededImportedSingleTypes = expr.neededImportedSingleTypes ++ switchblock.neededImportedSingleTypes;
}

aspect production algebraic_case_class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.neededImportedSingleTypes = cb.neededImportedSingleTypes;
}

aspect production algebraic_switch_block
switchblock::Algebraic_Switch_Block ::= gs::Algebraic_Switch_Groups {
 switchblock.neededImportedSingleTypes = gs.neededImportedSingleTypes;
}

aspect production algebraic_switch_groups_one
sgs::Algebraic_Switch_Groups ::= sg::Algebraic_Switch_Group {
 sgs.neededImportedSingleTypes = sg.neededImportedSingleTypes;
}

aspect production algebraic_switch_groups_snoc
sgs::Algebraic_Switch_Groups ::= list::Algebraic_Switch_Groups item::Algebraic_Switch_Group {
 sgs.neededImportedSingleTypes = list.neededImportedSingleTypes ++ item.neededImportedSingleTypes;
}

aspect production algebraic_switch_group
switch::Algebraic_Switch_Group ::= p::Pattern stmts::Stmt {
 switch.neededImportedSingleTypes = stmts.neededImportedSingleTypes;
}

------------

aspect production algebraic_switch
switch::Stmt ::= t::aswitchTerm expr::Expr switchblock::Algebraic_Switch_Block {
-- switch.neededCurrentPackageTypes = expr.neededCurrentPackageTypes ++ switchblock.neededCurrentPackageTypes;
}

aspect production algebraic_case_class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.neededCurrentPackageTypes = cb.neededCurrentPackageTypes;
}

aspect production algebraic_switch_block
switchblock::Algebraic_Switch_Block ::= gs::Algebraic_Switch_Groups {
 switchblock.neededCurrentPackageTypes = gs.neededCurrentPackageTypes;
}

aspect production algebraic_switch_groups_one
sgs::Algebraic_Switch_Groups ::= sg::Algebraic_Switch_Group {
 sgs.neededCurrentPackageTypes = sg.neededCurrentPackageTypes;
}

aspect production algebraic_switch_groups_snoc
sgs::Algebraic_Switch_Groups ::= list::Algebraic_Switch_Groups item::Algebraic_Switch_Group {
 sgs.neededCurrentPackageTypes = list.neededCurrentPackageTypes ++ item.neededCurrentPackageTypes;
}

aspect production algebraic_switch_group
switch::Algebraic_Switch_Group ::= p::Pattern stmts::Stmt {
 switch.neededCurrentPackageTypes = stmts.neededCurrentPackageTypes;
}

------------

aspect production algebraic_switch
switch::Stmt ::= t::aswitchTerm expr::Expr switchblock::Algebraic_Switch_Block {
-- switch.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes ++ switchblock.neededImportedOnDemandTypes;
}

aspect production algebraic_case_class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.neededImportedOnDemandTypes = cb.neededImportedOnDemandTypes;
}

aspect production algebraic_switch_block
switchblock::Algebraic_Switch_Block ::= gs::Algebraic_Switch_Groups {
 switchblock.neededImportedOnDemandTypes = gs.neededImportedOnDemandTypes;
}

aspect production algebraic_switch_groups_one
sgs::Algebraic_Switch_Groups ::= sg::Algebraic_Switch_Group {
 sgs.neededImportedOnDemandTypes = sg.neededImportedOnDemandTypes;
}

aspect production algebraic_switch_groups_snoc
sgs::Algebraic_Switch_Groups ::= list::Algebraic_Switch_Groups item::Algebraic_Switch_Group {
 sgs.neededImportedOnDemandTypes = list.neededImportedOnDemandTypes ++ item.neededImportedOnDemandTypes;
}

aspect production algebraic_switch_group
switch::Algebraic_Switch_Group ::= p::Pattern stmts::Stmt {
 switch.neededImportedOnDemandTypes = stmts.neededImportedOnDemandTypes;
}

------------

aspect production algebraic_switch
switch::Stmt ::= t::aswitchTerm expr::Expr switchblock::Algebraic_Switch_Block {
 switch.localTypes = [];
}

aspect production algebraic_case_class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.localTypes = [ cname.lexeme ];
}

aspect production algebraic_switch_block
switchblock::Algebraic_Switch_Block ::= gs::Algebraic_Switch_Groups {
 switchblock.localTypes = [];
}

aspect production algebraic_switch_groups_one
sgs::Algebraic_Switch_Groups ::= sg::Algebraic_Switch_Group {
 sgs.localTypes = [];
}

aspect production algebraic_switch_groups_snoc
sgs::Algebraic_Switch_Groups ::= list::Algebraic_Switch_Groups item::Algebraic_Switch_Group {
 sgs.localTypes = [];
}

aspect production algebraic_switch_group
switch::Algebraic_Switch_Group ::= p::Pattern stmts::Stmt {
 switch.localTypes = [];
}

------------

aspect production algebraic_switch
switch::Stmt ::= t::aswitchTerm expr::Expr switchblock::Algebraic_Switch_Block {
-- switch.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes ++ switchblock.neededFullyQualifiedTypes;
}

aspect production algebraic_case_class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.neededFullyQualifiedTypes = cb.neededFullyQualifiedTypes;
}

aspect production algebraic_switch_block
switchblock::Algebraic_Switch_Block ::= gs::Algebraic_Switch_Groups {
 switchblock.neededFullyQualifiedTypes = gs.neededFullyQualifiedTypes;
}

aspect production algebraic_switch_groups_one
sgs::Algebraic_Switch_Groups ::= sg::Algebraic_Switch_Group {
 sgs.neededFullyQualifiedTypes = sg.neededFullyQualifiedTypes;
}

aspect production algebraic_switch_groups_snoc
sgs::Algebraic_Switch_Groups ::= list::Algebraic_Switch_Groups item::Algebraic_Switch_Group {
 sgs.neededFullyQualifiedTypes = list.neededFullyQualifiedTypes ++ item.neededFullyQualifiedTypes;
}

aspect production algebraic_switch_group
switch::Algebraic_Switch_Group ::= p::Pattern stmts::Stmt {
 switch.neededFullyQualifiedTypes = stmts.neededFullyQualifiedTypes;
}

