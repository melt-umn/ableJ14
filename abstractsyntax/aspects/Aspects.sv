grammar edu:umn:cs:melt:ableJ14:abstractsyntax:aspects;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:abstractsyntax:packages;
import edu:umn:cs:melt:ableJ14:terminals;

-- neededImportedSingleTypes
-- neededCurrentPackageTypes
-- neededImportedOnDemandTypes
-- localTypes
-- neededFullyQualifiedTypes

--synthesized attribute ... :: ... ;
--attribute ... occurs on 			  	PackageName, PackageOrTypeName, TypeName, ExprName, MethodName, AmbiguousName, 
--							Root, Package_Dcl, Import_Dcls, Import_Dcl, 
--							Type_Dcls, Type_Dcl, LHS, Expr, Stmt_Expr, Exprs, Class_Body, Block, Stmt,
--							For_Init, For_Update, Stmt_Exprs, Local_Var_Dcl, For_Test, Switch_Block, Switch_Groups,
--							Switch_Labels, Switch_Group, Switch_Label, Class_Dcl, Class_Member_Dcls, Class_Member_Dcl,
--							Interface_Dcl, Type, Var_Declarators, Field_Dcl, Var_Declarator, Var_Declarator_Id,
--							Var_Init, Array_Init, Var_Inits, Method_Dcl, Method_Header, Formal_Params, Formal_Param,
--							Method_Declarator, Throws, Constructor_Invocation, Reference_Type, Array_Type, Catch, Catches,
--							Interface_Member_Dcls, Interface_Member_Dcl, TypeNames, Dim_Exprs;

aspect production compilation_unit
r::Root ::= pd::Package_Dcl ids::Import_Dcls tds::Type_Dcls {
 r.neededImportedSingleTypes = tds.neededImportedSingleTypes;
}

aspect production package_dcl
p::Package_Dcl ::= n::PackageName {
}

aspect production package_dcl_none
p::Package_Dcl ::= {
}

aspect production import_dcls_none
idcls::Import_Dcls ::= {
}

aspect production import_dcls_snoc
idcls::Import_Dcls ::= idcls1::Import_Dcls idcl::Import_Dcl {
}

aspect production import_dcl
idcl::Import_Dcl ::= t::Import_t n::TypeName {
}

aspect production import_dcl_on_demand
idcl::Import_Dcl ::= n::PackageOrTypeName {
}

aspect production type_dcls_none
tdcls::Type_Dcls ::= {
 tdcls.neededImportedSingleTypes =  [];
}

aspect production type_dcls_snoc
tdcls::Type_Dcls ::= tdcls1::Type_Dcls tdcl::Type_Dcl {
 tdcls.neededImportedSingleTypes =  tdcls1.neededImportedSingleTypes ++ tdcl.neededImportedSingleTypes;
}

aspect production type_dcl_empty
tdcl::Type_Dcl ::= {
 tdcl.neededImportedSingleTypes = [];
}

aspect production lhs_name
lhs::LHS ::= nm::ExprName {
 lhs.neededImportedSingleTypes = nm.neededImportedSingleTypes;
}

aspect production expr_lhs
expr::Expr ::= lhs::LHS {
 expr.neededImportedSingleTypes = lhs.neededImportedSingleTypes;
}

aspect production expr_stmt_expr
e::Expr ::= se::Stmt_Expr {
 e.neededImportedSingleTypes = se.neededImportedSingleTypes;
}

aspect production this
e::Expr ::= {
 e.neededImportedSingleTypes = [];
}

aspect production expr_field_access
lhs::LHS ::= e1::Expr id::Id_t {
 lhs.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production method_call
e::Stmt_Expr ::= name1::MethodName args::Exprs {
 e.neededImportedSingleTypes = name1.neededImportedSingleTypes ++ args.neededImportedSingleTypes;
}

aspect production expr_method_call
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs {
 e.neededImportedSingleTypes = obj.neededImportedSingleTypes ++ params.neededImportedSingleTypes;
}

aspect production resolved_method_call_copy
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs rettr::TypeRep {
 e.neededImportedSingleTypes = obj.neededImportedSingleTypes ++ params.neededImportedSingleTypes;
}

aspect production new_class
e::Stmt_Expr ::= t::TypeName params::Exprs {
 e.neededImportedSingleTypes = t.neededImportedSingleTypes ++ params.neededImportedSingleTypes;
}

aspect production new_class_body
e::Stmt_Expr ::= t::TypeName params::Exprs cb::Class_Body {
 e.neededImportedSingleTypes = t.neededImportedSingleTypes ++ params.neededImportedSingleTypes ++ cb.neededImportedSingleTypes;
}

aspect production resolved_new_class
e::Stmt_Expr ::= tr::TypeRep params::Exprs {
 e.neededImportedSingleTypes = params.neededImportedSingleTypes;
}

aspect production resolved_new_class_body
e::Stmt_Expr ::= tr::TypeRep params::Exprs cb::Class_Body {
 e.neededImportedSingleTypes = params.neededImportedSingleTypes ++ cb.neededImportedSingleTypes;
}

aspect production cast_prod
e::Expr ::= name1::TypeName e1::Expr {
 e.neededImportedSingleTypes = name1.neededImportedSingleTypes ++ e1.neededImportedSingleTypes;
}

aspect production resolved_cast
e::Expr ::= t::TypeRep e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production assign
a::Stmt_Expr ::= lhs::LHS t::Eq_t expr::Expr {
 a.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production plus
e::Expr ::= e1::Expr t::Plus_t e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production minus
e::Expr ::= e1::Expr t::Minus_t e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production mul
e::Expr ::= e1::Expr t::Mul_t e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production byte_const
e::Expr ::= t::String {
 e.neededImportedSingleTypes = [];
}

aspect production short_const
e::Expr ::= t::String {
 e.neededImportedSingleTypes = [];
}

aspect production char_const
e::Expr ::= t::String {
 e.neededImportedSingleTypes = [];
}

aspect production int_const
e::Expr ::= t::String {
 e.neededImportedSingleTypes = [];
}

aspect production long_const
e::Expr ::= t::String {
 e.neededImportedSingleTypes = [];
}

aspect production float_const
e::Expr ::= t::String {
 e.neededImportedSingleTypes = [];
}

aspect production double_const
e::Expr ::= t::String {
 e.neededImportedSingleTypes = [];
}

aspect production true_const
e::Expr ::= {
 e.neededImportedSingleTypes = [];
}

aspect production false_const
e::Expr ::= {
 e.neededImportedSingleTypes = [];
}

aspect production string_const
e::Expr ::= t::String {
 e.neededImportedSingleTypes = [];
}

aspect production null_const
e::Expr ::= {
 e.neededImportedSingleTypes = [];
}

aspect production exprs_none
es::Exprs ::= {
 es.neededImportedSingleTypes = [];
}

aspect production exprs_one
es::Exprs ::= e::Expr {
 es.neededImportedSingleTypes = e.neededImportedSingleTypes;
}

aspect production exprs_snoc
es::Exprs ::= es1::Exprs e::Expr {
 es.neededImportedSingleTypes = es1.neededImportedSingleTypes ++ e.neededImportedSingleTypes;
}

aspect production exprs_cons
es::Exprs ::= e::Expr es1::Exprs {
 es.neededImportedSingleTypes = e.neededImportedSingleTypes ++ es1.neededImportedSingleTypes;
}

aspect production cast_primitive
e::Expr ::= t::Primitive_Type e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production cast_simple
e::Expr ::= t::Expr e1::Expr {
 e.neededImportedSingleTypes = t.neededImportedSingleTypes ++ e1.neededImportedSingleTypes;
}

aspect production block
b::Block ::= stmt::Stmt {
 b.neededImportedSingleTypes = stmt.neededImportedSingleTypes;
}

aspect production empty_block
b::Block ::= {
 b.neededImportedSingleTypes = [];
}

aspect production stmt_seq
seq::Stmt ::= stmt1::Stmt stmt2::Stmt {
 seq.neededImportedSingleTypes = stmt1.neededImportedSingleTypes ++ stmt2.neededImportedSingleTypes;
}

aspect production stmt_block
stmt::Stmt ::= b::Block {
 stmt.neededImportedSingleTypes = b.neededImportedSingleTypes;
}

aspect production empty_stmt
s::Stmt ::= {
 s.neededImportedSingleTypes = [];
}

aspect production erroneous_Stmt
s::Stmt ::= err_tree::Stmt errs::[Error] {
 s.neededImportedSingleTypes = [];
}


aspect production stmt_stmt_expr
s::Stmt ::= s1::Stmt_Expr {
 s.neededImportedSingleTypes = s1.neededImportedSingleTypes;
}

aspect production block_stmt_class
s::Stmt ::= cdcl::Class_Dcl {
 s.neededImportedSingleTypes = cdcl.neededImportedSingleTypes;
}

aspect production block_stmt_interface
s::Stmt ::= idcl::Interface_Dcl {
 s.neededImportedSingleTypes = idcl.neededImportedSingleTypes;
}

aspect production synchronized
s::Stmt ::= e::Expr b::Block {
 s.neededImportedSingleTypes = e.neededImportedSingleTypes ++ b.neededImportedSingleTypes;
}

aspect production throw
s::Stmt ::= e::Expr {
 s.neededImportedSingleTypes = e.neededImportedSingleTypes;
}

aspect production try
s::Stmt ::= b::Block c::Catches {
 s.neededImportedSingleTypes = b.neededImportedSingleTypes ++ c.neededImportedSingleTypes;
}

aspect production try_finally
s::Stmt ::= b::Block c::Catches b1::Block {
 s.neededImportedSingleTypes = b.neededImportedSingleTypes ++ c.neededImportedSingleTypes ++ b1.neededImportedSingleTypes;
}

aspect production assert
s::Stmt ::= expr::Expr {
 s.neededImportedSingleTypes = expr.neededImportedSingleTypes;
}

aspect production assert_colon
s::Stmt ::= expr1::Expr expr2::Expr {
 s.neededImportedSingleTypes = expr1.neededImportedSingleTypes ++ expr2.neededImportedSingleTypes;
}

aspect production catches_none
c::Catches ::= {
 c.neededImportedSingleTypes = [];
}

aspect production catches_snoc
c::Catches ::= cs::Catches cc::Catch {
 c.neededImportedSingleTypes = cs.neededImportedSingleTypes ++ cc.neededImportedSingleTypes;
}

aspect production catch
c::Catch ::= fp::Formal_Param b::Block {
 c.neededImportedSingleTypes = fp.neededImportedSingleTypes ++ b.neededImportedSingleTypes;
}

aspect production while_prod
while::Stmt ::= t::While_t cond::Expr body::Stmt {
 while.neededImportedSingleTypes = cond.neededImportedSingleTypes ++ body.neededImportedSingleTypes;
}

aspect production do
dowhile::Stmt ::= body::Stmt t::While_t cond::Expr {
 dowhile.neededImportedSingleTypes = body.neededImportedSingleTypes ++ cond.neededImportedSingleTypes;
}

aspect production label_prod
lstmt::Stmt ::= id::Id_t stmt::Stmt {
 lstmt.neededImportedSingleTypes = stmt.neededImportedSingleTypes;
}

aspect production break_prod
break::Stmt ::= {
 break.neededImportedSingleTypes = [];
}

aspect production break_label
break::Stmt ::= id::Id_t {
 break.neededImportedSingleTypes = [];
}

aspect production continue_prod
continue::Stmt ::= {
 continue.neededImportedSingleTypes = [];
}

aspect production continue_label
continue::Stmt ::= id::Id_t {
 continue.neededImportedSingleTypes = [];
}

aspect production if_then
ifthen::Stmt ::= t::If_t expr::Expr thenbody::Stmt {
 ifthen.neededImportedSingleTypes = expr.neededImportedSingleTypes ++ thenbody.neededImportedSingleTypes;
}

aspect production if_then_else
ifthenelse::Stmt ::= t::If_t expr::Expr thenbody::Stmt elsebody::Stmt {
 ifthenelse.neededImportedSingleTypes = expr.neededImportedSingleTypes ++ thenbody.neededImportedSingleTypes ++ elsebody.neededImportedSingleTypes;
}

aspect production for
forstmt::Stmt ::= init::For_Init test::For_Test update::For_Update body::Stmt {
 forstmt.neededImportedSingleTypes = init.neededImportedSingleTypes ++ test.neededImportedSingleTypes ++ update.neededImportedSingleTypes ++ body.neededImportedSingleTypes;
}

aspect production for_init_empty
forinit::For_Init ::= {
 forinit.neededImportedSingleTypes = [];
}

aspect production for_init_some
forinit::For_Init ::= ses::Stmt_Exprs {
 forinit.neededImportedSingleTypes = ses.neededImportedSingleTypes;
}

aspect production for_init_dcl
forinit::For_Init ::= dcl::Local_Var_Dcl {
 forinit.neededImportedSingleTypes = dcl.neededImportedSingleTypes;
}

aspect production for_test_none
fortest::For_Test ::= {
 fortest.neededImportedSingleTypes = [];
}

aspect production for_test_one
fortest::For_Test ::= e::Expr {
 fortest.neededImportedSingleTypes = e.neededImportedSingleTypes;
}

aspect production for_update_empty
forupdate::For_Update ::= {
 forupdate.neededImportedSingleTypes = [];
}

aspect production for_update_some
forupdate::For_Update ::= ses::Stmt_Exprs {
 forupdate.neededImportedSingleTypes = ses.neededImportedSingleTypes;
}

aspect production stmt_exprs_one
ses::Stmt_Exprs ::= se::Stmt_Expr {
 ses.neededImportedSingleTypes = se.neededImportedSingleTypes;
}

aspect production stmt_exprs_snoc
ses::Stmt_Exprs ::= ses1::Stmt_Exprs se::Stmt_Expr {
 ses.neededImportedSingleTypes = ses1.neededImportedSingleTypes ++ se.neededImportedSingleTypes;
}

aspect production switch_prod
switch::Stmt ::= expr::Expr switchblock::Switch_Block {
 switch.neededImportedSingleTypes = expr.neededImportedSingleTypes ++ switchblock.neededImportedSingleTypes;
}

aspect production switch_block
switchblock::Switch_Block ::= gs::Switch_Groups ls::Switch_Labels {
 switchblock.neededImportedSingleTypes = gs.neededImportedSingleTypes ++ ls.neededImportedSingleTypes;
}

aspect production switch_block_no_labels
switchblock::Switch_Block ::= s::Switch_Groups {
 switchblock.neededImportedSingleTypes = s.neededImportedSingleTypes;
}

aspect production switch_block_no_groups
switchblock::Switch_Block ::= s::Switch_Labels {
 switchblock.neededImportedSingleTypes = s.neededImportedSingleTypes;
}

aspect production switch_block_empty
switchblock::Switch_Block ::= {
 switchblock.neededImportedSingleTypes = [];
}

aspect production switch_groups_one
switch::Switch_Groups ::= item::Switch_Group {
 switch.neededImportedSingleTypes = item.neededImportedSingleTypes;
}

aspect production switch_groups_snoc
switch::Switch_Groups ::= list::Switch_Groups item::Switch_Group {
 switch.neededImportedSingleTypes = list.neededImportedSingleTypes ++ item.neededImportedSingleTypes;
}

aspect production switch_group
switch::Switch_Group ::= label_list::Switch_Labels stmts::Stmt {
 switch.neededImportedSingleTypes = label_list.neededImportedSingleTypes ++ stmts.neededImportedSingleTypes;
}

aspect production switch_labels_one
switchlabels::Switch_Labels ::= switchlabel::Switch_Label {
 switchlabels.neededImportedSingleTypes = switchlabel.neededImportedSingleTypes;
}

aspect production switch_labels_snoc
switchlabels::Switch_Labels ::=  list::Switch_Labels item::Switch_Label {
 switchlabels.neededImportedSingleTypes = list.neededImportedSingleTypes ++ item.neededImportedSingleTypes;
}

aspect production switch_label
label::Switch_Label ::= expr::Expr {
 label.neededImportedSingleTypes = expr.neededImportedSingleTypes;
}

aspect production switch_label_default
label::Switch_Label ::= {
 label.neededImportedSingleTypes = [];
}

aspect production type_class_dcl
td::Type_Dcl ::= cdcl::Class_Dcl {
 td.neededImportedSingleTypes = cdcl.neededImportedSingleTypes;
}

aspect production class_dcl_seq
cdcl::Class_Dcl ::= cdcl1::Class_Dcl cdcl2::Class_Dcl {
 cdcl.neededImportedSingleTypes = cdcl1.neededImportedSingleTypes ++ cdcl2.neededImportedSingleTypes;
}

aspect production class_dcl_none
cdcl::Class_Dcl ::= {
 cdcl.neededImportedSingleTypes = [];
}

aspect production class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.neededImportedSingleTypes = parent.neededImportedSingleTypes ++ inters.neededImportedSingleTypes ++ cb.neededImportedSingleTypes ;
}

aspect production class_body
cb::Class_Body ::= dcls::Class_Member_Dcls {
 cb.neededImportedSingleTypes = dcls.neededImportedSingleTypes;
}

aspect production class_member_dcls_none
cdcls::Class_Member_Dcls ::= {
 cdcls.neededImportedSingleTypes = [];
}

aspect production class_member_dcls_snoc
cdcls::Class_Member_Dcls ::= cdcls1::Class_Member_Dcls cdcl::Class_Member_Dcl {
 cdcls.neededImportedSingleTypes = cdcls1.neededImportedSingleTypes ++ cdcl.neededImportedSingleTypes;
}

aspect production class_member_dcls_one
cdcls::Class_Member_Dcls ::= cdcl::Class_Member_Dcl {
 cdcls.neededImportedSingleTypes = cdcl.neededImportedSingleTypes;
}

aspect production class_member_dcl_seq
cdcl::Class_Member_Dcl ::= cdcl1::Class_Member_Dcl cdcl2::Class_Member_Dcl {
 cdcl.neededImportedSingleTypes = cdcl1.neededImportedSingleTypes ++ cdcl2.neededImportedSingleTypes;
}

aspect production class_member_empty
cdcl::Class_Member_Dcl ::= {
 cdcl.neededImportedSingleTypes = [];
}

aspect production inner_class
cdcl::Class_Member_Dcl ::= cd::Class_Dcl {
 cdcl.neededImportedSingleTypes = cd.neededImportedSingleTypes;
}

aspect production class_constructor
cdcl::Class_Member_Dcl ::= mods::Modifiers id::Id_t fps::Formal_Params thr::Throws cb::Block {
 cdcl.neededImportedSingleTypes = fps.neededImportedSingleTypes ++ thr.neededImportedSingleTypes ++ cb.neededImportedSingleTypes;
}

aspect production inner_interface
cdcl::Class_Member_Dcl ::= id::Interface_Dcl {
 cdcl.neededImportedSingleTypes = id.neededImportedSingleTypes;
}

aspect production class_block
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.neededImportedSingleTypes = b.neededImportedSingleTypes;
}

aspect production class_static_initializer
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.neededImportedSingleTypes = b.neededImportedSingleTypes;
}

aspect production class_field
cdcl::Class_Member_Dcl ::= f::Field_Dcl {
 cdcl.neededImportedSingleTypes = f.neededImportedSingleTypes;
}

aspect production class_method
cdcl::Class_Member_Dcl ::= mdcl::Method_Dcl {
 cdcl.neededImportedSingleTypes = mdcl.neededImportedSingleTypes;
}

aspect production stmt_dcl
stmt::Stmt ::= vdcl::Local_Var_Dcl {
 stmt.neededImportedSingleTypes = vdcl.neededImportedSingleTypes;
}

aspect production local_var_dcl
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
 vdcl.neededImportedSingleTypes = dcltype.neededImportedSingleTypes ++ dcls.neededImportedSingleTypes;
}

aspect production field_dcl
f::Field_Dcl ::= mods::Modifiers t::Type dcls::Var_Declarators {
 f.neededImportedSingleTypes = t.neededImportedSingleTypes ++ dcls.neededImportedSingleTypes;
}

aspect production var_declarator
vdcl::Var_Declarator ::= v::Var_Declarator_Id {
 vdcl.neededImportedSingleTypes = v.neededImportedSingleTypes;
}

aspect production var_declarator_init
vdcl::Var_Declarator ::= v::Var_Declarator_Id init::Var_Init {
 vdcl.neededImportedSingleTypes = v.neededImportedSingleTypes ++ init.neededImportedSingleTypes;
}

aspect production var_declarator_id
vdcl::Var_Declarator_Id ::= id::Id_t {
 vdcl.neededImportedSingleTypes = [];
}

aspect production var_init_expr
init::Var_Init ::= e::Expr {
 init.neededImportedSingleTypes = e.neededImportedSingleTypes;
}

aspect production local_var_dcl_seq
vdcl::Local_Var_Dcl ::= vdcl1::Local_Var_Dcl vdcl2::Local_Var_Dcl {
 vdcl.neededImportedSingleTypes = vdcl1.neededImportedSingleTypes ++ vdcl2.neededImportedSingleTypes;
}

aspect production local_var_dcl_one
vdcl::Local_Var_Dcl ::= dcltype::Type dcl::Var_Declarator {
 vdcl.neededImportedSingleTypes = dcltype.neededImportedSingleTypes ++ dcl.neededImportedSingleTypes;
}

aspect production field_dcl_seq
fdcl::Field_Dcl ::= fdcl1::Field_Dcl fdcl2::Field_Dcl {
 fdcl.neededImportedSingleTypes = fdcl1.neededImportedSingleTypes ++ fdcl2.neededImportedSingleTypes;
}

aspect production field_dcl_one
f::Field_Dcl ::= mods::Modifiers t::Type dcl::Var_Declarator {
 f.neededImportedSingleTypes = t.neededImportedSingleTypes ++ dcl.neededImportedSingleTypes;
}

aspect production var_declarators_one
vdcls::Var_Declarators ::= vdcl::Var_Declarator {
 vdcls.neededImportedSingleTypes = vdcl.neededImportedSingleTypes;
}

aspect production var_declarators_snoc
vdcls::Var_Declarators ::= vdcls1::Var_Declarators vdcl::Var_Declarator {
 vdcls.neededImportedSingleTypes = vdcls1.neededImportedSingleTypes ++ vdcl.neededImportedSingleTypes;
}

aspect production local_var_dcl_final
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
 vdcl.neededImportedSingleTypes = dcltype.neededImportedSingleTypes ++ dcls.neededImportedSingleTypes;
}

aspect production var_declarator_array
vdcl::Var_Declarator_Id ::= v::Var_Declarator_Id {
 vdcl.neededImportedSingleTypes = v.neededImportedSingleTypes;
}

aspect production var_init_array
init::Var_Init ::= e::Array_Init {
 init.neededImportedSingleTypes = e.neededImportedSingleTypes;
}

aspect production array_init
ai::Array_Init ::= vs::Var_Inits {
 ai.neededImportedSingleTypes = vs.neededImportedSingleTypes;
}

aspect production array_init_no_comma
ai::Array_Init ::= vs::Var_Inits {
 ai.neededImportedSingleTypes = vs.neededImportedSingleTypes;
}

aspect production array_init_no_var_inits
ai::Array_Init ::= {
 ai.neededImportedSingleTypes = [];
}

aspect production array_init_empty
ai::Array_Init ::= {
 ai.neededImportedSingleTypes = [];
}

aspect production var_inits_one
vis::Var_Inits ::= vi::Var_Init {
 vis.neededImportedSingleTypes = vi.neededImportedSingleTypes;
}

aspect production var_inits_snoc
vis::Var_Inits ::= vis1::Var_Inits vi::Var_Init {
 vis.neededImportedSingleTypes = vis1.neededImportedSingleTypes ++ vi.neededImportedSingleTypes;
}

aspect production method_dcl_prod
mdcl::Method_Dcl ::=  mh::Method_Header b::Block {
 mdcl.neededImportedSingleTypes = mh.neededImportedSingleTypes ++ b.neededImportedSingleTypes;
}

aspect production method_dcl_no_body
mdcl::Method_Dcl ::= mh::Method_Header {
 mdcl.neededImportedSingleTypes = mh.neededImportedSingleTypes;
}

aspect production method_header_prod
mh::Method_Header ::= mods::Modifiers return_ty::Type mname::Id_t fps::Formal_Params th::Throws {
 mh.neededImportedSingleTypes = return_ty.neededImportedSingleTypes ++ fps.neededImportedSingleTypes ++ th.neededImportedSingleTypes;
}

aspect production void_type
t::Type ::= {
 t.neededImportedSingleTypes = [];
}

aspect production formal_params_none
fps::Formal_Params ::= {
 fps.neededImportedSingleTypes = [];
}

aspect production formal_params_one
fps::Formal_Params ::= fp::Formal_Param {
 fps.neededImportedSingleTypes = fp.neededImportedSingleTypes;
}

aspect production formal_params_snoc
fps::Formal_Params ::= fps1::Formal_Params fp::Formal_Param {
 fps.neededImportedSingleTypes = fps1.neededImportedSingleTypes ++ fp.neededImportedSingleTypes;
}

aspect production formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
 fp.neededImportedSingleTypes = t.neededImportedSingleTypes ++ vid.neededImportedSingleTypes;
}

aspect production return_statement
s::Stmt ::= t::Return_t {
 s.neededImportedSingleTypes = [];
}

aspect production return_expr
s::Stmt ::= t::Return_t e::Expr {
 s.neededImportedSingleTypes = e.neededImportedSingleTypes;
}

aspect production method_header_declarator
mh::Method_Header ::= mods::Modifiers t::Type md::Method_Declarator th::Throws {
 mh.neededImportedSingleTypes = t.neededImportedSingleTypes ++ md.neededImportedSingleTypes ++ th.neededImportedSingleTypes;
}

aspect production method_declarator
md::Method_Declarator ::= id::Id_t fps::Formal_Params {
 md.neededImportedSingleTypes = fps.neededImportedSingleTypes;
}

aspect production method_declarator_array
md::Method_Declarator ::= md1::Method_Declarator {
 md.neededImportedSingleTypes = md1.neededImportedSingleTypes;
}

aspect production final_formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
 fp.neededImportedSingleTypes = t.neededImportedSingleTypes ++ vid.neededImportedSingleTypes;
}

aspect production throws_none
th::Throws ::= {
 th.neededImportedSingleTypes = [];
}

aspect production throws
th::Throws ::= ctl::TypeNames {
 th.neededImportedSingleTypes = ctl.neededImportedSingleTypes;
}

aspect production stmt_constructor_invocation
stmt::Stmt ::= inv::Constructor_Invocation {
 stmt.neededImportedSingleTypes = inv.neededImportedSingleTypes;
}

aspect production this_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
 inv.neededImportedSingleTypes = args.neededImportedSingleTypes;
}

aspect production super_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
 inv.neededImportedSingleTypes = args.neededImportedSingleTypes;
}

aspect production this_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
 inv.neededImportedSingleTypes = expr.neededImportedSingleTypes ++ args.neededImportedSingleTypes;
}

aspect production this_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
 inv.neededImportedSingleTypes = t.neededImportedSingleTypes ++ args.neededImportedSingleTypes;
}

aspect production super_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
 inv.neededImportedSingleTypes = expr.neededImportedSingleTypes ++ args.neededImportedSingleTypes;
}

aspect production super_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
 inv.neededImportedSingleTypes = t.neededImportedSingleTypes ++ args.neededImportedSingleTypes;
}

aspect production primitive_type
t::Type ::= t1::Primitive_Type {
 t.neededImportedSingleTypes = [];
}

aspect production reference_type
t::Type ::= t1::Reference_Type {
 t.neededImportedSingleTypes = t1.neededImportedSingleTypes;
}

aspect production type_typerep
t::Type ::= tr::TypeRep {
 t.neededImportedSingleTypes = [];
}

aspect production name_type
t::Reference_Type ::= n::TypeName {
 t.neededImportedSingleTypes = n.neededImportedSingleTypes;
}

aspect production type_names_none
ns::TypeNames ::= {
 ns.neededImportedSingleTypes = [];
}

aspect production type_names_one
ns::TypeNames ::= n::TypeName {
 ns.neededImportedSingleTypes = n.neededImportedSingleTypes;
}

aspect production type_names_snoc
ns::TypeNames ::= ns1::TypeNames n::TypeName {
 ns.neededImportedSingleTypes = ns1.neededImportedSingleTypes ++ n.neededImportedSingleTypes;
}

aspect production type_interface_dcl
td::Type_Dcl ::= idcl::Interface_Dcl {
 td.neededImportedSingleTypes = idcl.neededImportedSingleTypes;
}

aspect production interface_member_dcls_none
idcls::Interface_Member_Dcls ::= {
 idcls.neededImportedSingleTypes = [];
}

aspect production interface_member_dcls_snoc
idcls::Interface_Member_Dcls ::= idcls1::Interface_Member_Dcls idcl::Interface_Member_Dcl {
 idcls.neededImportedSingleTypes = idcls1.neededImportedSingleTypes ++ idcl.neededImportedSingleTypes;
}

aspect production interface_field
idcl::Interface_Member_Dcl ::= fd::Field_Dcl {
 idcl.neededImportedSingleTypes = fd.neededImportedSingleTypes;
}

aspect production interface_method
idcl::Interface_Member_Dcl ::= mh::Method_Header {
 idcl.neededImportedSingleTypes = mh.neededImportedSingleTypes;
}

aspect production interface_empty
idcl::Interface_Member_Dcl ::= {
 idcl.neededImportedSingleTypes = [];
}

aspect production interface_dcl
idcl::Interface_Dcl ::= mods::Modifiers iname::Id_t inters::TypeNames dcls::Interface_Member_Dcls {
 idcl.neededImportedSingleTypes = inters.neededImportedSingleTypes ++ dcls.neededImportedSingleTypes;
}

aspect production interface_inner_class
idcl::Interface_Member_Dcl ::= cd::Class_Dcl {
 idcl.neededImportedSingleTypes = cd.neededImportedSingleTypes;
}

aspect production interface_inner_interface
idcl::Interface_Member_Dcl ::= id::Interface_Dcl {
 idcl.neededImportedSingleTypes = id.neededImportedSingleTypes;
}

aspect production cast_name_array
e::Expr ::= t::TypeName dims::Integer e1::Expr {
 e.neededImportedSingleTypes = t.neededImportedSingleTypes ++ e1.neededImportedSingleTypes;
}

aspect production cast_primitive_array
e::Expr ::= t::Primitive_Type dims::Integer e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production not
e::Expr ::= e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production or_or
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production and_and
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production super_field_access
lhs::LHS ::= id::Id_t {
 lhs.neededImportedSingleTypes = [];
}

aspect production name_super_field_access
lhs::LHS ::= n::TypeName id::Id_t {
 lhs.neededImportedSingleTypes = n.neededImportedSingleTypes;
}

aspect production super_method_call
e::Stmt_Expr ::= id::Id_t es::Exprs {
 e.neededImportedSingleTypes = es.neededImportedSingleTypes;
}

aspect production name_super_method_call
e::Stmt_Expr ::= n::TypeName id::Id_t es::Exprs {
 e.neededImportedSingleTypes = n.neededImportedSingleTypes ++ es.neededImportedSingleTypes;
}

aspect production array_access
lhs::LHS ::= n::ExprName e1::Expr {
 lhs.neededImportedSingleTypes = n.neededImportedSingleTypes ++ e1.neededImportedSingleTypes;
}

aspect production array_access_general
lhs::LHS ::= e1::Expr e2::Expr {
 lhs.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production mul_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production div_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production mod_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production plus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production minus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production lshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production rshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production urshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production and_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production xor_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production or_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ expr.neededImportedSingleTypes;
}

aspect production new_class_expr
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs {
 e.neededImportedSingleTypes = expr.neededImportedSingleTypes ++ es.neededImportedSingleTypes;
}

aspect production new_class_expr_body
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs cb::Class_Body {
 e.neededImportedSingleTypes = expr.neededImportedSingleTypes ++ es.neededImportedSingleTypes ++ cb.neededImportedSingleTypes;
}

aspect production new_class_name
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs {
 e.neededImportedSingleTypes = nam.neededImportedSingleTypes ++ es.neededImportedSingleTypes;
}

aspect production new_class_name_body
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs cb::Class_Body {
 e.neededImportedSingleTypes = nam.neededImportedSingleTypes ++ es.neededImportedSingleTypes ++ cb.neededImportedSingleTypes;
}

aspect production conditional
e::Expr ::= cond::Expr thenexpr::Expr elseexpr::Expr {
 e.neededImportedSingleTypes = cond.neededImportedSingleTypes ++ thenexpr.neededImportedSingleTypes ++ elseexpr.neededImportedSingleTypes;
}

aspect production or
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production xor
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production and
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production eq
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production not_eq
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production lt
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production gt
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production lteq
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production gteq
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production instanceof
e::Expr ::= e1::Expr t1::Reference_Type {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ t1.neededImportedSingleTypes;
}

aspect production lshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production rshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production urshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production div
e::Expr ::= e1::Expr _ e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production mod
e::Expr ::= e1::Expr _ e2::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes ++ e2.neededImportedSingleTypes;
}

aspect production unary_plus
e::Expr ::= e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production unary_minus
e::Expr ::= e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production pre_inc
e::Stmt_Expr ::= e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production pre_dec
e::Stmt_Expr ::= e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production comp
e::Expr ::= e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production post_inc
e::Stmt_Expr ::= e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production post_dec
e::Stmt_Expr ::= e1::Expr {
 e.neededImportedSingleTypes = e1.neededImportedSingleTypes;
}

aspect production new_array_init_primitive
e::Expr ::= t1::Primitive_Type dims::Integer ai::Array_Init {
 e.neededImportedSingleTypes = ai.neededImportedSingleTypes;
}

aspect production new_array_init_name
e::Expr ::= n::TypeName dims::Integer ai::Array_Init {
 e.neededImportedSingleTypes = n.neededImportedSingleTypes ++ ai.neededImportedSingleTypes;
}

aspect production new_array_no_init_primitive
e::Expr ::= t1::Primitive_Type d1::Dim_Exprs dims::Integer {
 e.neededImportedSingleTypes = d1.neededImportedSingleTypes;
}

aspect production new_array_no_init_name
e::Expr ::= n::TypeName d1::Dim_Exprs dims::Integer {
 e.neededImportedSingleTypes = n.neededImportedSingleTypes ++ d1.neededImportedSingleTypes;
}

aspect production primitive_dot_class
e::Expr ::= t::Primitive_Type {
 e.neededImportedSingleTypes = [];
}

aspect production void_dot_class
e::Expr ::= {
 e.neededImportedSingleTypes = [];
}

aspect production array_dot_class
e::Expr ::= t::Array_Type {
 e.neededImportedSingleTypes = t.neededImportedSingleTypes;
}

aspect production name_dot_class
e::Expr ::= n::TypeName {
 e.neededImportedSingleTypes = n.neededImportedSingleTypes;
}

aspect production name_dot_this
e::Expr ::= n::TypeName {
 e.neededImportedSingleTypes = n.neededImportedSingleTypes;
}

aspect production dim_exprs_one
d::Dim_Exprs ::= e::Expr {
 d.neededImportedSingleTypes = e.neededImportedSingleTypes;
}

aspect production dim_exprs_snoc
d::Dim_Exprs ::= d1::Dim_Exprs e::Expr {
 d.neededImportedSingleTypes = d1.neededImportedSingleTypes ++ e.neededImportedSingleTypes;
}

aspect production array_type
t::Reference_Type ::= t1::Array_Type {
 t.neededImportedSingleTypes = t1.neededImportedSingleTypes;
}

aspect production primitive_array
t::Array_Type ::= t1::Primitive_Type ds::Integer {
 t.neededImportedSingleTypes = [];
}

aspect production name_array
t::Array_Type ::= n::TypeName ds::Integer {
 t.neededImportedSingleTypes = n.neededImportedSingleTypes;
}

aspect production simple_package_name
pn::PackageName ::= id::Id_t {
 pn.neededImportedSingleTypes = [];
}

aspect production qualified_package_name
pn::PackageName ::= pn2::PackageName id::Id_t {
 pn.neededImportedSingleTypes = pn2.neededImportedSingleTypes;
}

aspect production simple_package_or_type_name 
ptn::PackageOrTypeName ::= id::Id_t {
}

aspect production qualified_package_or_type_name
ptn::PackageOrTypeName ::= pn::PackageOrTypeName id::Id_t {
 ptn.neededImportedSingleTypes = pn.neededImportedSingleTypes;
}

aspect production simple_type_name
tn::TypeName ::= id::Id_t {
}

aspect production qualified_type_name
tn::TypeName ::= pn::PackageOrTypeName id::Id_t {
 tn.neededImportedSingleTypes = pn.neededImportedSingleTypes;
}

aspect production simple_expr_name
en::ExprName ::= id::Id_t {
 en.neededImportedSingleTypes = [];
}

aspect production qualified_expr_name
en::ExprName ::= an::AmbiguousName id::Id_t {
 en.neededImportedSingleTypes = an.neededImportedSingleTypes;
}

aspect production simple_method_name
mn::MethodName ::= id::Id_t  { 
 mn.neededImportedSingleTypes = [];
}

aspect production qualified_method_name
mn::MethodName ::=  an::AmbiguousName id::Id_t {
 mn.neededImportedSingleTypes = an.neededImportedSingleTypes;
}

aspect production simple_ambiguous_name
an::AmbiguousName ::= id::Id_t {
}

aspect production qualified_ambiguous_name
andi::AmbiguousName ::=  an::AmbiguousName id::Id_t {
 andi.neededImportedSingleTypes = an.neededImportedSingleTypes;
}

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

aspect production compilation_unit
r::Root ::= pd::Package_Dcl ids::Import_Dcls tds::Type_Dcls {
 r.neededCurrentPackageTypes = tds.neededCurrentPackageTypes;
}

aspect production package_dcl
p::Package_Dcl ::= n::PackageName {
}

aspect production package_dcl_none
p::Package_Dcl ::= {
}

aspect production import_dcls_none
idcls::Import_Dcls ::= {
}

aspect production import_dcls_snoc
idcls::Import_Dcls ::= idcls1::Import_Dcls idcl::Import_Dcl {
}

aspect production import_dcl
idcl::Import_Dcl ::= t::Import_t n::TypeName {
}

aspect production import_dcl_on_demand
idcl::Import_Dcl ::= n::PackageOrTypeName {
}

aspect production type_dcls_none
tdcls::Type_Dcls ::= {
 tdcls.neededCurrentPackageTypes =  [];
}

aspect production type_dcls_snoc
tdcls::Type_Dcls ::= tdcls1::Type_Dcls tdcl::Type_Dcl {
 tdcls.neededCurrentPackageTypes =  tdcls1.neededCurrentPackageTypes ++ tdcl.neededCurrentPackageTypes;
}

aspect production type_dcl_empty
tdcl::Type_Dcl ::= {
 tdcl.neededCurrentPackageTypes = [];
}

aspect production lhs_name
lhs::LHS ::= nm::ExprName {
 lhs.neededCurrentPackageTypes = nm.neededCurrentPackageTypes;
}

aspect production expr_lhs
expr::Expr ::= lhs::LHS {
 expr.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes;
}

aspect production expr_stmt_expr
e::Expr ::= se::Stmt_Expr {
 e.neededCurrentPackageTypes = se.neededCurrentPackageTypes;
}

aspect production this
e::Expr ::= {
 e.neededCurrentPackageTypes = [];
}

aspect production expr_field_access
lhs::LHS ::= e1::Expr id::Id_t {
 lhs.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production method_call
e::Stmt_Expr ::= name1::MethodName args::Exprs {
 e.neededCurrentPackageTypes = name1.neededCurrentPackageTypes ++ args.neededCurrentPackageTypes;
}

aspect production expr_method_call
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs {
 e.neededCurrentPackageTypes = obj.neededCurrentPackageTypes ++ params.neededCurrentPackageTypes;
}

aspect production resolved_method_call_copy
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs rettr::TypeRep {
 e.neededCurrentPackageTypes = obj.neededCurrentPackageTypes ++ params.neededCurrentPackageTypes;
}

aspect production new_class
e::Stmt_Expr ::= t::TypeName params::Exprs {
 e.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ params.neededCurrentPackageTypes;
}

aspect production new_class_body
e::Stmt_Expr ::= t::TypeName params::Exprs cb::Class_Body {
 e.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ params.neededCurrentPackageTypes ++ cb.neededCurrentPackageTypes;
}

aspect production resolved_new_class
e::Stmt_Expr ::= tr::TypeRep params::Exprs {
 e.neededCurrentPackageTypes = params.neededCurrentPackageTypes;
}

aspect production resolved_new_class_body
e::Stmt_Expr ::= tr::TypeRep params::Exprs cb::Class_Body {
 e.neededCurrentPackageTypes = params.neededCurrentPackageTypes ++ cb.neededCurrentPackageTypes;
}

aspect production cast_prod
e::Expr ::= name1::TypeName e1::Expr {
 e.neededCurrentPackageTypes = name1.neededCurrentPackageTypes ++ e1.neededCurrentPackageTypes;
}

aspect production resolved_cast
e::Expr ::= t::TypeRep e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production assign
a::Stmt_Expr ::= lhs::LHS t::Eq_t expr::Expr {
 a.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production plus
e::Expr ::= e1::Expr t::Plus_t e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production minus
e::Expr ::= e1::Expr t::Minus_t e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production mul
e::Expr ::= e1::Expr t::Mul_t e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production byte_const
e::Expr ::= t::String {
 e.neededCurrentPackageTypes = [];
}

aspect production short_const
e::Expr ::= t::String {
 e.neededCurrentPackageTypes = [];
}

aspect production char_const
e::Expr ::= t::String {
 e.neededCurrentPackageTypes = [];
}

aspect production int_const
e::Expr ::= t::String {
 e.neededCurrentPackageTypes = [];
}

aspect production long_const
e::Expr ::= t::String {
 e.neededCurrentPackageTypes = [];
}

aspect production float_const
e::Expr ::= t::String {
 e.neededCurrentPackageTypes = [];
}

aspect production double_const
e::Expr ::= t::String {
 e.neededCurrentPackageTypes = [];
}

aspect production true_const
e::Expr ::= {
 e.neededCurrentPackageTypes = [];
}

aspect production false_const
e::Expr ::= {
 e.neededCurrentPackageTypes = [];
}

aspect production string_const
e::Expr ::= t::String {
 e.neededCurrentPackageTypes = [];
}

aspect production null_const
e::Expr ::= {
 e.neededCurrentPackageTypes = [];
}

aspect production exprs_none
es::Exprs ::= {
 es.neededCurrentPackageTypes = [];
}

aspect production exprs_one
es::Exprs ::= e::Expr {
 es.neededCurrentPackageTypes = e.neededCurrentPackageTypes;
}

aspect production exprs_snoc
es::Exprs ::= es1::Exprs e::Expr {
 es.neededCurrentPackageTypes = es1.neededCurrentPackageTypes ++ e.neededCurrentPackageTypes;
}

aspect production exprs_cons
es::Exprs ::= e::Expr es1::Exprs {
 es.neededCurrentPackageTypes = e.neededCurrentPackageTypes ++ es1.neededCurrentPackageTypes;
}

aspect production cast_primitive
e::Expr ::= t::Primitive_Type e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production cast_simple
e::Expr ::= t::Expr e1::Expr {
 e.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ e1.neededCurrentPackageTypes;
}

aspect production block
b::Block ::= stmt::Stmt {
 b.neededCurrentPackageTypes = stmt.neededCurrentPackageTypes;
}

aspect production empty_block
b::Block ::= {
 b.neededCurrentPackageTypes = [];
}

aspect production stmt_seq
seq::Stmt ::= stmt1::Stmt stmt2::Stmt {
 seq.neededCurrentPackageTypes = stmt1.neededCurrentPackageTypes ++ stmt2.neededCurrentPackageTypes;
}

aspect production stmt_block
stmt::Stmt ::= b::Block {
 stmt.neededCurrentPackageTypes = b.neededCurrentPackageTypes;
}

aspect production empty_stmt
s::Stmt ::= {
 s.neededCurrentPackageTypes = [];
}

aspect production erroneous_Stmt
s::Stmt ::= err_tree::Stmt errs::[Error] {
 s.neededCurrentPackageTypes = [];
}

aspect production stmt_stmt_expr
s::Stmt ::= s1::Stmt_Expr {
 s.neededCurrentPackageTypes = s1.neededCurrentPackageTypes;
}

aspect production block_stmt_class
s::Stmt ::= cdcl::Class_Dcl {
 s.neededCurrentPackageTypes = cdcl.neededCurrentPackageTypes;
}

aspect production block_stmt_interface
s::Stmt ::= idcl::Interface_Dcl {
 s.neededCurrentPackageTypes = idcl.neededCurrentPackageTypes;
}

aspect production synchronized
s::Stmt ::= e::Expr b::Block {
 s.neededCurrentPackageTypes = e.neededCurrentPackageTypes ++ b.neededCurrentPackageTypes;
}

aspect production throw
s::Stmt ::= e::Expr {
 s.neededCurrentPackageTypes = e.neededCurrentPackageTypes;
}

aspect production try
s::Stmt ::= b::Block c::Catches {
 s.neededCurrentPackageTypes = b.neededCurrentPackageTypes ++ c.neededCurrentPackageTypes;
}

aspect production try_finally
s::Stmt ::= b::Block c::Catches b1::Block {
 s.neededCurrentPackageTypes = b.neededCurrentPackageTypes ++ c.neededCurrentPackageTypes ++ b1.neededCurrentPackageTypes;
}

aspect production assert
s::Stmt ::= expr::Expr {
 s.neededCurrentPackageTypes = expr.neededCurrentPackageTypes;
}

aspect production assert_colon
s::Stmt ::= expr1::Expr expr2::Expr {
 s.neededCurrentPackageTypes = expr1.neededCurrentPackageTypes ++ expr2.neededCurrentPackageTypes;
}

aspect production catches_none
c::Catches ::= {
 c.neededCurrentPackageTypes = [];
}

aspect production catches_snoc
c::Catches ::= cs::Catches cc::Catch {
 c.neededCurrentPackageTypes = cs.neededCurrentPackageTypes ++ cc.neededCurrentPackageTypes;
}

aspect production catch
c::Catch ::= fp::Formal_Param b::Block {
 c.neededCurrentPackageTypes = fp.neededCurrentPackageTypes ++ b.neededCurrentPackageTypes;
}

aspect production while_prod
while::Stmt ::= t::While_t cond::Expr body::Stmt {
 while.neededCurrentPackageTypes = cond.neededCurrentPackageTypes ++ body.neededCurrentPackageTypes;
}

aspect production do
dowhile::Stmt ::= body::Stmt t::While_t cond::Expr {
 dowhile.neededCurrentPackageTypes = body.neededCurrentPackageTypes ++ cond.neededCurrentPackageTypes;
}

aspect production label_prod
lstmt::Stmt ::= id::Id_t stmt::Stmt {
 lstmt.neededCurrentPackageTypes = stmt.neededCurrentPackageTypes;
}

aspect production break_prod
break::Stmt ::= {
 break.neededCurrentPackageTypes = [];
}

aspect production break_label
break::Stmt ::= id::Id_t {
 break.neededCurrentPackageTypes = [];
}

aspect production continue_prod
continue::Stmt ::= {
 continue.neededCurrentPackageTypes = [];
}

aspect production continue_label
continue::Stmt ::= id::Id_t {
 continue.neededCurrentPackageTypes = [];
}

aspect production if_then
ifthen::Stmt ::= t::If_t expr::Expr thenbody::Stmt {
 ifthen.neededCurrentPackageTypes = expr.neededCurrentPackageTypes ++ thenbody.neededCurrentPackageTypes;
}

aspect production if_then_else
ifthenelse::Stmt ::= t::If_t expr::Expr thenbody::Stmt elsebody::Stmt {
 ifthenelse.neededCurrentPackageTypes = expr.neededCurrentPackageTypes ++ thenbody.neededCurrentPackageTypes ++ elsebody.neededCurrentPackageTypes;
}

aspect production for
forstmt::Stmt ::= init::For_Init test::For_Test update::For_Update body::Stmt {
 forstmt.neededCurrentPackageTypes = init.neededCurrentPackageTypes ++ test.neededCurrentPackageTypes ++ update.neededCurrentPackageTypes ++ body.neededCurrentPackageTypes;
}

aspect production for_init_empty
forinit::For_Init ::= {
 forinit.neededCurrentPackageTypes = [];
}

aspect production for_init_some
forinit::For_Init ::= ses::Stmt_Exprs {
 forinit.neededCurrentPackageTypes = ses.neededCurrentPackageTypes;
}

aspect production for_init_dcl
forinit::For_Init ::= dcl::Local_Var_Dcl {
 forinit.neededCurrentPackageTypes = dcl.neededCurrentPackageTypes;
}

aspect production for_test_none
fortest::For_Test ::= {
 fortest.neededCurrentPackageTypes = [];
}

aspect production for_test_one
fortest::For_Test ::= e::Expr {
 fortest.neededCurrentPackageTypes = e.neededCurrentPackageTypes;
}

aspect production for_update_empty
forupdate::For_Update ::= {
 forupdate.neededCurrentPackageTypes = [];
}

aspect production for_update_some
forupdate::For_Update ::= ses::Stmt_Exprs {
 forupdate.neededCurrentPackageTypes = ses.neededCurrentPackageTypes;
}

aspect production stmt_exprs_one
ses::Stmt_Exprs ::= se::Stmt_Expr {
 ses.neededCurrentPackageTypes = se.neededCurrentPackageTypes;
}

aspect production stmt_exprs_snoc
ses::Stmt_Exprs ::= ses1::Stmt_Exprs se::Stmt_Expr {
 ses.neededCurrentPackageTypes = ses1.neededCurrentPackageTypes ++ se.neededCurrentPackageTypes;
}

aspect production switch_prod
switch::Stmt ::= expr::Expr switchblock::Switch_Block {
 switch.neededCurrentPackageTypes = expr.neededCurrentPackageTypes ++ switchblock.neededCurrentPackageTypes;
}

aspect production switch_block
switchblock::Switch_Block ::= gs::Switch_Groups ls::Switch_Labels {
 switchblock.neededCurrentPackageTypes = gs.neededCurrentPackageTypes ++ ls.neededCurrentPackageTypes;
}

aspect production switch_block_no_labels
switchblock::Switch_Block ::= s::Switch_Groups {
 switchblock.neededCurrentPackageTypes = s.neededCurrentPackageTypes;
}

aspect production switch_block_no_groups
switchblock::Switch_Block ::= s::Switch_Labels {
 switchblock.neededCurrentPackageTypes = s.neededCurrentPackageTypes;
}

aspect production switch_block_empty
switchblock::Switch_Block ::= {
 switchblock.neededCurrentPackageTypes = [];
}

aspect production switch_groups_one
switch::Switch_Groups ::= item::Switch_Group {
 switch.neededCurrentPackageTypes = item.neededCurrentPackageTypes;
}

aspect production switch_groups_snoc
switch::Switch_Groups ::= list::Switch_Groups item::Switch_Group {
 switch.neededCurrentPackageTypes = list.neededCurrentPackageTypes ++ item.neededCurrentPackageTypes;
}

aspect production switch_group
switch::Switch_Group ::= label_list::Switch_Labels stmts::Stmt {
 switch.neededCurrentPackageTypes = label_list.neededCurrentPackageTypes ++ stmts.neededCurrentPackageTypes;
}

aspect production switch_labels_one
switchlabels::Switch_Labels ::= switchlabel::Switch_Label {
 switchlabels.neededCurrentPackageTypes = switchlabel.neededCurrentPackageTypes;
}

aspect production switch_labels_snoc
switchlabels::Switch_Labels ::=  list::Switch_Labels item::Switch_Label {
 switchlabels.neededCurrentPackageTypes = list.neededCurrentPackageTypes ++ item.neededCurrentPackageTypes;
}

aspect production switch_label
label::Switch_Label ::= expr::Expr {
 label.neededCurrentPackageTypes = expr.neededCurrentPackageTypes;
}

aspect production switch_label_default
label::Switch_Label ::= {
 label.neededCurrentPackageTypes = [];
}

aspect production type_class_dcl
td::Type_Dcl ::= cdcl::Class_Dcl {
 td.neededCurrentPackageTypes = cdcl.neededCurrentPackageTypes;
}

aspect production class_dcl_seq
cdcl::Class_Dcl ::= cdcl1::Class_Dcl cdcl2::Class_Dcl {
 cdcl.neededCurrentPackageTypes = cdcl1.neededCurrentPackageTypes ++ cdcl2.neededCurrentPackageTypes;
}

aspect production class_dcl_none
cdcl::Class_Dcl ::= {
 cdcl.neededCurrentPackageTypes = [];
}

aspect production class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.neededCurrentPackageTypes = parent.neededCurrentPackageTypes ++ inters.neededCurrentPackageTypes ++ cb.neededCurrentPackageTypes ;
}

aspect production class_body
cb::Class_Body ::= dcls::Class_Member_Dcls {
 cb.neededCurrentPackageTypes = dcls.neededCurrentPackageTypes;
}

aspect production class_member_dcls_none
cdcls::Class_Member_Dcls ::= {
 cdcls.neededCurrentPackageTypes = [];
}

aspect production class_member_dcls_snoc
cdcls::Class_Member_Dcls ::= cdcls1::Class_Member_Dcls cdcl::Class_Member_Dcl {
 cdcls.neededCurrentPackageTypes = cdcls1.neededCurrentPackageTypes ++ cdcl.neededCurrentPackageTypes;
}

aspect production class_member_dcls_one
cdcls::Class_Member_Dcls ::= cdcl::Class_Member_Dcl {
 cdcls.neededCurrentPackageTypes = cdcl.neededCurrentPackageTypes;
}

aspect production class_member_dcl_seq
cdcl::Class_Member_Dcl ::= cdcl1::Class_Member_Dcl cdcl2::Class_Member_Dcl {
 cdcl.neededCurrentPackageTypes = cdcl1.neededCurrentPackageTypes ++ cdcl2.neededCurrentPackageTypes;
}

aspect production class_member_empty
cdcl::Class_Member_Dcl ::= {
 cdcl.neededCurrentPackageTypes = [];
}

aspect production inner_class
cdcl::Class_Member_Dcl ::= cd::Class_Dcl {
 cdcl.neededCurrentPackageTypes = cd.neededCurrentPackageTypes;
}

aspect production class_constructor
cdcl::Class_Member_Dcl ::= mods::Modifiers id::Id_t fps::Formal_Params thr::Throws cb::Block {
 cdcl.neededCurrentPackageTypes = fps.neededCurrentPackageTypes ++ thr.neededCurrentPackageTypes ++ cb.neededCurrentPackageTypes;
}

aspect production inner_interface
cdcl::Class_Member_Dcl ::= id::Interface_Dcl {
 cdcl.neededCurrentPackageTypes = id.neededCurrentPackageTypes;
}

aspect production class_block
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.neededCurrentPackageTypes = b.neededCurrentPackageTypes;
}

aspect production class_static_initializer
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.neededCurrentPackageTypes = b.neededCurrentPackageTypes;
}

aspect production class_field
cdcl::Class_Member_Dcl ::= f::Field_Dcl {
 cdcl.neededCurrentPackageTypes = f.neededCurrentPackageTypes;
}

aspect production class_method
cdcl::Class_Member_Dcl ::= mdcl::Method_Dcl {
 cdcl.neededCurrentPackageTypes = mdcl.neededCurrentPackageTypes;
}

aspect production stmt_dcl
stmt::Stmt ::= vdcl::Local_Var_Dcl {
 stmt.neededCurrentPackageTypes = vdcl.neededCurrentPackageTypes;
}

aspect production local_var_dcl
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
 vdcl.neededCurrentPackageTypes = dcltype.neededCurrentPackageTypes ++ dcls.neededCurrentPackageTypes;
}

aspect production field_dcl
f::Field_Dcl ::= mods::Modifiers t::Type dcls::Var_Declarators {
 f.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ dcls.neededCurrentPackageTypes;
}

aspect production var_declarator
vdcl::Var_Declarator ::= v::Var_Declarator_Id {
 vdcl.neededCurrentPackageTypes = v.neededCurrentPackageTypes;
}

aspect production var_declarator_init
vdcl::Var_Declarator ::= v::Var_Declarator_Id init::Var_Init {
 vdcl.neededCurrentPackageTypes = v.neededCurrentPackageTypes ++ init.neededCurrentPackageTypes;
}

aspect production var_declarator_id
vdcl::Var_Declarator_Id ::= id::Id_t {
 vdcl.neededCurrentPackageTypes = [];
}

aspect production var_init_expr
init::Var_Init ::= e::Expr {
 init.neededCurrentPackageTypes = e.neededCurrentPackageTypes;
}

aspect production local_var_dcl_seq
vdcl::Local_Var_Dcl ::= vdcl1::Local_Var_Dcl vdcl2::Local_Var_Dcl {
 vdcl.neededCurrentPackageTypes = vdcl1.neededCurrentPackageTypes ++ vdcl2.neededCurrentPackageTypes;
}

aspect production local_var_dcl_one
vdcl::Local_Var_Dcl ::= dcltype::Type dcl::Var_Declarator {
 vdcl.neededCurrentPackageTypes = dcltype.neededCurrentPackageTypes ++ dcl.neededCurrentPackageTypes;
}

aspect production field_dcl_seq
fdcl::Field_Dcl ::= fdcl1::Field_Dcl fdcl2::Field_Dcl {
 fdcl.neededCurrentPackageTypes = fdcl1.neededCurrentPackageTypes ++ fdcl2.neededCurrentPackageTypes;
}

aspect production field_dcl_one
f::Field_Dcl ::= mods::Modifiers t::Type dcl::Var_Declarator {
 f.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ dcl.neededCurrentPackageTypes;
}

aspect production var_declarators_one
vdcls::Var_Declarators ::= vdcl::Var_Declarator {
 vdcls.neededCurrentPackageTypes = vdcl.neededCurrentPackageTypes;
}

aspect production var_declarators_snoc
vdcls::Var_Declarators ::= vdcls1::Var_Declarators vdcl::Var_Declarator {
 vdcls.neededCurrentPackageTypes = vdcls1.neededCurrentPackageTypes ++ vdcl.neededCurrentPackageTypes;
}

aspect production local_var_dcl_final
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
 vdcl.neededCurrentPackageTypes = dcltype.neededCurrentPackageTypes ++ dcls.neededCurrentPackageTypes;
}

aspect production var_declarator_array
vdcl::Var_Declarator_Id ::= v::Var_Declarator_Id {
 vdcl.neededCurrentPackageTypes = v.neededCurrentPackageTypes;
}

aspect production var_init_array
init::Var_Init ::= e::Array_Init {
 init.neededCurrentPackageTypes = e.neededCurrentPackageTypes;
}

aspect production array_init
ai::Array_Init ::= vs::Var_Inits {
 ai.neededCurrentPackageTypes = vs.neededCurrentPackageTypes;
}

aspect production array_init_no_comma
ai::Array_Init ::= vs::Var_Inits {
 ai.neededCurrentPackageTypes = vs.neededCurrentPackageTypes;
}

aspect production array_init_no_var_inits
ai::Array_Init ::= {
 ai.neededCurrentPackageTypes = [];
}

aspect production array_init_empty
ai::Array_Init ::= {
 ai.neededCurrentPackageTypes = [];
}

aspect production var_inits_one
vis::Var_Inits ::= vi::Var_Init {
 vis.neededCurrentPackageTypes = vi.neededCurrentPackageTypes;
}

aspect production var_inits_snoc
vis::Var_Inits ::= vis1::Var_Inits vi::Var_Init {
 vis.neededCurrentPackageTypes = vis1.neededCurrentPackageTypes ++ vi.neededCurrentPackageTypes;
}

aspect production method_dcl_prod
mdcl::Method_Dcl ::=  mh::Method_Header b::Block {
 mdcl.neededCurrentPackageTypes = mh.neededCurrentPackageTypes ++ b.neededCurrentPackageTypes;
}

aspect production method_dcl_no_body
mdcl::Method_Dcl ::= mh::Method_Header {
 mdcl.neededCurrentPackageTypes = mh.neededCurrentPackageTypes;
}

aspect production method_header_prod
mh::Method_Header ::= mods::Modifiers return_ty::Type mname::Id_t fps::Formal_Params th::Throws {
 mh.neededCurrentPackageTypes = return_ty.neededCurrentPackageTypes ++ fps.neededCurrentPackageTypes ++ th.neededCurrentPackageTypes;
}

aspect production void_type
t::Type ::= {
 t.neededCurrentPackageTypes = [];
}

aspect production formal_params_none
fps::Formal_Params ::= {
 fps.neededCurrentPackageTypes = [];
}

aspect production formal_params_one
fps::Formal_Params ::= fp::Formal_Param {
 fps.neededCurrentPackageTypes = fp.neededCurrentPackageTypes;
}

aspect production formal_params_snoc
fps::Formal_Params ::= fps1::Formal_Params fp::Formal_Param {
 fps.neededCurrentPackageTypes = fps1.neededCurrentPackageTypes ++ fp.neededCurrentPackageTypes;
}

aspect production formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
 fp.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ vid.neededCurrentPackageTypes;
}

aspect production return_statement
s::Stmt ::= t::Return_t {
 s.neededCurrentPackageTypes = [];
}

aspect production return_expr
s::Stmt ::= t::Return_t e::Expr {
 s.neededCurrentPackageTypes = e.neededCurrentPackageTypes;
}

aspect production method_header_declarator
mh::Method_Header ::= mods::Modifiers t::Type md::Method_Declarator th::Throws {
 mh.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ md.neededCurrentPackageTypes ++ th.neededCurrentPackageTypes;
}

aspect production method_declarator
md::Method_Declarator ::= id::Id_t fps::Formal_Params {
 md.neededCurrentPackageTypes = fps.neededCurrentPackageTypes;
}

aspect production method_declarator_array
md::Method_Declarator ::= md1::Method_Declarator {
 md.neededCurrentPackageTypes = md1.neededCurrentPackageTypes;
}

aspect production final_formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
 fp.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ vid.neededCurrentPackageTypes;
}

aspect production throws_none
th::Throws ::= {
 th.neededCurrentPackageTypes = [];
}

aspect production throws
th::Throws ::= ctl::TypeNames {
 th.neededCurrentPackageTypes = ctl.neededCurrentPackageTypes;
}

aspect production stmt_constructor_invocation
stmt::Stmt ::= inv::Constructor_Invocation {
 stmt.neededCurrentPackageTypes = inv.neededCurrentPackageTypes;
}

aspect production this_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
 inv.neededCurrentPackageTypes = args.neededCurrentPackageTypes;
}

aspect production super_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
 inv.neededCurrentPackageTypes = args.neededCurrentPackageTypes;
}

aspect production this_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
 inv.neededCurrentPackageTypes = expr.neededCurrentPackageTypes ++ args.neededCurrentPackageTypes;
}

aspect production this_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
 inv.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ args.neededCurrentPackageTypes;
}

aspect production super_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
 inv.neededCurrentPackageTypes = expr.neededCurrentPackageTypes ++ args.neededCurrentPackageTypes;
}

aspect production super_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
 inv.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ args.neededCurrentPackageTypes;
}

aspect production primitive_type
t::Type ::= t1::Primitive_Type {
 t.neededCurrentPackageTypes = [];
}

aspect production reference_type
t::Type ::= t1::Reference_Type {
 t.neededCurrentPackageTypes = t1.neededCurrentPackageTypes;
}

aspect production type_typerep
t::Type ::= tr::TypeRep {
 t.neededCurrentPackageTypes = [];
}

aspect production name_type
t::Reference_Type ::= n::TypeName {
 t.neededCurrentPackageTypes = n.neededCurrentPackageTypes;
}

aspect production type_names_none
ns::TypeNames ::= {
 ns.neededCurrentPackageTypes = [];
}

aspect production type_names_one
ns::TypeNames ::= n::TypeName {
 ns.neededCurrentPackageTypes = n.neededCurrentPackageTypes;
}

aspect production type_names_snoc
ns::TypeNames ::= ns1::TypeNames n::TypeName {
 ns.neededCurrentPackageTypes = ns1.neededCurrentPackageTypes ++ n.neededCurrentPackageTypes;
}

aspect production type_interface_dcl
td::Type_Dcl ::= idcl::Interface_Dcl {
 td.neededCurrentPackageTypes = idcl.neededCurrentPackageTypes;
}

aspect production interface_member_dcls_none
idcls::Interface_Member_Dcls ::= {
 idcls.neededCurrentPackageTypes = [];
}

aspect production interface_member_dcls_snoc
idcls::Interface_Member_Dcls ::= idcls1::Interface_Member_Dcls idcl::Interface_Member_Dcl {
 idcls.neededCurrentPackageTypes = idcls1.neededCurrentPackageTypes ++ idcl.neededCurrentPackageTypes;
}

aspect production interface_field
idcl::Interface_Member_Dcl ::= fd::Field_Dcl {
 idcl.neededCurrentPackageTypes = fd.neededCurrentPackageTypes;
}

aspect production interface_method
idcl::Interface_Member_Dcl ::= mh::Method_Header {
 idcl.neededCurrentPackageTypes = mh.neededCurrentPackageTypes;
}

aspect production interface_empty
idcl::Interface_Member_Dcl ::= {
 idcl.neededCurrentPackageTypes = [];
}

aspect production interface_dcl
idcl::Interface_Dcl ::= mods::Modifiers iname::Id_t inters::TypeNames dcls::Interface_Member_Dcls {
 idcl.neededCurrentPackageTypes = inters.neededCurrentPackageTypes ++ dcls.neededCurrentPackageTypes;
}

aspect production interface_inner_class
idcl::Interface_Member_Dcl ::= cd::Class_Dcl {
 idcl.neededCurrentPackageTypes = cd.neededCurrentPackageTypes;
}

aspect production interface_inner_interface
idcl::Interface_Member_Dcl ::= id::Interface_Dcl {
 idcl.neededCurrentPackageTypes = id.neededCurrentPackageTypes;
}

aspect production cast_name_array
e::Expr ::= t::TypeName dims::Integer e1::Expr {
 e.neededCurrentPackageTypes = t.neededCurrentPackageTypes ++ e1.neededCurrentPackageTypes;
}

aspect production cast_primitive_array
e::Expr ::= t::Primitive_Type dims::Integer e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production not
e::Expr ::= e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production or_or
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production and_and
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production super_field_access
lhs::LHS ::= id::Id_t {
 lhs.neededCurrentPackageTypes = [];
}

aspect production name_super_field_access
lhs::LHS ::= n::TypeName id::Id_t {
 lhs.neededCurrentPackageTypes = n.neededCurrentPackageTypes;
}

aspect production super_method_call
e::Stmt_Expr ::= id::Id_t es::Exprs {
 e.neededCurrentPackageTypes = es.neededCurrentPackageTypes;
}

aspect production name_super_method_call
e::Stmt_Expr ::= n::TypeName id::Id_t es::Exprs {
 e.neededCurrentPackageTypes = n.neededCurrentPackageTypes ++ es.neededCurrentPackageTypes;
}

aspect production array_access
lhs::LHS ::= n::ExprName e1::Expr {
 lhs.neededCurrentPackageTypes = n.neededCurrentPackageTypes ++ e1.neededCurrentPackageTypes;
}

aspect production array_access_general
lhs::LHS ::= e1::Expr e2::Expr {
 lhs.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production mul_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production div_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production mod_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production plus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production minus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production lshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production rshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production urshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production and_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production xor_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production or_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ expr.neededCurrentPackageTypes;
}

aspect production new_class_expr
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs {
 e.neededCurrentPackageTypes = expr.neededCurrentPackageTypes ++ es.neededCurrentPackageTypes;
}

aspect production new_class_expr_body
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs cb::Class_Body {
 e.neededCurrentPackageTypes = expr.neededCurrentPackageTypes ++ es.neededCurrentPackageTypes ++ cb.neededCurrentPackageTypes;
}

aspect production new_class_name
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs {
 e.neededCurrentPackageTypes = nam.neededCurrentPackageTypes ++ es.neededCurrentPackageTypes;
}

aspect production new_class_name_body
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs cb::Class_Body {
 e.neededCurrentPackageTypes = nam.neededCurrentPackageTypes ++ es.neededCurrentPackageTypes ++ cb.neededCurrentPackageTypes;
}

aspect production conditional
e::Expr ::= cond::Expr thenexpr::Expr elseexpr::Expr {
 e.neededCurrentPackageTypes = cond.neededCurrentPackageTypes ++ thenexpr.neededCurrentPackageTypes ++ elseexpr.neededCurrentPackageTypes;
}

aspect production or
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production xor
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production and
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production eq
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production not_eq
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production lt
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production gt
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production lteq
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production gteq
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production instanceof
e::Expr ::= e1::Expr t1::Reference_Type {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ t1.neededCurrentPackageTypes;
}

aspect production lshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production rshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production urshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production div
e::Expr ::= e1::Expr _ e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production mod
e::Expr ::= e1::Expr _ e2::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes ++ e2.neededCurrentPackageTypes;
}

aspect production unary_plus
e::Expr ::= e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production unary_minus
e::Expr ::= e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production pre_inc
e::Stmt_Expr ::= e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production pre_dec
e::Stmt_Expr ::= e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production comp
e::Expr ::= e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production post_inc
e::Stmt_Expr ::= e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production post_dec
e::Stmt_Expr ::= e1::Expr {
 e.neededCurrentPackageTypes = e1.neededCurrentPackageTypes;
}

aspect production new_array_init_primitive
e::Expr ::= t1::Primitive_Type dims::Integer ai::Array_Init {
 e.neededCurrentPackageTypes = ai.neededCurrentPackageTypes;
}

aspect production new_array_init_name
e::Expr ::= n::TypeName dims::Integer ai::Array_Init {
 e.neededCurrentPackageTypes = n.neededCurrentPackageTypes ++ ai.neededCurrentPackageTypes;
}

aspect production new_array_no_init_primitive
e::Expr ::= t1::Primitive_Type d1::Dim_Exprs dims::Integer {
 e.neededCurrentPackageTypes = d1.neededCurrentPackageTypes;
}

aspect production new_array_no_init_name
e::Expr ::= n::TypeName d1::Dim_Exprs dims::Integer {
 e.neededCurrentPackageTypes = n.neededCurrentPackageTypes ++ d1.neededCurrentPackageTypes;
}

aspect production primitive_dot_class
e::Expr ::= t::Primitive_Type {
 e.neededCurrentPackageTypes = [];
}

aspect production void_dot_class
e::Expr ::= {
 e.neededCurrentPackageTypes = [];
}

aspect production array_dot_class
e::Expr ::= t::Array_Type {
 e.neededCurrentPackageTypes = t.neededCurrentPackageTypes;
}

aspect production name_dot_class
e::Expr ::= n::TypeName {
 e.neededCurrentPackageTypes = n.neededCurrentPackageTypes;
}

aspect production name_dot_this
e::Expr ::= n::TypeName {
 e.neededCurrentPackageTypes = n.neededCurrentPackageTypes;
}

aspect production dim_exprs_one
d::Dim_Exprs ::= e::Expr {
 d.neededCurrentPackageTypes = e.neededCurrentPackageTypes;
}

aspect production dim_exprs_snoc
d::Dim_Exprs ::= d1::Dim_Exprs e::Expr {
 d.neededCurrentPackageTypes = d1.neededCurrentPackageTypes ++ e.neededCurrentPackageTypes;
}

aspect production array_type
t::Reference_Type ::= t1::Array_Type {
 t.neededCurrentPackageTypes = t1.neededCurrentPackageTypes;
}

aspect production primitive_array
t::Array_Type ::= t1::Primitive_Type ds::Integer {
 t.neededCurrentPackageTypes = [];
}

aspect production name_array
t::Array_Type ::= n::TypeName ds::Integer {
 t.neededCurrentPackageTypes = n.neededCurrentPackageTypes;
}

aspect production simple_package_name
pn::PackageName ::= id::Id_t {
 pn.neededCurrentPackageTypes = [];
}

aspect production qualified_package_name
pn::PackageName ::= pn2::PackageName id::Id_t {
 pn.neededCurrentPackageTypes = pn2.neededCurrentPackageTypes;
}

aspect production simple_package_or_type_name 
ptn::PackageOrTypeName ::= id::Id_t {
}

aspect production qualified_package_or_type_name
ptn::PackageOrTypeName ::= pn::PackageOrTypeName id::Id_t {
 ptn.neededCurrentPackageTypes = pn.neededCurrentPackageTypes;
}

aspect production simple_type_name
tn::TypeName ::= id::Id_t {
}

aspect production qualified_type_name
tn::TypeName ::= pn::PackageOrTypeName id::Id_t {
 tn.neededCurrentPackageTypes = pn.neededCurrentPackageTypes;
}

aspect production simple_expr_name
en::ExprName ::= id::Id_t {
 en.neededCurrentPackageTypes = [];
}

aspect production qualified_expr_name
en::ExprName ::= an::AmbiguousName id::Id_t {
 en.neededCurrentPackageTypes = an.neededCurrentPackageTypes;
}

aspect production simple_method_name
mn::MethodName ::= id::Id_t  { 
 mn.neededCurrentPackageTypes = [];
}

aspect production qualified_method_name
mn::MethodName ::= an::AmbiguousName id::Id_t {
 mn.neededCurrentPackageTypes = an.neededCurrentPackageTypes;
}

aspect production simple_ambiguous_name
an::AmbiguousName ::= id::Id_t {
}

aspect production qualified_ambiguous_name
andi::AmbiguousName ::= an::AmbiguousName id::Id_t {
 andi.neededCurrentPackageTypes = an.neededCurrentPackageTypes;
}

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

aspect production compilation_unit
r::Root ::= pd::Package_Dcl ids::Import_Dcls tds::Type_Dcls {
 r.neededImportedOnDemandTypes = tds.neededImportedOnDemandTypes;
}

aspect production package_dcl
p::Package_Dcl ::= n::PackageName {
}

aspect production package_dcl_none
p::Package_Dcl ::= {
}

aspect production import_dcls_none
idcls::Import_Dcls ::= {
}

aspect production import_dcls_snoc
idcls::Import_Dcls ::= idcls1::Import_Dcls idcl::Import_Dcl {
}

aspect production import_dcl
idcl::Import_Dcl ::= t::Import_t n::TypeName {
}

aspect production import_dcl_on_demand
idcl::Import_Dcl ::= n::PackageOrTypeName {
}

aspect production type_dcls_none
tdcls::Type_Dcls ::= {
 tdcls.neededImportedOnDemandTypes =  [];
}

aspect production type_dcls_snoc
tdcls::Type_Dcls ::= tdcls1::Type_Dcls tdcl::Type_Dcl {
 tdcls.neededImportedOnDemandTypes =  tdcls1.neededImportedOnDemandTypes ++ tdcl.neededImportedOnDemandTypes;
}

aspect production type_dcl_empty
tdcl::Type_Dcl ::= {
 tdcl.neededImportedOnDemandTypes = [];
}

aspect production lhs_name
lhs::LHS ::= nm::ExprName {
 lhs.neededImportedOnDemandTypes = nm.neededImportedOnDemandTypes;
}

aspect production expr_lhs
expr::Expr ::= lhs::LHS {
 expr.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes;
}

aspect production expr_stmt_expr
e::Expr ::= se::Stmt_Expr {
 e.neededImportedOnDemandTypes = se.neededImportedOnDemandTypes;
}

aspect production this
e::Expr ::= {
 e.neededImportedOnDemandTypes = [];
}

aspect production expr_field_access
lhs::LHS ::= e1::Expr id::Id_t {
 lhs.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production method_call
e::Stmt_Expr ::= name1::MethodName args::Exprs {
 e.neededImportedOnDemandTypes = name1.neededImportedOnDemandTypes ++ args.neededImportedOnDemandTypes;
}

aspect production expr_method_call
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs {
 e.neededImportedOnDemandTypes = obj.neededImportedOnDemandTypes ++ params.neededImportedOnDemandTypes;
}

aspect production resolved_method_call_copy
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs rettr::TypeRep {
 e.neededImportedOnDemandTypes = obj.neededImportedOnDemandTypes ++ params.neededImportedOnDemandTypes;
}

aspect production new_class
e::Stmt_Expr ::= t::TypeName params::Exprs {
 e.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ params.neededImportedOnDemandTypes;
}

aspect production new_class_body
e::Stmt_Expr ::= t::TypeName params::Exprs cb::Class_Body {
 e.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ params.neededImportedOnDemandTypes ++ cb.neededImportedOnDemandTypes;
}

aspect production resolved_new_class
e::Stmt_Expr ::= tr::TypeRep params::Exprs {
 e.neededImportedOnDemandTypes = params.neededImportedOnDemandTypes;
}

aspect production resolved_new_class_body
e::Stmt_Expr ::= tr::TypeRep params::Exprs cb::Class_Body {
 e.neededImportedOnDemandTypes = params.neededImportedOnDemandTypes ++ cb.neededImportedOnDemandTypes;
}

aspect production cast_prod
e::Expr ::= name1::TypeName e1::Expr {
 e.neededImportedOnDemandTypes = name1.neededImportedOnDemandTypes ++ e1.neededImportedOnDemandTypes;
}

aspect production resolved_cast
e::Expr ::= t::TypeRep e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production assign
a::Stmt_Expr ::= lhs::LHS t::Eq_t expr::Expr {
 a.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production plus
e::Expr ::= e1::Expr t::Plus_t e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production minus
e::Expr ::= e1::Expr t::Minus_t e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production mul
e::Expr ::= e1::Expr t::Mul_t e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production byte_const
e::Expr ::= t::String {
 e.neededImportedOnDemandTypes = [];
}

aspect production short_const
e::Expr ::= t::String {
 e.neededImportedOnDemandTypes = [];
}

aspect production char_const
e::Expr ::= t::String {
 e.neededImportedOnDemandTypes = [];
}

aspect production int_const
e::Expr ::= t::String {
 e.neededImportedOnDemandTypes = [];
}

aspect production long_const
e::Expr ::= t::String {
 e.neededImportedOnDemandTypes = [];
}

aspect production float_const
e::Expr ::= t::String {
 e.neededImportedOnDemandTypes = [];
}

aspect production double_const
e::Expr ::= t::String {
 e.neededImportedOnDemandTypes = [];
}

aspect production true_const
e::Expr ::= {
 e.neededImportedOnDemandTypes = [];
}

aspect production false_const
e::Expr ::= {
 e.neededImportedOnDemandTypes = [];
}

aspect production string_const
e::Expr ::= t::String {
 e.neededImportedOnDemandTypes = [];
}

aspect production null_const
e::Expr ::= {
 e.neededImportedOnDemandTypes = [];
}

aspect production exprs_none
es::Exprs ::= {
 es.neededImportedOnDemandTypes = [];
}

aspect production exprs_one
es::Exprs ::= e::Expr {
 es.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes;
}

aspect production exprs_snoc
es::Exprs ::= es1::Exprs e::Expr {
 es.neededImportedOnDemandTypes = es1.neededImportedOnDemandTypes ++ e.neededImportedOnDemandTypes;
}

aspect production exprs_cons
es::Exprs ::= e::Expr es1::Exprs {
 es.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes ++ es1.neededImportedOnDemandTypes;
}

aspect production cast_primitive
e::Expr ::= t::Primitive_Type e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production cast_simple
e::Expr ::= t::Expr e1::Expr {
 e.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ e1.neededImportedOnDemandTypes;
}

aspect production block
b::Block ::= stmt::Stmt {
 b.neededImportedOnDemandTypes = stmt.neededImportedOnDemandTypes;
}

aspect production empty_block
b::Block ::= {
 b.neededImportedOnDemandTypes = [];
}

aspect production stmt_seq
seq::Stmt ::= stmt1::Stmt stmt2::Stmt {
 seq.neededImportedOnDemandTypes = stmt1.neededImportedOnDemandTypes ++ stmt2.neededImportedOnDemandTypes;
}

aspect production stmt_block
stmt::Stmt ::= b::Block {
 stmt.neededImportedOnDemandTypes = b.neededImportedOnDemandTypes;
}

aspect production empty_stmt
s::Stmt ::= {
 s.neededImportedOnDemandTypes = [];
}

aspect production erroneous_Stmt
s::Stmt ::= err_tree::Stmt errs::[Error] {
 s.neededImportedOnDemandTypes = [];
}

aspect production stmt_stmt_expr
s::Stmt ::= s1::Stmt_Expr {
 s.neededImportedOnDemandTypes = s1.neededImportedOnDemandTypes;
}

aspect production block_stmt_class
s::Stmt ::= cdcl::Class_Dcl {
 s.neededImportedOnDemandTypes = cdcl.neededImportedOnDemandTypes;
}

aspect production block_stmt_interface
s::Stmt ::= idcl::Interface_Dcl {
 s.neededImportedOnDemandTypes = idcl.neededImportedOnDemandTypes;
}

aspect production synchronized
s::Stmt ::= e::Expr b::Block {
 s.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes ++ b.neededImportedOnDemandTypes;
}

aspect production throw
s::Stmt ::= e::Expr {
 s.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes;
}

aspect production try
s::Stmt ::= b::Block c::Catches {
 s.neededImportedOnDemandTypes = b.neededImportedOnDemandTypes ++ c.neededImportedOnDemandTypes;
}

aspect production try_finally
s::Stmt ::= b::Block c::Catches b1::Block {
 s.neededImportedOnDemandTypes = b.neededImportedOnDemandTypes ++ c.neededImportedOnDemandTypes ++ b1.neededImportedOnDemandTypes;
}

aspect production assert
s::Stmt ::= expr::Expr {
 s.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes;
}

aspect production assert_colon
s::Stmt ::= expr1::Expr expr2::Expr {
 s.neededImportedOnDemandTypes = expr1.neededImportedOnDemandTypes ++ expr2.neededImportedOnDemandTypes;
}

aspect production catches_none
c::Catches ::= {
 c.neededImportedOnDemandTypes = [];
}

aspect production catches_snoc
c::Catches ::= cs::Catches cc::Catch {
 c.neededImportedOnDemandTypes = cs.neededImportedOnDemandTypes ++ cc.neededImportedOnDemandTypes;
}

aspect production catch
c::Catch ::= fp::Formal_Param b::Block {
 c.neededImportedOnDemandTypes = fp.neededImportedOnDemandTypes ++ b.neededImportedOnDemandTypes;
}

aspect production while_prod
while::Stmt ::= t::While_t cond::Expr body::Stmt {
 while.neededImportedOnDemandTypes = cond.neededImportedOnDemandTypes ++ body.neededImportedOnDemandTypes;
}

aspect production do
dowhile::Stmt ::= body::Stmt t::While_t cond::Expr {
 dowhile.neededImportedOnDemandTypes = body.neededImportedOnDemandTypes ++ cond.neededImportedOnDemandTypes;
}

aspect production label_prod
lstmt::Stmt ::= id::Id_t stmt::Stmt {
 lstmt.neededImportedOnDemandTypes = stmt.neededImportedOnDemandTypes;
}

aspect production break_prod
break::Stmt ::= {
 break.neededImportedOnDemandTypes = [];
}

aspect production break_label
break::Stmt ::= id::Id_t {
 break.neededImportedOnDemandTypes = [];
}

aspect production continue_prod
continue::Stmt ::= {
 continue.neededImportedOnDemandTypes = [];
}

aspect production continue_label
continue::Stmt ::= id::Id_t {
 continue.neededImportedOnDemandTypes = [];
}

aspect production if_then
ifthen::Stmt ::= t::If_t expr::Expr thenbody::Stmt {
 ifthen.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes ++ thenbody.neededImportedOnDemandTypes;
}

aspect production if_then_else
ifthenelse::Stmt ::= t::If_t expr::Expr thenbody::Stmt elsebody::Stmt {
 ifthenelse.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes ++ thenbody.neededImportedOnDemandTypes ++ elsebody.neededImportedOnDemandTypes;
}

aspect production for
forstmt::Stmt ::= init::For_Init test::For_Test update::For_Update body::Stmt {
 forstmt.neededImportedOnDemandTypes = init.neededImportedOnDemandTypes ++ test.neededImportedOnDemandTypes ++ update.neededImportedOnDemandTypes ++ body.neededImportedOnDemandTypes;
}

aspect production for_init_empty
forinit::For_Init ::= {
 forinit.neededImportedOnDemandTypes = [];
}

aspect production for_init_some
forinit::For_Init ::= ses::Stmt_Exprs {
 forinit.neededImportedOnDemandTypes = ses.neededImportedOnDemandTypes;
}

aspect production for_init_dcl
forinit::For_Init ::= dcl::Local_Var_Dcl {
 forinit.neededImportedOnDemandTypes = dcl.neededImportedOnDemandTypes;
}

aspect production for_test_none
fortest::For_Test ::= {
 fortest.neededImportedOnDemandTypes = [];
}

aspect production for_test_one
fortest::For_Test ::= e::Expr {
 fortest.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes;
}

aspect production for_update_empty
forupdate::For_Update ::= {
 forupdate.neededImportedOnDemandTypes = [];
}

aspect production for_update_some
forupdate::For_Update ::= ses::Stmt_Exprs {
 forupdate.neededImportedOnDemandTypes = ses.neededImportedOnDemandTypes;
}

aspect production stmt_exprs_one
ses::Stmt_Exprs ::= se::Stmt_Expr {
 ses.neededImportedOnDemandTypes = se.neededImportedOnDemandTypes;
}

aspect production stmt_exprs_snoc
ses::Stmt_Exprs ::= ses1::Stmt_Exprs se::Stmt_Expr {
 ses.neededImportedOnDemandTypes = ses1.neededImportedOnDemandTypes ++ se.neededImportedOnDemandTypes;
}

aspect production switch_prod
switch::Stmt ::= expr::Expr switchblock::Switch_Block {
 switch.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes ++ switchblock.neededImportedOnDemandTypes;
}

aspect production switch_block
switchblock::Switch_Block ::= gs::Switch_Groups ls::Switch_Labels {
 switchblock.neededImportedOnDemandTypes = gs.neededImportedOnDemandTypes ++ ls.neededImportedOnDemandTypes;
}

aspect production switch_block_no_labels
switchblock::Switch_Block ::= s::Switch_Groups {
 switchblock.neededImportedOnDemandTypes = s.neededImportedOnDemandTypes;
}

aspect production switch_block_no_groups
switchblock::Switch_Block ::= s::Switch_Labels {
 switchblock.neededImportedOnDemandTypes = s.neededImportedOnDemandTypes;
}

aspect production switch_block_empty
switchblock::Switch_Block ::= {
 switchblock.neededImportedOnDemandTypes = [];
}

aspect production switch_groups_one
switch::Switch_Groups ::= item::Switch_Group {
 switch.neededImportedOnDemandTypes = item.neededImportedOnDemandTypes;
}

aspect production switch_groups_snoc
switch::Switch_Groups ::= list::Switch_Groups item::Switch_Group {
 switch.neededImportedOnDemandTypes = list.neededImportedOnDemandTypes ++ item.neededImportedOnDemandTypes;
}

aspect production switch_group
switch::Switch_Group ::= label_list::Switch_Labels stmts::Stmt {
 switch.neededImportedOnDemandTypes = label_list.neededImportedOnDemandTypes ++ stmts.neededImportedOnDemandTypes;
}

aspect production switch_labels_one
switchlabels::Switch_Labels ::= switchlabel::Switch_Label {
 switchlabels.neededImportedOnDemandTypes = switchlabel.neededImportedOnDemandTypes;
}

aspect production switch_labels_snoc
switchlabels::Switch_Labels ::=  list::Switch_Labels item::Switch_Label {
 switchlabels.neededImportedOnDemandTypes = list.neededImportedOnDemandTypes ++ item.neededImportedOnDemandTypes;
}

aspect production switch_label
label::Switch_Label ::= expr::Expr {
 label.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes;
}

aspect production switch_label_default
label::Switch_Label ::= {
 label.neededImportedOnDemandTypes = [];
}

aspect production type_class_dcl
td::Type_Dcl ::= cdcl::Class_Dcl {
 td.neededImportedOnDemandTypes = cdcl.neededImportedOnDemandTypes;
}

aspect production class_dcl_seq
cdcl::Class_Dcl ::= cdcl1::Class_Dcl cdcl2::Class_Dcl {
 cdcl.neededImportedOnDemandTypes = cdcl1.neededImportedOnDemandTypes ++ cdcl2.neededImportedOnDemandTypes;
}

aspect production class_dcl_none
cdcl::Class_Dcl ::= {
 cdcl.neededImportedOnDemandTypes = [];
}

aspect production class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.neededImportedOnDemandTypes = parent.neededImportedOnDemandTypes ++ inters.neededImportedOnDemandTypes ++ cb.neededImportedOnDemandTypes ;
}

aspect production class_body
cb::Class_Body ::= dcls::Class_Member_Dcls {
 cb.neededImportedOnDemandTypes = dcls.neededImportedOnDemandTypes;
}

aspect production class_member_dcls_none
cdcls::Class_Member_Dcls ::= {
 cdcls.neededImportedOnDemandTypes = [];
}

aspect production class_member_dcls_snoc
cdcls::Class_Member_Dcls ::= cdcls1::Class_Member_Dcls cdcl::Class_Member_Dcl {
 cdcls.neededImportedOnDemandTypes = cdcls1.neededImportedOnDemandTypes ++ cdcl.neededImportedOnDemandTypes;
}

aspect production class_member_dcls_one
cdcls::Class_Member_Dcls ::= cdcl::Class_Member_Dcl {
 cdcls.neededImportedOnDemandTypes = cdcl.neededImportedOnDemandTypes;
}

aspect production class_member_dcl_seq
cdcl::Class_Member_Dcl ::= cdcl1::Class_Member_Dcl cdcl2::Class_Member_Dcl {
 cdcl.neededImportedOnDemandTypes = cdcl1.neededImportedOnDemandTypes ++ cdcl2.neededImportedOnDemandTypes;
}

aspect production class_member_empty
cdcl::Class_Member_Dcl ::= {
 cdcl.neededImportedOnDemandTypes = [];
}

aspect production inner_class
cdcl::Class_Member_Dcl ::= cd::Class_Dcl {
 cdcl.neededImportedOnDemandTypes = cd.neededImportedOnDemandTypes;
}

aspect production inner_interface
cdcl::Class_Member_Dcl ::= id::Interface_Dcl {
 cdcl.neededImportedOnDemandTypes = id.neededImportedOnDemandTypes;
}

aspect production class_block
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.neededImportedOnDemandTypes = b.neededImportedOnDemandTypes;
}

aspect production class_static_initializer
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.neededImportedOnDemandTypes = b.neededImportedOnDemandTypes;
}

aspect production stmt_dcl
stmt::Stmt ::= vdcl::Local_Var_Dcl {
 stmt.neededImportedOnDemandTypes = vdcl.neededImportedOnDemandTypes;
}

aspect production local_var_dcl
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
 vdcl.neededImportedOnDemandTypes = dcltype.neededImportedOnDemandTypes ++ dcls.neededImportedOnDemandTypes;
}

aspect production class_field
cdcl::Class_Member_Dcl ::= f::Field_Dcl {
 cdcl.neededImportedOnDemandTypes = f.neededImportedOnDemandTypes;
}

aspect production field_dcl
f::Field_Dcl ::= mods::Modifiers t::Type dcls::Var_Declarators {
 f.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ dcls.neededImportedOnDemandTypes;
}

aspect production var_declarator
vdcl::Var_Declarator ::= v::Var_Declarator_Id {
 vdcl.neededImportedOnDemandTypes = v.neededImportedOnDemandTypes;
}

aspect production var_declarator_init
vdcl::Var_Declarator ::= v::Var_Declarator_Id init::Var_Init {
 vdcl.neededImportedOnDemandTypes = v.neededImportedOnDemandTypes ++ init.neededImportedOnDemandTypes;
}

aspect production var_declarator_id
vdcl::Var_Declarator_Id ::= id::Id_t {
 vdcl.neededImportedOnDemandTypes = [];
}

aspect production var_init_expr
init::Var_Init ::= e::Expr {
 init.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes;
}

aspect production local_var_dcl_seq
vdcl::Local_Var_Dcl ::= vdcl1::Local_Var_Dcl vdcl2::Local_Var_Dcl {
 vdcl.neededImportedOnDemandTypes = vdcl1.neededImportedOnDemandTypes ++ vdcl2.neededImportedOnDemandTypes;
}

aspect production local_var_dcl_one
vdcl::Local_Var_Dcl ::= dcltype::Type dcl::Var_Declarator {
 vdcl.neededImportedOnDemandTypes = dcltype.neededImportedOnDemandTypes ++ dcl.neededImportedOnDemandTypes;
}

aspect production field_dcl_seq
fdcl::Field_Dcl ::= fdcl1::Field_Dcl fdcl2::Field_Dcl {
 fdcl.neededImportedOnDemandTypes = fdcl1.neededImportedOnDemandTypes ++ fdcl2.neededImportedOnDemandTypes;
}

aspect production field_dcl_one
f::Field_Dcl ::= mods::Modifiers t::Type dcl::Var_Declarator {
 f.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ dcl.neededImportedOnDemandTypes;
}

aspect production var_declarators_one
vdcls::Var_Declarators ::= vdcl::Var_Declarator {
 vdcls.neededImportedOnDemandTypes = vdcl.neededImportedOnDemandTypes;
}

aspect production var_declarators_snoc
vdcls::Var_Declarators ::= vdcls1::Var_Declarators vdcl::Var_Declarator {
 vdcls.neededImportedOnDemandTypes = vdcls1.neededImportedOnDemandTypes ++ vdcl.neededImportedOnDemandTypes;
}

aspect production local_var_dcl_final
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
 vdcl.neededImportedOnDemandTypes = dcltype.neededImportedOnDemandTypes ++ dcls.neededImportedOnDemandTypes;
}

aspect production var_declarator_array
vdcl::Var_Declarator_Id ::= v::Var_Declarator_Id {
 vdcl.neededImportedOnDemandTypes = v.neededImportedOnDemandTypes;
}

aspect production var_init_array
init::Var_Init ::= e::Array_Init {
 init.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes;
}

aspect production array_init
ai::Array_Init ::= vs::Var_Inits {
 ai.neededImportedOnDemandTypes = vs.neededImportedOnDemandTypes;
}

aspect production array_init_no_comma
ai::Array_Init ::= vs::Var_Inits {
 ai.neededImportedOnDemandTypes = vs.neededImportedOnDemandTypes;
}

aspect production array_init_no_var_inits
ai::Array_Init ::= {
 ai.neededImportedOnDemandTypes = [];
}

aspect production array_init_empty
ai::Array_Init ::= {
 ai.neededImportedOnDemandTypes = [];
}

aspect production var_inits_one
vis::Var_Inits ::= vi::Var_Init {
 vis.neededImportedOnDemandTypes = vi.neededImportedOnDemandTypes;
}

aspect production var_inits_snoc
vis::Var_Inits ::= vis1::Var_Inits vi::Var_Init {
 vis.neededImportedOnDemandTypes = vis1.neededImportedOnDemandTypes ++ vi.neededImportedOnDemandTypes;
}

aspect production class_method
cdcl::Class_Member_Dcl ::= mdcl::Method_Dcl {
 cdcl.neededImportedOnDemandTypes = mdcl.neededImportedOnDemandTypes;
}

aspect production method_dcl_prod
mdcl::Method_Dcl ::=  mh::Method_Header b::Block {
 mdcl.neededImportedOnDemandTypes = mh.neededImportedOnDemandTypes ++ b.neededImportedOnDemandTypes;
}

aspect production method_dcl_no_body
mdcl::Method_Dcl ::= mh::Method_Header {
 mdcl.neededImportedOnDemandTypes = mh.neededImportedOnDemandTypes;
}

aspect production method_header_prod
mh::Method_Header ::= mods::Modifiers return_ty::Type mname::Id_t fps::Formal_Params th::Throws {
 mh.neededImportedOnDemandTypes = return_ty.neededImportedOnDemandTypes ++ fps.neededImportedOnDemandTypes ++ th.neededImportedOnDemandTypes;
}

aspect production void_type
t::Type ::= {
 t.neededImportedOnDemandTypes = [];
}

aspect production formal_params_none
fps::Formal_Params ::= {
 fps.neededImportedOnDemandTypes = [];
}

aspect production formal_params_one
fps::Formal_Params ::= fp::Formal_Param {
 fps.neededImportedOnDemandTypes = fp.neededImportedOnDemandTypes;
}

aspect production formal_params_snoc
fps::Formal_Params ::= fps1::Formal_Params fp::Formal_Param {
 fps.neededImportedOnDemandTypes = fps1.neededImportedOnDemandTypes ++ fp.neededImportedOnDemandTypes;
}

aspect production formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
 fp.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ vid.neededImportedOnDemandTypes;
}

aspect production return_statement
s::Stmt ::= t::Return_t {
 s.neededImportedOnDemandTypes = [];
}

aspect production return_expr
s::Stmt ::= t::Return_t e::Expr {
 s.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes;
}

aspect production method_header_declarator
mh::Method_Header ::= mods::Modifiers t::Type md::Method_Declarator th::Throws {
 mh.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ md.neededImportedOnDemandTypes ++ th.neededImportedOnDemandTypes;
}

aspect production method_declarator
md::Method_Declarator ::= id::Id_t fps::Formal_Params {
 md.neededImportedOnDemandTypes = fps.neededImportedOnDemandTypes;
}

aspect production method_declarator_array
md::Method_Declarator ::= md1::Method_Declarator {
 md.neededImportedOnDemandTypes = md1.neededImportedOnDemandTypes;
}

aspect production final_formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
 fp.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ vid.neededImportedOnDemandTypes;
}

aspect production throws_none
th::Throws ::= {
 th.neededImportedOnDemandTypes = [];
}

aspect production throws
th::Throws ::= ctl::TypeNames {
 th.neededImportedOnDemandTypes = ctl.neededImportedOnDemandTypes;
}

aspect production class_constructor
cdcl::Class_Member_Dcl ::= mods::Modifiers id::Id_t fps::Formal_Params thr::Throws cb::Block {
 cdcl.neededImportedOnDemandTypes = fps.neededImportedOnDemandTypes ++ thr.neededImportedOnDemandTypes ++ cb.neededImportedOnDemandTypes;
}

aspect production stmt_constructor_invocation
stmt::Stmt ::= inv::Constructor_Invocation {
 stmt.neededImportedOnDemandTypes = inv.neededImportedOnDemandTypes;
}

aspect production this_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
 inv.neededImportedOnDemandTypes = args.neededImportedOnDemandTypes;
}

aspect production super_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
 inv.neededImportedOnDemandTypes = args.neededImportedOnDemandTypes;
}

aspect production this_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
 inv.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes ++ args.neededImportedOnDemandTypes;
}

aspect production this_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
 inv.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ args.neededImportedOnDemandTypes;
}

aspect production super_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
 inv.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes ++ args.neededImportedOnDemandTypes;
}

aspect production super_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
 inv.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ args.neededImportedOnDemandTypes;
}

aspect production primitive_type
t::Type ::= t1::Primitive_Type {
 t.neededImportedOnDemandTypes = [];
}

aspect production reference_type
t::Type ::= t1::Reference_Type {
 t.neededImportedOnDemandTypes = t1.neededImportedOnDemandTypes;
}

aspect production type_typerep
t::Type ::= tr::TypeRep {
 t.neededImportedOnDemandTypes = [];
}

aspect production name_type
t::Reference_Type ::= n::TypeName {
 t.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes;
}

aspect production type_names_none
ns::TypeNames ::= {
 ns.neededImportedOnDemandTypes = [];
}

aspect production type_names_one
ns::TypeNames ::= n::TypeName {
 ns.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes;
}

aspect production type_names_snoc
ns::TypeNames ::= ns1::TypeNames n::TypeName {
 ns.neededImportedOnDemandTypes = ns1.neededImportedOnDemandTypes ++ n.neededImportedOnDemandTypes;
}

aspect production type_interface_dcl
td::Type_Dcl ::= idcl::Interface_Dcl {
 td.neededImportedOnDemandTypes = idcl.neededImportedOnDemandTypes;
}

aspect production interface_member_dcls_none
idcls::Interface_Member_Dcls ::= {
 idcls.neededImportedOnDemandTypes = [];
}

aspect production interface_member_dcls_snoc
idcls::Interface_Member_Dcls ::= idcls1::Interface_Member_Dcls idcl::Interface_Member_Dcl {
 idcls.neededImportedOnDemandTypes = idcls1.neededImportedOnDemandTypes ++ idcl.neededImportedOnDemandTypes;
}

aspect production interface_field
idcl::Interface_Member_Dcl ::= fd::Field_Dcl {
 idcl.neededImportedOnDemandTypes = fd.neededImportedOnDemandTypes;
}

aspect production interface_method
idcl::Interface_Member_Dcl ::= mh::Method_Header {
 idcl.neededImportedOnDemandTypes = mh.neededImportedOnDemandTypes;
}

aspect production interface_empty
idcl::Interface_Member_Dcl ::= {
 idcl.neededImportedOnDemandTypes = [];
}

aspect production interface_dcl
idcl::Interface_Dcl ::= mods::Modifiers iname::Id_t inters::TypeNames dcls::Interface_Member_Dcls {
 idcl.neededImportedOnDemandTypes = inters.neededImportedOnDemandTypes ++ dcls.neededImportedOnDemandTypes;
}

aspect production interface_inner_class
idcl::Interface_Member_Dcl ::= cd::Class_Dcl {
 idcl.neededImportedOnDemandTypes = cd.neededImportedOnDemandTypes;
}

aspect production interface_inner_interface
idcl::Interface_Member_Dcl ::= id::Interface_Dcl {
 idcl.neededImportedOnDemandTypes = id.neededImportedOnDemandTypes;
}

aspect production cast_name_array
e::Expr ::= t::TypeName dims::Integer e1::Expr {
 e.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes ++ e1.neededImportedOnDemandTypes;
}

aspect production cast_primitive_array
e::Expr ::= t::Primitive_Type dims::Integer e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production not
e::Expr ::= e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production or_or
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production and_and
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production super_field_access
lhs::LHS ::= id::Id_t {
 lhs.neededImportedOnDemandTypes = [];
}

aspect production name_super_field_access
lhs::LHS ::= n::TypeName id::Id_t {
 lhs.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes;
}

aspect production super_method_call
e::Stmt_Expr ::= id::Id_t es::Exprs {
 e.neededImportedOnDemandTypes = es.neededImportedOnDemandTypes;
}

aspect production name_super_method_call
e::Stmt_Expr ::= n::TypeName id::Id_t es::Exprs {
 e.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes ++ es.neededImportedOnDemandTypes;
}

aspect production array_access
lhs::LHS ::= n::ExprName e1::Expr {
 lhs.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes ++ e1.neededImportedOnDemandTypes;
}

aspect production array_access_general
lhs::LHS ::= e1::Expr e2::Expr {
 lhs.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production mul_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production div_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production mod_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production plus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production minus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production lshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production rshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production urshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production and_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production xor_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production or_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ expr.neededImportedOnDemandTypes;
}

aspect production new_class_expr
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs {
 e.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes ++ es.neededImportedOnDemandTypes;
}

aspect production new_class_expr_body
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs cb::Class_Body {
 e.neededImportedOnDemandTypes = expr.neededImportedOnDemandTypes ++ es.neededImportedOnDemandTypes ++ cb.neededImportedOnDemandTypes;
}

aspect production new_class_name
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs {
 e.neededImportedOnDemandTypes = nam.neededImportedOnDemandTypes ++ es.neededImportedOnDemandTypes;
}

aspect production new_class_name_body
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs cb::Class_Body {
 e.neededImportedOnDemandTypes = nam.neededImportedOnDemandTypes ++ es.neededImportedOnDemandTypes ++ cb.neededImportedOnDemandTypes;
}

aspect production conditional
e::Expr ::= cond::Expr thenexpr::Expr elseexpr::Expr {
 e.neededImportedOnDemandTypes = cond.neededImportedOnDemandTypes ++ thenexpr.neededImportedOnDemandTypes ++ elseexpr.neededImportedOnDemandTypes;
}

aspect production or
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production xor
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production and
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production eq
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production not_eq
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production lt
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production gt
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production lteq
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production gteq
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production instanceof
e::Expr ::= e1::Expr t1::Reference_Type {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ t1.neededImportedOnDemandTypes;
}

aspect production lshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production rshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production urshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production div
e::Expr ::= e1::Expr _ e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production mod
e::Expr ::= e1::Expr _ e2::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes ++ e2.neededImportedOnDemandTypes;
}

aspect production unary_plus
e::Expr ::= e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production unary_minus
e::Expr ::= e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production pre_inc
e::Stmt_Expr ::= e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production pre_dec
e::Stmt_Expr ::= e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production comp
e::Expr ::= e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production post_inc
e::Stmt_Expr ::= e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production post_dec
e::Stmt_Expr ::= e1::Expr {
 e.neededImportedOnDemandTypes = e1.neededImportedOnDemandTypes;
}

aspect production new_array_init_primitive
e::Expr ::= t1::Primitive_Type dims::Integer ai::Array_Init {
 e.neededImportedOnDemandTypes = ai.neededImportedOnDemandTypes;
}

aspect production new_array_init_name
e::Expr ::= n::TypeName dims::Integer ai::Array_Init {
 e.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes ++ ai.neededImportedOnDemandTypes;
}

aspect production new_array_no_init_primitive
e::Expr ::= t1::Primitive_Type d1::Dim_Exprs dims::Integer {
 e.neededImportedOnDemandTypes = d1.neededImportedOnDemandTypes;
}

aspect production new_array_no_init_name
e::Expr ::= n::TypeName d1::Dim_Exprs dims::Integer {
 e.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes ++ d1.neededImportedOnDemandTypes;
}

aspect production primitive_dot_class
e::Expr ::= t::Primitive_Type {
 e.neededImportedOnDemandTypes = [];
}

aspect production void_dot_class
e::Expr ::= {
 e.neededImportedOnDemandTypes = [];
}

aspect production array_dot_class
e::Expr ::= t::Array_Type {
 e.neededImportedOnDemandTypes = t.neededImportedOnDemandTypes;
}

aspect production name_dot_class
e::Expr ::= n::TypeName {
 e.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes;
}

aspect production name_dot_this
e::Expr ::= n::TypeName {
 e.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes;
}

aspect production dim_exprs_one
d::Dim_Exprs ::= e::Expr {
 d.neededImportedOnDemandTypes = e.neededImportedOnDemandTypes;
}

aspect production dim_exprs_snoc
d::Dim_Exprs ::= d1::Dim_Exprs e::Expr {
 d.neededImportedOnDemandTypes = d1.neededImportedOnDemandTypes ++ e.neededImportedOnDemandTypes;
}

aspect production array_type
t::Reference_Type ::= t1::Array_Type {
 t.neededImportedOnDemandTypes = t1.neededImportedOnDemandTypes;
}

aspect production primitive_array
t::Array_Type ::= t1::Primitive_Type ds::Integer {
 t.neededImportedOnDemandTypes = [];
}

aspect production name_array
t::Array_Type ::= n::TypeName ds::Integer {
 t.neededImportedOnDemandTypes = n.neededImportedOnDemandTypes;
}

aspect production simple_package_name
pn::PackageName ::= id::Id_t {
 pn.neededImportedOnDemandTypes = [];
}

aspect production qualified_package_name
pn::PackageName ::= pn2::PackageName id::Id_t {
 pn.neededImportedOnDemandTypes = pn2.neededImportedOnDemandTypes;
}

aspect production simple_package_or_type_name 
ptn::PackageOrTypeName ::= id::Id_t {
}

aspect production qualified_package_or_type_name
ptn::PackageOrTypeName ::= pn::PackageOrTypeName id::Id_t {
 ptn.neededImportedOnDemandTypes = pn.neededImportedOnDemandTypes;
}

aspect production simple_type_name
tn::TypeName ::= id::Id_t {
}

aspect production qualified_type_name
tn::TypeName ::= pn::PackageOrTypeName id::Id_t {
 tn.neededImportedOnDemandTypes = pn.neededImportedOnDemandTypes;
}

aspect production simple_expr_name
en::ExprName ::= id::Id_t {
 en.neededImportedOnDemandTypes = [];
}

aspect production qualified_expr_name
en::ExprName ::= an::AmbiguousName id::Id_t {
 en.neededImportedOnDemandTypes = an.neededImportedOnDemandTypes;
}

aspect production simple_method_name
mn::MethodName ::= id::Id_t  { 
 mn.neededImportedOnDemandTypes = [];
}

aspect production qualified_method_name
mn::MethodName ::= an::AmbiguousName id::Id_t {
 mn.neededImportedOnDemandTypes = an.neededImportedOnDemandTypes;
}

aspect production simple_ambiguous_name
an::AmbiguousName ::= id::Id_t {
}

aspect production qualified_ambiguous_name
andi::AmbiguousName ::= an::AmbiguousName id::Id_t {
 andi.neededImportedOnDemandTypes = an.neededImportedOnDemandTypes;
}

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

aspect production type_dcls_none
tdcls::Type_Dcls ::= {
  tdcls.localTypes = [];
}

aspect production type_dcl_empty
tdcl::Type_Dcl ::= {
  tdcl.localTypes = [];
}

aspect production class_dcl_none
cdcl::Class_Dcl ::= {
 cdcl.localTypes = [];
}

aspect production class_member_dcls_none
cdcls::Class_Member_Dcls ::= {
 cdcls.localTypes = [];
}

aspect production class_member_empty
cdcl::Class_Member_Dcl ::= {
 cdcl.localTypes = [];
}

aspect production class_block
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.localTypes = [];
}

aspect production class_static_initializer
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.localTypes = [];
}

aspect production class_field
cdcl::Class_Member_Dcl ::= f::Field_Dcl {
 cdcl.localTypes = [];
}

aspect production class_method
cdcl::Class_Member_Dcl ::= mdcl::Method_Dcl {
 cdcl.localTypes = [];
}

aspect production class_constructor
cdcl::Class_Member_Dcl ::= mods::Modifiers id::Id_t fps::Formal_Params thr::Throws cb::Block {
 cdcl.localTypes = [];
}

aspect production interface_member_dcls_none
idcls::Interface_Member_Dcls ::= {
 idcls.localTypes = [];
}

aspect production interface_field
idcl::Interface_Member_Dcl ::= fd::Field_Dcl {
 idcl.localTypes = [];
}

aspect production interface_method
idcl::Interface_Member_Dcl ::= mh::Method_Header {
 idcl.localTypes = [];
}

aspect production interface_empty
idcl::Interface_Member_Dcl ::= {
 idcl.localTypes = [];
}

aspect production stmt_block
stmt::Stmt ::= b::Block {
 stmt.localTypes = [];
}

aspect production empty_stmt
s::Stmt ::= {
 s.localTypes = [];
}

aspect production erroneous_Stmt
s::Stmt ::= err_tree::Stmt errs::[Error] {
 s.localTypes = [];
}

aspect production stmt_stmt_expr
s::Stmt ::= s1::Stmt_Expr {
 s.localTypes = [];
}

aspect production synchronized
s::Stmt ::= e::Expr b::Block {
 s.localTypes = [];
}

aspect production throw
s::Stmt ::= e::Expr {
 s.localTypes = [];
}

aspect production try
s::Stmt ::= b::Block c::Catches {
 s.localTypes = [];
}

aspect production try_finally
s::Stmt ::= b::Block c::Catches b1::Block {
 s.localTypes = [];
}

aspect production assert
s::Stmt ::= expr::Expr {
 s.localTypes = [];
}

aspect production assert_colon
s::Stmt ::= expr1::Expr expr2::Expr {
 s.localTypes = [];
}

aspect production while_prod
while::Stmt ::= t::While_t cond::Expr body::Stmt {
 while.localTypes = [];
}

aspect production do
dowhile::Stmt ::= body::Stmt t::While_t cond::Expr {
 dowhile.localTypes = [];
}

aspect production label_prod
lstmt::Stmt ::= id::Id_t stmt::Stmt {
 lstmt.localTypes = [];
}

aspect production break_prod
break::Stmt ::= {
 break.localTypes = [];
}

aspect production break_label
break::Stmt ::= id::Id_t {
 break.localTypes = [];
}

aspect production continue_prod
continue::Stmt ::= {
 continue.localTypes = [];
}

aspect production continue_label
continue::Stmt ::= id::Id_t {
 continue.localTypes = [];
}

aspect production if_then
ifthen::Stmt ::= t::If_t expr::Expr thenbody::Stmt {
 ifthen.localTypes = [];
}

aspect production if_then_else
ifthenelse::Stmt ::= t::If_t expr::Expr thenbody::Stmt elsebody::Stmt {
 ifthenelse.localTypes = [];
}

aspect production for
forstmt::Stmt ::= init::For_Init test::For_Test update::For_Update body::Stmt {
 forstmt.localTypes = [];
}

aspect production switch_prod
switch::Stmt ::= expr::Expr switchblock::Switch_Block {
 switch.localTypes = [];
}

aspect production return_statement
s::Stmt ::= t::Return_t {
 s.localTypes = [];
}

aspect production return_expr
s::Stmt ::= t::Return_t e::Expr {
 s.localTypes = [];
}

aspect production stmt_constructor_invocation
stmt::Stmt ::= inv::Constructor_Invocation {
 stmt.localTypes = [];
}

aspect production stmt_dcl
stmt::Stmt ::= vdcl::Local_Var_Dcl {
 stmt.localTypes = [];
}

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

aspect production package_dcl
p::Package_Dcl ::= n::PackageName {
}

aspect production package_dcl_none
p::Package_Dcl ::= {
}

aspect production import_dcls_none
idcls::Import_Dcls ::= {
}

aspect production import_dcls_snoc
idcls::Import_Dcls ::= idcls1::Import_Dcls idcl::Import_Dcl {
}

aspect production import_dcl
idcl::Import_Dcl ::= t::Import_t n::TypeName {
}

aspect production import_dcl_on_demand
idcl::Import_Dcl ::= n::PackageOrTypeName {
}

aspect production type_dcls_none
tdcls::Type_Dcls ::= {
 tdcls.neededFullyQualifiedTypes =  [];
}

aspect production type_dcls_snoc
tdcls::Type_Dcls ::= tdcls1::Type_Dcls tdcl::Type_Dcl {
 tdcls.neededFullyQualifiedTypes =  tdcls1.neededFullyQualifiedTypes ++ tdcl.neededFullyQualifiedTypes;
}

aspect production type_dcl_empty
tdcl::Type_Dcl ::= {
 tdcl.neededFullyQualifiedTypes = [];
}

aspect production lhs_name
lhs::LHS ::= nm::ExprName {
 lhs.neededFullyQualifiedTypes = nm.neededFullyQualifiedTypes;
}

aspect production expr_lhs
expr::Expr ::= lhs::LHS {
 expr.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes;
}

aspect production expr_stmt_expr
e::Expr ::= se::Stmt_Expr {
 e.neededFullyQualifiedTypes = se.neededFullyQualifiedTypes;
}

aspect production this
e::Expr ::= {
 e.neededFullyQualifiedTypes = [];
}

aspect production expr_field_access
lhs::LHS ::= e1::Expr id::Id_t {
 lhs.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production method_call
e::Stmt_Expr ::= name1::MethodName args::Exprs {
 e.neededFullyQualifiedTypes = name1.neededFullyQualifiedTypes ++ args.neededFullyQualifiedTypes;
}

aspect production expr_method_call
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs {
 e.neededFullyQualifiedTypes = obj.neededFullyQualifiedTypes ++ params.neededFullyQualifiedTypes;
}

aspect production resolved_method_call_copy
e::Stmt_Expr ::= obj::Expr id::Id_t params::Exprs rettr::TypeRep {
 e.neededFullyQualifiedTypes = obj.neededFullyQualifiedTypes ++ params.neededFullyQualifiedTypes;
}

aspect production new_class
e::Stmt_Expr ::= t::TypeName params::Exprs {
 e.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ params.neededFullyQualifiedTypes;
}

aspect production new_class_body
e::Stmt_Expr ::= t::TypeName params::Exprs cb::Class_Body {
 e.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ params.neededFullyQualifiedTypes ++ cb.neededFullyQualifiedTypes;
}

aspect production resolved_new_class
e::Stmt_Expr ::= tr::TypeRep params::Exprs {
 e.neededFullyQualifiedTypes = params.neededFullyQualifiedTypes;
}

aspect production resolved_new_class_body
e::Stmt_Expr ::= tr::TypeRep params::Exprs cb::Class_Body {
 e.neededFullyQualifiedTypes = params.neededFullyQualifiedTypes ++ cb.neededFullyQualifiedTypes;
}

aspect production cast_prod
e::Expr ::= name1::TypeName e1::Expr {
 e.neededFullyQualifiedTypes = name1.neededFullyQualifiedTypes ++ e1.neededFullyQualifiedTypes;
}

aspect production resolved_cast
e::Expr ::= t::TypeRep e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production assign
a::Stmt_Expr ::= lhs::LHS t::Eq_t expr::Expr {
 a.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production plus
e::Expr ::= e1::Expr t::Plus_t e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production minus
e::Expr ::= e1::Expr t::Minus_t e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production mul
e::Expr ::= e1::Expr t::Mul_t e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production byte_const
e::Expr ::= t::String {
 e.neededFullyQualifiedTypes = [];
}

aspect production short_const
e::Expr ::= t::String {
 e.neededFullyQualifiedTypes = [];
}

aspect production char_const
e::Expr ::= t::String {
 e.neededFullyQualifiedTypes = [];
}

aspect production int_const
e::Expr ::= t::String {
 e.neededFullyQualifiedTypes = [];
}

aspect production long_const
e::Expr ::= t::String {
 e.neededFullyQualifiedTypes = [];
}

aspect production float_const
e::Expr ::= t::String {
 e.neededFullyQualifiedTypes = [];
}

aspect production double_const
e::Expr ::= t::String {
 e.neededFullyQualifiedTypes = [];
}

aspect production true_const
e::Expr ::= {
 e.neededFullyQualifiedTypes = [];
}

aspect production false_const
e::Expr ::= {
 e.neededFullyQualifiedTypes = [];
}

aspect production string_const
e::Expr ::= t::String {
 e.neededFullyQualifiedTypes = [];
}

aspect production null_const
e::Expr ::= {
 e.neededFullyQualifiedTypes = [];
}

aspect production exprs_none
es::Exprs ::= {
 es.neededFullyQualifiedTypes = [];
}

aspect production exprs_one
es::Exprs ::= e::Expr {
 es.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes;
}

aspect production exprs_snoc
es::Exprs ::= es1::Exprs e::Expr {
 es.neededFullyQualifiedTypes = es1.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes;
}

aspect production exprs_cons
es::Exprs ::= e::Expr es1::Exprs {
 es.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes ++ es1.neededFullyQualifiedTypes;
}

aspect production cast_primitive
e::Expr ::= t::Primitive_Type e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production cast_simple
e::Expr ::= t::Expr e1::Expr {
 e.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ e1.neededFullyQualifiedTypes;
}

aspect production block
b::Block ::= stmt::Stmt {
 b.neededFullyQualifiedTypes = stmt.neededFullyQualifiedTypes;
}

aspect production empty_block
b::Block ::= {
 b.neededFullyQualifiedTypes = [];
}

aspect production stmt_seq
seq::Stmt ::= stmt1::Stmt stmt2::Stmt {
 seq.neededFullyQualifiedTypes = stmt1.neededFullyQualifiedTypes ++ stmt2.neededFullyQualifiedTypes;
}

aspect production stmt_block
stmt::Stmt ::= b::Block {
 stmt.neededFullyQualifiedTypes = b.neededFullyQualifiedTypes;
}

aspect production empty_stmt
s::Stmt ::= {
 s.neededFullyQualifiedTypes = [];
}

aspect production erroneous_Stmt
s::Stmt ::= err_tree::Stmt errs::[Error] {
 s.neededFullyQualifiedTypes = [];
}

aspect production stmt_stmt_expr
s::Stmt ::= s1::Stmt_Expr {
 s.neededFullyQualifiedTypes = s1.neededFullyQualifiedTypes;
}

aspect production block_stmt_class
s::Stmt ::= cdcl::Class_Dcl {
 s.neededFullyQualifiedTypes = cdcl.neededFullyQualifiedTypes;
}

aspect production block_stmt_interface
s::Stmt ::= idcl::Interface_Dcl {
 s.neededFullyQualifiedTypes = idcl.neededFullyQualifiedTypes;
}

aspect production synchronized
s::Stmt ::= e::Expr b::Block {
 s.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes ++ b.neededFullyQualifiedTypes;
}

aspect production throw
s::Stmt ::= e::Expr {
 s.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes;
}

aspect production try
s::Stmt ::= b::Block c::Catches {
 s.neededFullyQualifiedTypes = b.neededFullyQualifiedTypes ++ c.neededFullyQualifiedTypes;
}

aspect production try_finally
s::Stmt ::= b::Block c::Catches b1::Block {
 s.neededFullyQualifiedTypes = b.neededFullyQualifiedTypes ++ c.neededFullyQualifiedTypes ++ b1.neededFullyQualifiedTypes;
}

aspect production assert
s::Stmt ::= expr::Expr {
 s.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes;
}

aspect production assert_colon
s::Stmt ::= expr1::Expr expr2::Expr {
 s.neededFullyQualifiedTypes = expr1.neededFullyQualifiedTypes ++ expr2.neededFullyQualifiedTypes;
}

aspect production catches_none
c::Catches ::= {
 c.neededFullyQualifiedTypes = [];
}

aspect production catches_snoc
c::Catches ::= cs::Catches cc::Catch {
 c.neededFullyQualifiedTypes = cs.neededFullyQualifiedTypes ++ cc.neededFullyQualifiedTypes;
}

aspect production catch
c::Catch ::= fp::Formal_Param b::Block {
 c.neededFullyQualifiedTypes = fp.neededFullyQualifiedTypes ++ b.neededFullyQualifiedTypes;
}

aspect production while_prod
while::Stmt ::= t::While_t cond::Expr body::Stmt {
 while.neededFullyQualifiedTypes = cond.neededFullyQualifiedTypes ++ body.neededFullyQualifiedTypes;
}

aspect production do
dowhile::Stmt ::= body::Stmt t::While_t cond::Expr {
 dowhile.neededFullyQualifiedTypes = body.neededFullyQualifiedTypes ++ cond.neededFullyQualifiedTypes;
}

aspect production label_prod
lstmt::Stmt ::= id::Id_t stmt::Stmt {
 lstmt.neededFullyQualifiedTypes = stmt.neededFullyQualifiedTypes;
}

aspect production break_prod
break::Stmt ::= {
 break.neededFullyQualifiedTypes = [];
}

aspect production break_label
break::Stmt ::= id::Id_t {
 break.neededFullyQualifiedTypes = [];
}

aspect production continue_prod
continue::Stmt ::= {
 continue.neededFullyQualifiedTypes = [];
}

aspect production continue_label
continue::Stmt ::= id::Id_t {
 continue.neededFullyQualifiedTypes = [];
}

aspect production if_then
ifthen::Stmt ::= t::If_t expr::Expr thenbody::Stmt {
 ifthen.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes ++ thenbody.neededFullyQualifiedTypes;
}

aspect production if_then_else
ifthenelse::Stmt ::= t::If_t expr::Expr thenbody::Stmt elsebody::Stmt {
 ifthenelse.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes ++ thenbody.neededFullyQualifiedTypes ++ elsebody.neededFullyQualifiedTypes;
}

aspect production for
forstmt::Stmt ::= init::For_Init test::For_Test update::For_Update body::Stmt {
 forstmt.neededFullyQualifiedTypes = init.neededFullyQualifiedTypes ++ test.neededFullyQualifiedTypes ++ update.neededFullyQualifiedTypes ++ body.neededFullyQualifiedTypes;
}

aspect production for_init_empty
forinit::For_Init ::= {
 forinit.neededFullyQualifiedTypes = [];
}

aspect production for_init_some
forinit::For_Init ::= ses::Stmt_Exprs {
 forinit.neededFullyQualifiedTypes = ses.neededFullyQualifiedTypes;
}

aspect production for_init_dcl
forinit::For_Init ::= dcl::Local_Var_Dcl {
 forinit.neededFullyQualifiedTypes = dcl.neededFullyQualifiedTypes;
}

aspect production for_test_none
fortest::For_Test ::= {
 fortest.neededFullyQualifiedTypes = [];
}

aspect production for_test_one
fortest::For_Test ::= e::Expr {
 fortest.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes;
}

aspect production for_update_empty
forupdate::For_Update ::= {
 forupdate.neededFullyQualifiedTypes = [];
}

aspect production for_update_some
forupdate::For_Update ::= ses::Stmt_Exprs {
 forupdate.neededFullyQualifiedTypes = ses.neededFullyQualifiedTypes;
}

aspect production stmt_exprs_one
ses::Stmt_Exprs ::= se::Stmt_Expr {
 ses.neededFullyQualifiedTypes = se.neededFullyQualifiedTypes;
}

aspect production stmt_exprs_snoc
ses::Stmt_Exprs ::= ses1::Stmt_Exprs se::Stmt_Expr {
 ses.neededFullyQualifiedTypes = ses1.neededFullyQualifiedTypes ++ se.neededFullyQualifiedTypes;
}

aspect production switch_prod
switch::Stmt ::= expr::Expr switchblock::Switch_Block {
 switch.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes ++ switchblock.neededFullyQualifiedTypes;
}

aspect production switch_block
switchblock::Switch_Block ::= gs::Switch_Groups ls::Switch_Labels {
 switchblock.neededFullyQualifiedTypes = gs.neededFullyQualifiedTypes ++ ls.neededFullyQualifiedTypes;
}

aspect production switch_block_no_labels
switchblock::Switch_Block ::= s::Switch_Groups {
 switchblock.neededFullyQualifiedTypes = s.neededFullyQualifiedTypes;
}

aspect production switch_block_no_groups
switchblock::Switch_Block ::= s::Switch_Labels {
 switchblock.neededFullyQualifiedTypes = s.neededFullyQualifiedTypes;
}

aspect production switch_block_empty
switchblock::Switch_Block ::= {
 switchblock.neededFullyQualifiedTypes = [];
}

aspect production switch_groups_one
switch::Switch_Groups ::= item::Switch_Group {
 switch.neededFullyQualifiedTypes = item.neededFullyQualifiedTypes;
}

aspect production switch_groups_snoc
switch::Switch_Groups ::= list::Switch_Groups item::Switch_Group {
 switch.neededFullyQualifiedTypes = list.neededFullyQualifiedTypes ++ item.neededFullyQualifiedTypes;
}

aspect production switch_group
switch::Switch_Group ::= label_list::Switch_Labels stmts::Stmt {
 switch.neededFullyQualifiedTypes = label_list.neededFullyQualifiedTypes ++ stmts.neededFullyQualifiedTypes;
}

aspect production switch_labels_one
switchlabels::Switch_Labels ::= switchlabel::Switch_Label {
 switchlabels.neededFullyQualifiedTypes = switchlabel.neededFullyQualifiedTypes;
}

aspect production switch_labels_snoc
switchlabels::Switch_Labels ::=  list::Switch_Labels item::Switch_Label {
 switchlabels.neededFullyQualifiedTypes = list.neededFullyQualifiedTypes ++ item.neededFullyQualifiedTypes;
}

aspect production switch_label
label::Switch_Label ::= expr::Expr {
 label.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes;
}

aspect production switch_label_default
label::Switch_Label ::= {
 label.neededFullyQualifiedTypes = [];
}

aspect production type_class_dcl
td::Type_Dcl ::= cdcl::Class_Dcl {
 td.neededFullyQualifiedTypes = cdcl.neededFullyQualifiedTypes;
}

aspect production class_dcl_seq
cdcl::Class_Dcl ::= cdcl1::Class_Dcl cdcl2::Class_Dcl {
 cdcl.neededFullyQualifiedTypes = cdcl1.neededFullyQualifiedTypes ++ cdcl2.neededFullyQualifiedTypes;
}

aspect production class_dcl_none
cdcl::Class_Dcl ::= {
 cdcl.neededFullyQualifiedTypes = [];
}

aspect production class_dcl
cdcl::Class_Dcl ::= mods::Modifiers cname::Id_t parent::TypeName inters::TypeNames cb::Class_Body {
 cdcl.neededFullyQualifiedTypes = parent.neededFullyQualifiedTypes ++ inters.neededFullyQualifiedTypes ++ cb.neededFullyQualifiedTypes ;
}

aspect production class_body
cb::Class_Body ::= dcls::Class_Member_Dcls {
 cb.neededFullyQualifiedTypes = dcls.neededFullyQualifiedTypes;
}

aspect production class_member_dcls_none
cdcls::Class_Member_Dcls ::= {
 cdcls.neededFullyQualifiedTypes = [];
}

aspect production class_member_dcls_snoc
cdcls::Class_Member_Dcls ::= cdcls1::Class_Member_Dcls cdcl::Class_Member_Dcl {
 cdcls.neededFullyQualifiedTypes = cdcls1.neededFullyQualifiedTypes ++ cdcl.neededFullyQualifiedTypes;
}

aspect production class_member_dcls_one
cdcls::Class_Member_Dcls ::= cdcl::Class_Member_Dcl {
 cdcls.neededFullyQualifiedTypes = cdcl.neededFullyQualifiedTypes;
}

aspect production class_member_dcl_seq
cdcl::Class_Member_Dcl ::= cdcl1::Class_Member_Dcl cdcl2::Class_Member_Dcl {
 cdcl.neededFullyQualifiedTypes = cdcl1.neededFullyQualifiedTypes ++ cdcl2.neededFullyQualifiedTypes;
}

aspect production class_member_empty
cdcl::Class_Member_Dcl ::= {
 cdcl.neededFullyQualifiedTypes = [];
}

aspect production inner_class
cdcl::Class_Member_Dcl ::= cd::Class_Dcl {
 cdcl.neededFullyQualifiedTypes = cd.neededFullyQualifiedTypes;
}

aspect production inner_interface
cdcl::Class_Member_Dcl ::= id::Interface_Dcl {
 cdcl.neededFullyQualifiedTypes = id.neededFullyQualifiedTypes;
}

aspect production class_block
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.neededFullyQualifiedTypes = b.neededFullyQualifiedTypes;
}

aspect production class_static_initializer
cdcl::Class_Member_Dcl ::= b::Block {
 cdcl.neededFullyQualifiedTypes = b.neededFullyQualifiedTypes;
}

aspect production class_field
cdcl::Class_Member_Dcl ::= f::Field_Dcl {
 cdcl.neededFullyQualifiedTypes = f.neededFullyQualifiedTypes;
}

aspect production class_method
cdcl::Class_Member_Dcl ::= mdcl::Method_Dcl {
 cdcl.neededFullyQualifiedTypes = mdcl.neededFullyQualifiedTypes;
}

aspect production stmt_dcl
stmt::Stmt ::= vdcl::Local_Var_Dcl {
 stmt.neededFullyQualifiedTypes = vdcl.neededFullyQualifiedTypes;
}

aspect production local_var_dcl
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
 vdcl.neededFullyQualifiedTypes = dcltype.neededFullyQualifiedTypes ++ dcls.neededFullyQualifiedTypes;
}

aspect production field_dcl
f::Field_Dcl ::= mods::Modifiers t::Type dcls::Var_Declarators {
 f.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ dcls.neededFullyQualifiedTypes;
}

aspect production var_declarator
vdcl::Var_Declarator ::= v::Var_Declarator_Id {
 vdcl.neededFullyQualifiedTypes = v.neededFullyQualifiedTypes;
}

aspect production var_declarator_init
vdcl::Var_Declarator ::= v::Var_Declarator_Id init::Var_Init {
 vdcl.neededFullyQualifiedTypes = v.neededFullyQualifiedTypes ++ init.neededFullyQualifiedTypes;
}

aspect production var_declarator_id
vdcl::Var_Declarator_Id ::= id::Id_t {
 vdcl.neededFullyQualifiedTypes = [];
}

aspect production var_init_expr
init::Var_Init ::= e::Expr {
 init.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes;
}

aspect production local_var_dcl_seq
vdcl::Local_Var_Dcl ::= vdcl1::Local_Var_Dcl vdcl2::Local_Var_Dcl {
 vdcl.neededFullyQualifiedTypes = vdcl1.neededFullyQualifiedTypes ++ vdcl2.neededFullyQualifiedTypes;
}

aspect production local_var_dcl_one
vdcl::Local_Var_Dcl ::= dcltype::Type dcl::Var_Declarator {
 vdcl.neededFullyQualifiedTypes = dcltype.neededFullyQualifiedTypes ++ dcl.neededFullyQualifiedTypes;
}

aspect production field_dcl_seq
fdcl::Field_Dcl ::= fdcl1::Field_Dcl fdcl2::Field_Dcl {
 fdcl.neededFullyQualifiedTypes = fdcl1.neededFullyQualifiedTypes ++ fdcl2.neededFullyQualifiedTypes;
}

aspect production field_dcl_one
f::Field_Dcl ::= mods::Modifiers t::Type dcl::Var_Declarator {
 f.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ dcl.neededFullyQualifiedTypes;
}

aspect production var_declarators_one
vdcls::Var_Declarators ::= vdcl::Var_Declarator {
 vdcls.neededFullyQualifiedTypes = vdcl.neededFullyQualifiedTypes;
}

aspect production var_declarators_snoc
vdcls::Var_Declarators ::= vdcls1::Var_Declarators vdcl::Var_Declarator {
 vdcls.neededFullyQualifiedTypes = vdcls1.neededFullyQualifiedTypes ++ vdcl.neededFullyQualifiedTypes;
}

aspect production local_var_dcl_final
vdcl::Local_Var_Dcl ::= dcltype::Type dcls::Var_Declarators {
 vdcl.neededFullyQualifiedTypes = dcltype.neededFullyQualifiedTypes ++ dcls.neededFullyQualifiedTypes;
}

aspect production var_declarator_array
vdcl::Var_Declarator_Id ::= v::Var_Declarator_Id {
 vdcl.neededFullyQualifiedTypes = v.neededFullyQualifiedTypes;
}

aspect production var_init_array
init::Var_Init ::= e::Array_Init {
 init.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes;
}

aspect production array_init
ai::Array_Init ::= vs::Var_Inits {
 ai.neededFullyQualifiedTypes = vs.neededFullyQualifiedTypes;
}

aspect production array_init_no_comma
ai::Array_Init ::= vs::Var_Inits {
 ai.neededFullyQualifiedTypes = vs.neededFullyQualifiedTypes;
}

aspect production array_init_no_var_inits
ai::Array_Init ::= {
 ai.neededFullyQualifiedTypes = [];
}

aspect production array_init_empty
ai::Array_Init ::= {
 ai.neededFullyQualifiedTypes = [];
}

aspect production var_inits_one
vis::Var_Inits ::= vi::Var_Init {
 vis.neededFullyQualifiedTypes = vi.neededFullyQualifiedTypes;
}

aspect production var_inits_snoc
vis::Var_Inits ::= vis1::Var_Inits vi::Var_Init {
 vis.neededFullyQualifiedTypes = vis1.neededFullyQualifiedTypes ++ vi.neededFullyQualifiedTypes;
}

aspect production method_dcl_prod
mdcl::Method_Dcl ::=  mh::Method_Header b::Block {
 mdcl.neededFullyQualifiedTypes = mh.neededFullyQualifiedTypes ++ b.neededFullyQualifiedTypes;
}

aspect production method_dcl_no_body
mdcl::Method_Dcl ::= mh::Method_Header {
 mdcl.neededFullyQualifiedTypes = mh.neededFullyQualifiedTypes;
}

aspect production method_header_prod
mh::Method_Header ::= mods::Modifiers return_ty::Type mname::Id_t fps::Formal_Params th::Throws {
 mh.neededFullyQualifiedTypes = return_ty.neededFullyQualifiedTypes ++ fps.neededFullyQualifiedTypes ++ th.neededFullyQualifiedTypes;
}

aspect production void_type
t::Type ::= {
 t.neededFullyQualifiedTypes = [];
}

aspect production formal_params_none
fps::Formal_Params ::= {
 fps.neededFullyQualifiedTypes = [];
}

aspect production formal_params_one
fps::Formal_Params ::= fp::Formal_Param {
 fps.neededFullyQualifiedTypes = fp.neededFullyQualifiedTypes;
}

aspect production formal_params_snoc
fps::Formal_Params ::= fps1::Formal_Params fp::Formal_Param {
 fps.neededFullyQualifiedTypes = fps1.neededFullyQualifiedTypes ++ fp.neededFullyQualifiedTypes;
}

aspect production formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
 fp.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ vid.neededFullyQualifiedTypes;
}

aspect production return_statement
s::Stmt ::= t::Return_t {
 s.neededFullyQualifiedTypes = [];
}

aspect production return_expr
s::Stmt ::= t::Return_t e::Expr {
 s.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes;
}

aspect production method_header_declarator
mh::Method_Header ::= mods::Modifiers t::Type md::Method_Declarator th::Throws {
 mh.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ md.neededFullyQualifiedTypes ++ th.neededFullyQualifiedTypes;
}

aspect production method_declarator
md::Method_Declarator ::= id::Id_t fps::Formal_Params {
 md.neededFullyQualifiedTypes = fps.neededFullyQualifiedTypes;
}

aspect production method_declarator_array
md::Method_Declarator ::= md1::Method_Declarator {
 md.neededFullyQualifiedTypes = md1.neededFullyQualifiedTypes;
}

aspect production final_formal_param
fp::Formal_Param ::= t::Type vid::Var_Declarator_Id {
 fp.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ vid.neededFullyQualifiedTypes;
}

aspect production throws_none
th::Throws ::= {
 th.neededFullyQualifiedTypes = [];
}

aspect production throws
th::Throws ::= ctl::TypeNames {
 th.neededFullyQualifiedTypes = ctl.neededFullyQualifiedTypes;
}

aspect production class_constructor
cdcl::Class_Member_Dcl ::= mods::Modifiers id::Id_t fps::Formal_Params thr::Throws cb::Block {
 cdcl.neededFullyQualifiedTypes = fps.neededFullyQualifiedTypes ++ thr.neededFullyQualifiedTypes ++ cb.neededFullyQualifiedTypes;
}

aspect production stmt_constructor_invocation
stmt::Stmt ::= inv::Constructor_Invocation {
 stmt.neededFullyQualifiedTypes = inv.neededFullyQualifiedTypes;
}

aspect production this_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
 inv.neededFullyQualifiedTypes = args.neededFullyQualifiedTypes;
}

aspect production super_constructor_invocation
inv::Constructor_Invocation ::= args::Exprs {
 inv.neededFullyQualifiedTypes = args.neededFullyQualifiedTypes;
}

aspect production this_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
 inv.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes ++ args.neededFullyQualifiedTypes;
}

aspect production this_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
 inv.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ args.neededFullyQualifiedTypes;
}

aspect production super_dot_constructor_invocation
inv::Constructor_Invocation ::= expr::Expr args::Exprs {
 inv.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes ++ args.neededFullyQualifiedTypes;
}

aspect production super_dot_constructor_name_invocation
inv::Constructor_Invocation ::= t::TypeName args::Exprs {
 inv.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ args.neededFullyQualifiedTypes;
}

aspect production primitive_type
t::Type ::= t1::Primitive_Type {
 t.neededFullyQualifiedTypes = [];
}

aspect production reference_type
t::Type ::= t1::Reference_Type {
 t.neededFullyQualifiedTypes = t1.neededFullyQualifiedTypes;
}

aspect production type_typerep
t::Type ::= tr::TypeRep {
 t.neededFullyQualifiedTypes = [];
}

aspect production name_type
t::Reference_Type ::= n::TypeName {
 t.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes;
}

aspect production type_names_none
ns::TypeNames ::= {
 ns.neededFullyQualifiedTypes = [];
}

aspect production type_names_one
ns::TypeNames ::= n::TypeName {
 ns.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes;
}

aspect production type_names_snoc
ns::TypeNames ::= ns1::TypeNames n::TypeName {
 ns.neededFullyQualifiedTypes = ns1.neededFullyQualifiedTypes ++ n.neededFullyQualifiedTypes;
}

aspect production type_interface_dcl
td::Type_Dcl ::= idcl::Interface_Dcl {
 td.neededFullyQualifiedTypes = idcl.neededFullyQualifiedTypes;
}

aspect production interface_member_dcls_none
idcls::Interface_Member_Dcls ::= {
 idcls.neededFullyQualifiedTypes = [];
}

aspect production interface_member_dcls_snoc
idcls::Interface_Member_Dcls ::= idcls1::Interface_Member_Dcls idcl::Interface_Member_Dcl {
 idcls.neededFullyQualifiedTypes = idcls1.neededFullyQualifiedTypes ++ idcl.neededFullyQualifiedTypes;
}

aspect production interface_field
idcl::Interface_Member_Dcl ::= fd::Field_Dcl {
 idcl.neededFullyQualifiedTypes = fd.neededFullyQualifiedTypes;
}

aspect production interface_method
idcl::Interface_Member_Dcl ::= mh::Method_Header {
 idcl.neededFullyQualifiedTypes = mh.neededFullyQualifiedTypes;
}

aspect production interface_empty
idcl::Interface_Member_Dcl ::= {
 idcl.neededFullyQualifiedTypes = [];
}

aspect production interface_dcl
idcl::Interface_Dcl ::= mods::Modifiers iname::Id_t inters::TypeNames dcls::Interface_Member_Dcls {
 idcl.neededFullyQualifiedTypes = inters.neededFullyQualifiedTypes ++ dcls.neededFullyQualifiedTypes;
}

aspect production interface_inner_class
idcl::Interface_Member_Dcl ::= cd::Class_Dcl {
 idcl.neededFullyQualifiedTypes = cd.neededFullyQualifiedTypes;
}

aspect production interface_inner_interface
idcl::Interface_Member_Dcl ::= id::Interface_Dcl {
 idcl.neededFullyQualifiedTypes = id.neededFullyQualifiedTypes;
}

aspect production cast_name_array
e::Expr ::= t::TypeName dims::Integer e1::Expr {
 e.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes ++ e1.neededFullyQualifiedTypes;
}

aspect production cast_primitive_array
e::Expr ::= t::Primitive_Type dims::Integer e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production not
e::Expr ::= e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production or_or
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production and_and
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production super_field_access
lhs::LHS ::= id::Id_t {
 lhs.neededFullyQualifiedTypes = [];
}

aspect production name_super_field_access
lhs::LHS ::= n::TypeName id::Id_t {
 lhs.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes;
}

aspect production super_method_call
e::Stmt_Expr ::= id::Id_t es::Exprs {
 e.neededFullyQualifiedTypes = es.neededFullyQualifiedTypes;
}

aspect production name_super_method_call
e::Stmt_Expr ::= n::TypeName id::Id_t es::Exprs {
 e.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes ++ es.neededFullyQualifiedTypes;
}

aspect production array_access
lhs::LHS ::= n::ExprName e1::Expr {
 lhs.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes ++ e1.neededFullyQualifiedTypes;
}

aspect production array_access_general
lhs::LHS ::= e1::Expr e2::Expr {
 lhs.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production mul_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production div_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production mod_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production plus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production minus_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production lshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production rshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production urshift_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production and_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production xor_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production or_assign
e::Stmt_Expr ::= lhs::LHS expr::Expr {
 e.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ expr.neededFullyQualifiedTypes;
}

aspect production new_class_expr
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs {
 e.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes ++ es.neededFullyQualifiedTypes;
}

aspect production new_class_expr_body
e::Stmt_Expr ::= expr::Expr id::Id_t es::Exprs cb::Class_Body {
 e.neededFullyQualifiedTypes = expr.neededFullyQualifiedTypes ++ es.neededFullyQualifiedTypes ++ cb.neededFullyQualifiedTypes;
}

aspect production new_class_name
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs {
 e.neededFullyQualifiedTypes = nam.neededFullyQualifiedTypes ++ es.neededFullyQualifiedTypes;
}

aspect production new_class_name_body
e::Stmt_Expr ::= nam::TypeName id::Id_t es::Exprs cb::Class_Body {
 e.neededFullyQualifiedTypes = nam.neededFullyQualifiedTypes ++ es.neededFullyQualifiedTypes ++ cb.neededFullyQualifiedTypes;
}

aspect production conditional
e::Expr ::= cond::Expr thenexpr::Expr elseexpr::Expr {
 e.neededFullyQualifiedTypes = cond.neededFullyQualifiedTypes ++ thenexpr.neededFullyQualifiedTypes ++ elseexpr.neededFullyQualifiedTypes;
}

aspect production or
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production xor
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production and
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production eq
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production not_eq
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production lt
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production gt
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production lteq
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production gteq
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production instanceof
e::Expr ::= e1::Expr t1::Reference_Type {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ t1.neededFullyQualifiedTypes;
}

aspect production lshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production rshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production urshift
e::Expr ::= e1::Expr e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production div
e::Expr ::= e1::Expr _ e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production mod
e::Expr ::= e1::Expr _ e2::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes ++ e2.neededFullyQualifiedTypes;
}

aspect production unary_plus
e::Expr ::= e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production unary_minus
e::Expr ::= e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production pre_inc
e::Stmt_Expr ::= e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production pre_dec
e::Stmt_Expr ::= e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production comp
e::Expr ::= e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production post_inc
e::Stmt_Expr ::= e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production post_dec
e::Stmt_Expr ::= e1::Expr {
 e.neededFullyQualifiedTypes = e1.neededFullyQualifiedTypes;
}

aspect production new_array_init_primitive
e::Expr ::= t1::Primitive_Type dims::Integer ai::Array_Init {
 e.neededFullyQualifiedTypes = ai.neededFullyQualifiedTypes;
}

aspect production new_array_init_name
e::Expr ::= n::TypeName dims::Integer ai::Array_Init {
 e.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes ++ ai.neededFullyQualifiedTypes;
}

aspect production new_array_no_init_primitive
e::Expr ::= t1::Primitive_Type d1::Dim_Exprs dims::Integer {
 e.neededFullyQualifiedTypes = d1.neededFullyQualifiedTypes;
}

aspect production new_array_no_init_name
e::Expr ::= n::TypeName d1::Dim_Exprs dims::Integer {
 e.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes ++ d1.neededFullyQualifiedTypes;
}

aspect production primitive_dot_class
e::Expr ::= t::Primitive_Type {
 e.neededFullyQualifiedTypes = [];
}

aspect production void_dot_class
e::Expr ::= {
 e.neededFullyQualifiedTypes = [];
}

aspect production array_dot_class
e::Expr ::= t::Array_Type {
 e.neededFullyQualifiedTypes = t.neededFullyQualifiedTypes;
}

aspect production name_dot_class
e::Expr ::= n::TypeName {
 e.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes;
}

aspect production name_dot_this
e::Expr ::= n::TypeName {
 e.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes;
}

aspect production dim_exprs_one
d::Dim_Exprs ::= e::Expr {
 d.neededFullyQualifiedTypes = e.neededFullyQualifiedTypes;
}

aspect production dim_exprs_snoc
d::Dim_Exprs ::= d1::Dim_Exprs e::Expr {
 d.neededFullyQualifiedTypes = d1.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes;
}

aspect production array_type
t::Reference_Type ::= t1::Array_Type {
 t.neededFullyQualifiedTypes = t1.neededFullyQualifiedTypes;
}

aspect production primitive_array
t::Array_Type ::= t1::Primitive_Type ds::Integer {
 t.neededFullyQualifiedTypes = [];
}

aspect production name_array
t::Array_Type ::= n::TypeName ds::Integer {
 t.neededFullyQualifiedTypes = n.neededFullyQualifiedTypes;
}

aspect production simple_package_name
pn::PackageName ::= id::Id_t {
 pn.neededFullyQualifiedTypes = [];
}

aspect production qualified_package_name
pn::PackageName ::= pn2::PackageName id::Id_t {
 pn.neededFullyQualifiedTypes = pn2.neededFullyQualifiedTypes;
}

aspect production simple_expr_name
en::ExprName ::= id::Id_t {
 en.neededFullyQualifiedTypes = [];
}

aspect production simple_method_name
mn::MethodName ::= id::Id_t  { 
 mn.neededFullyQualifiedTypes = [];
}

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
