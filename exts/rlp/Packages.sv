grammar edu:umn:cs:melt:ableJ14:exts:rlp;
import edu:umn:cs:melt:ableJ14:abstractsyntax;

-- neededImportedSingleTypes
-- neededCurrentPackageTypes
-- neededImportedOnDemandTypes
-- localTypes
-- neededFullyQualifiedTypes

aspect production signHost
s::Stmt ::= lhs::LHS e::Expr {
 s.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ e.neededImportedSingleTypes;
}

aspect production signSym
s::Stmt ::= lhs::LHS e::Expr {
 s.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ e.neededImportedSingleTypes;
}

aspect production equalInt
e::Expr ::= l::Expr r::Expr {
 e.neededImportedSingleTypes = l.neededImportedSingleTypes ++ r.neededImportedSingleTypes;
}

aspect production addSym
e::Expr ::= l::Expr r::Expr {
 e.neededImportedSingleTypes = l.neededImportedSingleTypes ++ r.neededImportedSingleTypes;
}

aspect production subtractSym
e::Expr ::= l::Expr r::Expr {
 e.neededImportedSingleTypes = l.neededImportedSingleTypes ++ r.neededImportedSingleTypes;
}

aspect production multiplySym
e::Expr ::= l::Expr r::Expr {
 e.neededImportedSingleTypes = l.neededImportedSingleTypes ++ r.neededImportedSingleTypes;
}



aspect production signRLP
s::Stmt ::= lhs::LHS e::Expr {
 s.neededImportedSingleTypes = lhs.neededImportedSingleTypes ++ e.neededImportedSingleTypes;
}

aspect production multiplyRLP
e::Expr ::= l::Expr r::Expr {
 e.neededImportedSingleTypes = l.neededImportedSingleTypes ++ r.neededImportedSingleTypes;
}

aspect production addRLP
e::Expr ::= l::Expr r::Expr {
 e.neededImportedSingleTypes = l.neededImportedSingleTypes ++ r.neededImportedSingleTypes;
}

aspect production subtractRLP
e::Expr ::= l::Expr r::Expr {
 e.neededImportedSingleTypes = l.neededImportedSingleTypes ++ r.neededImportedSingleTypes;
}


aspect production rlp_local_var_dcl
s::Stmt ::= pt::Type  dcl::Var_Declarator {
 s.neededImportedSingleTypes = pt.neededImportedSingleTypes ++ dcl.neededImportedSingleTypes;
}

------------------

aspect production signHost
s::Stmt ::= lhs::LHS e::Expr {
 s.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ e.neededCurrentPackageTypes;
}

aspect production signSym
s::Stmt ::= lhs::LHS e::Expr {
 s.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ e.neededCurrentPackageTypes;
}

aspect production equalInt
e::Expr ::= l::Expr r::Expr {
 e.neededCurrentPackageTypes = l.neededCurrentPackageTypes ++ r.neededCurrentPackageTypes;
}

aspect production addSym
e::Expr ::= l::Expr r::Expr {
 e.neededCurrentPackageTypes = l.neededCurrentPackageTypes ++ r.neededCurrentPackageTypes;
}

aspect production subtractSym
e::Expr ::= l::Expr r::Expr {
 e.neededCurrentPackageTypes = l.neededCurrentPackageTypes ++ r.neededCurrentPackageTypes;
}

aspect production multiplySym
e::Expr ::= l::Expr r::Expr {
 e.neededCurrentPackageTypes = l.neededCurrentPackageTypes ++ r.neededCurrentPackageTypes;
}



aspect production signRLP
s::Stmt ::= lhs::LHS e::Expr {
 s.neededCurrentPackageTypes = lhs.neededCurrentPackageTypes ++ e.neededCurrentPackageTypes;
}

aspect production multiplyRLP
e::Expr ::= l::Expr r::Expr {
 e.neededCurrentPackageTypes = l.neededCurrentPackageTypes ++ r.neededCurrentPackageTypes;
}

aspect production addRLP
e::Expr ::= l::Expr r::Expr {
 e.neededCurrentPackageTypes = l.neededCurrentPackageTypes ++ r.neededCurrentPackageTypes;
}

aspect production subtractRLP
e::Expr ::= l::Expr r::Expr {
 e.neededCurrentPackageTypes = l.neededCurrentPackageTypes ++ r.neededCurrentPackageTypes;
}


aspect production rlp_local_var_dcl
s::Stmt ::= pt::Type  dcl::Var_Declarator {
 s.neededCurrentPackageTypes = pt.neededCurrentPackageTypes ++ dcl.neededCurrentPackageTypes;
}

------------------

aspect production signHost
s::Stmt ::= lhs::LHS e::Expr {
 s.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ e.neededImportedOnDemandTypes;
}

aspect production signSym
s::Stmt ::= lhs::LHS e::Expr {
 s.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ e.neededImportedOnDemandTypes;
}

aspect production equalInt
e::Expr ::= l::Expr r::Expr {
 e.neededImportedOnDemandTypes = l.neededImportedOnDemandTypes ++ r.neededImportedOnDemandTypes;
}

aspect production addSym
e::Expr ::= l::Expr r::Expr {
 e.neededImportedOnDemandTypes = l.neededImportedOnDemandTypes ++ r.neededImportedOnDemandTypes;
}

aspect production subtractSym
e::Expr ::= l::Expr r::Expr {
 e.neededImportedOnDemandTypes = l.neededImportedOnDemandTypes ++ r.neededImportedOnDemandTypes;
}

aspect production multiplySym
e::Expr ::= l::Expr r::Expr {
 e.neededImportedOnDemandTypes = l.neededImportedOnDemandTypes ++ r.neededImportedOnDemandTypes;
}



aspect production signRLP
s::Stmt ::= lhs::LHS e::Expr {
 s.neededImportedOnDemandTypes = lhs.neededImportedOnDemandTypes ++ e.neededImportedOnDemandTypes;
}

aspect production multiplyRLP
e::Expr ::= l::Expr r::Expr {
 e.neededImportedOnDemandTypes = l.neededImportedOnDemandTypes ++ r.neededImportedOnDemandTypes;
}

aspect production addRLP
e::Expr ::= l::Expr r::Expr {
 e.neededImportedOnDemandTypes = l.neededImportedOnDemandTypes ++ r.neededImportedOnDemandTypes;
}

aspect production subtractRLP
e::Expr ::= l::Expr r::Expr {
 e.neededImportedOnDemandTypes = l.neededImportedOnDemandTypes ++ r.neededImportedOnDemandTypes;
}


aspect production rlp_local_var_dcl
s::Stmt ::= pt::Type  dcl::Var_Declarator {
 s.neededImportedOnDemandTypes = pt.neededImportedOnDemandTypes ++ dcl.neededImportedOnDemandTypes;
}

------------------

aspect production signHost
s::Stmt ::= lhs::LHS e::Expr {
 s.localTypes = [];
}

aspect production signSym
s::Stmt ::= lhs::LHS e::Expr {
 s.localTypes = [];
}

aspect production signRLP
s::Stmt ::= lhs::LHS e::Expr {
 s.localTypes = [];
}

aspect production rlp_local_var_dcl
s::Stmt ::= pt::Type  dcl::Var_Declarator {
 s.localTypes = [];
}

------------------

aspect production signHost
s::Stmt ::= lhs::LHS e::Expr {
-- added edu.umn.cs.melt.ableJ14.exts.rlp.Sign in Symbolic.sv
-- s.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes;
}

aspect production signSym
s::Stmt ::= lhs::LHS e::Expr {
-- added edu.umn.cs.melt.ableJ14.exts.rlp.Sign in Symbolic.sv
-- s.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes;
}

aspect production equalInt
e::Expr ::= l::Expr r::Expr {
 e.neededFullyQualifiedTypes = l.neededFullyQualifiedTypes ++ r.neededFullyQualifiedTypes;
}

aspect production addSym
e::Expr ::= l::Expr r::Expr {
 e.neededFullyQualifiedTypes = l.neededFullyQualifiedTypes ++ r.neededFullyQualifiedTypes;
}

aspect production subtractSym
e::Expr ::= l::Expr r::Expr {
 e.neededFullyQualifiedTypes = l.neededFullyQualifiedTypes ++ r.neededFullyQualifiedTypes;
}

aspect production multiplySym
e::Expr ::= l::Expr r::Expr {
 e.neededFullyQualifiedTypes = l.neededFullyQualifiedTypes ++ r.neededFullyQualifiedTypes;
}



aspect production signRLP
s::Stmt ::= lhs::LHS e::Expr {
-- added java.lang.Math in RLP.sv
-- s.neededFullyQualifiedTypes = lhs.neededFullyQualifiedTypes ++ e.neededFullyQualifiedTypes;
}

aspect production multiplyRLP
e::Expr ::= l::Expr r::Expr {
 e.neededFullyQualifiedTypes = l.neededFullyQualifiedTypes ++ r.neededFullyQualifiedTypes;
}

aspect production addRLP
e::Expr ::= l::Expr r::Expr {
 e.neededFullyQualifiedTypes = l.neededFullyQualifiedTypes ++ r.neededFullyQualifiedTypes;
}

aspect production subtractRLP
e::Expr ::= l::Expr r::Expr {
 e.neededFullyQualifiedTypes = l.neededFullyQualifiedTypes ++ r.neededFullyQualifiedTypes;
}


aspect production rlp_local_var_dcl
s::Stmt ::= pt::Type  dcl::Var_Declarator {
-- added java.lang.Math in RLP.sv
-- s.neededFullyQualifiedTypes = pt.neededFullyQualifiedTypes ++ dcl.neededFullyQualifiedTypes;
}

------------------
