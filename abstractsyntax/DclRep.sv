grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

import edu:umn:cs:melt:ableJ14:terminals;

nonterminal DclRep with name, unparse, is_package, is_class, is_interface, is_field, is_method, is_constructor, is_param, is_local, is_type, package_rep, class_rep, interface_rep, field_rep, method_rep, constructor_rep, param_rep, local_rep, typerep;

-- Information common to all declaration representations.

-- To specify what kind of declaration it is.
synthesized attribute is_package :: Boolean;
synthesized attribute is_class :: Boolean;
synthesized attribute is_interface :: Boolean;
synthesized attribute is_field :: Boolean;
synthesized attribute is_method :: Boolean;
synthesized attribute is_constructor :: Boolean;
synthesized attribute is_param :: Boolean;
synthesized attribute is_local :: Boolean;
synthesized attribute is_type :: Boolean;

-- Declaration specific information.
synthesized attribute package_rep :: PackageDclRep;
synthesized attribute class_rep :: ClassDclRep;
synthesized attribute interface_rep :: InterfaceDclRep;
synthesized attribute field_rep :: FieldDclRep;
synthesized attribute method_rep :: MethodDclRep;
synthesized attribute constructor_rep :: ConstructorDclRep;
synthesized attribute param_rep :: ParamDclRep;
synthesized attribute local_rep :: LocalDclRep;

-- Section 6.1 defines the kinds of declarations.
--------------------------------------------------
-- package_dcl, class_dcl, interface_dcl, field_dcl, method_dcl, param_dcl, local_dcl

abstract production dcl_rep_error
di::DclRep ::= {
  di.name = "dcl_rep_error";
  di.unparse = "dcl_rep_error ()";
  di.is_package = false;
  di.is_class = false;
  di.is_interface = false;
  di.is_field = false;
  di.is_method = false;
  di.is_param = false;
  di.is_local = false;
  di.is_type = false;

  di.package_rep = error ("Not a Package Dcl");
  di.class_rep = error ("Not a Class Dcl");
  di.interface_rep = error ("Not a Interface Dcl");
  di.field_rep = error ("Not a Field Dcl");
  di.method_rep = error ("Not a Method Dcl");
  di.param_rep = error ("Not a Param Dcl");
  di.local_rep = error ("Not a Local Dcl");
  di.typerep = error ("DclRep does not have typerep");
}

function errorDcl
DclRep ::= {
  return dcl_rep_error ();
}

-----------------------------------------------------------------------
-- Package Dcl
-----------------------------------------------------------------------

nonterminal PackageDclRep with name, unparse, fullyQualifiedName;

abstract production dcl_rep_package
di::DclRep ::= pi::PackageDclRep {
  di.name = pi.name;
  di.unparse = "dcl_rep_package (" ++ pi.unparse ++ ")";
  di.is_package = true;
  di.package_rep = pi;
  forwards to dcl_rep_error ();
}

abstract production package_dcl_rep
pi::PackageDclRep ::= id::String fqn::FullyQualifiedName {
  pi.name = id;
  pi.unparse = "package_dcl_rep (\"" ++ id ++ "\", " ++ fqn.unparse ++ ")";
  pi.fullyQualifiedName = fqn;
}

function packageDcl
DclRep ::= id::String fqn::FullyQualifiedName {
  return dcl_rep_package (package_dcl_rep (id, fqn));
}

-----------------------------------------------------------------------
-- Class Dcl
-----------------------------------------------------------------------

nonterminal ClassDclRep with name, unparse, fullyQualifiedName, typerep;

abstract production dcl_rep_class
di::DclRep ::= ci::ClassDclRep {
  di.name = ci.name;
  di.unparse = "dcl_rep_class (" ++ ci.unparse ++ ")";
  di.is_class = true;
  di.class_rep = ci;
  di.is_type = true;
  di.typerep = ci.typerep;
  forwards to dcl_rep_error ();
}

abstract production class_dcl_rep
ci::ClassDclRep ::= fqn::FullyQualifiedName tr::TypeRep {
  ci.name = tr.eqName;
  ci.unparse = "class_dcl_rep (" ++ fqn.unparse ++ ", " ++ tr.unparse ++ ")";
  ci.fullyQualifiedName = fqn;
  ci.typerep = tr;
}

function classDcl
DclRep ::= fqn::FullyQualifiedName tr::TypeRep {
  return dcl_rep_class (class_dcl_rep (fqn, tr));
}

-----------------------------------------------------------------------
-- Interface Dcl
-----------------------------------------------------------------------

nonterminal InterfaceDclRep with name, unparse, fullyQualifiedName, typerep;

abstract production dcl_rep_interface
di::DclRep ::= ii::InterfaceDclRep {
  di.name = ii.name;
  di.unparse = "dcl_rep_interface (" ++ ii.unparse ++ ")";
  di.is_interface = true;
  di.interface_rep = ii;
  di.is_type = true;
  di.typerep = ii.typerep;
  forwards to dcl_rep_error ();
}

abstract production interface_dcl_rep
ii::InterfaceDclRep ::= fqn::FullyQualifiedName tr::TypeRep {
  ii.name = tr.eqName;
  ii.unparse = "interface_dcl_rep (" ++ fqn.unparse ++ ", " ++ tr.unparse ++ ")";
  ii.fullyQualifiedName = fqn;
  ii.typerep = tr;
}

function interfaceDcl
DclRep ::= fqn::FullyQualifiedName tr::TypeRep {
  return dcl_rep_interface (interface_dcl_rep (fqn, tr));
}

-----------------------------------------------------------------------
-- Field Dcl
-----------------------------------------------------------------------

nonterminal FieldDclRep with name, unparse, typerep, modlist, is_static;
synthesized attribute is_static :: Boolean;

abstract production dcl_rep_field
di::DclRep ::= fi::FieldDclRep {
  di.name = fi.name;
  di.unparse = "dcl_rep_field (" ++ fi.unparse ++ ")";
  di.is_field = true;
  di.field_rep = fi;
  di.typerep = fi.typerep;
  forwards to dcl_rep_error ();
}

abstract production field_dcl_rep
fi::FieldDclRep ::= id::String mods::[ Modifier ] tr::TypeRep {
  fi.name = id;
  fi.unparse = "field_dcl_rep (\"" ++ id ++ "\", " ++ unparseModifiers (mods) ++ ", " ++ tr.unparse ++ ")";
  fi.typerep = tr;
  fi.modlist = mods;
  fi.is_static = isStatic (mods);
}

function fieldDcl
DclRep ::= id::String mods::[ Modifier ] tr::TypeRep {
  return dcl_rep_field (field_dcl_rep (id, mods, tr));
}

-----------------------------------------------------------------------
-- Method Dcl
-----------------------------------------------------------------------

nonterminal MethodDclRep with name, unparse, return_type, param_types, modlist, is_static, is_void;
synthesized attribute is_void :: Boolean;

abstract production dcl_rep_method
di::DclRep ::= mi::MethodDclRep {
  di.name = mi.name;
  di.unparse = "dcl_rep_method (" ++ mi.unparse ++ ")";
  di.is_method = true;
  di.method_rep = mi;
  forwards to dcl_rep_error ();
}

abstract production method_dcl_rep
mi::MethodDclRep ::= id::String mods::[ Modifier ] rettr::TypeRep ptypes::[ TypeRep ] {
  mi.name = id;
  mi.unparse = "method_dcl_rep (\"" ++ id ++ "\", " ++ unparseModifiers (mods) ++ ", " ++ rettr.unparse ++ ", " ++ unparseTypeReps (ptypes) ++ ")";
  mi.return_type = rettr;
  mi.param_types = ptypes;
  mi.modlist = mods;
  mi.is_static = isStatic (mods);
  mi.is_void = equality_check (rettr, voidTypeRep ());
}

abstract production unknown_method_dcl_rep
mi::MethodDclRep ::= id::String {
  mi.name = id;
  mi.unparse = "unknown_method_dcl_rep (\"" ++ id ++ "\")";
  mi.return_type = unknownTypeRep ();
  mi.param_types = [];
  mi.modlist = [];
  mi.is_static = false;
  mi.is_void = false;
}

function methodDcl
DclRep ::= id::String mods::[ Modifier ] rettr::TypeRep ptypes::[ TypeRep ] {
  return dcl_rep_method (method_dcl_rep (id, mods, rettr, ptypes));
}

-----------------------------------------------------------------------
-- Constructor Dcl
-----------------------------------------------------------------------

nonterminal ConstructorDclRep with name, unparse, param_types, modlist;

abstract production dcl_rep_constructor
di::DclRep ::= ci::ConstructorDclRep {
  di.name = ci.name;
  di.unparse = "dcl_rep_constructor (" ++ ci.unparse ++ ")";
  di.is_constructor = true;
  di.constructor_rep = ci;
  forwards to dcl_rep_error ();
}

abstract production constructor_dcl_rep
ci::ConstructorDclRep ::= id::String mods::[ Modifier ] ptypes::[ TypeRep ] {
  ci.name = id;
  ci.unparse = "constructor_dcl_rep (\"" ++ id ++ "\", " ++ unparseModifiers (mods) ++ ", " ++ unparseTypeReps (ptypes) ++ ")";
  ci.param_types = ptypes;
  ci.modlist = mods;
}

function constructorDcl
DclRep ::= id::String mods::[ Modifier ] ptypes::[ TypeRep ] {
  return dcl_rep_constructor (constructor_dcl_rep (id, mods, ptypes));
}

-----------------------------------------------------------------------
-- Param Dcl
-----------------------------------------------------------------------

nonterminal ParamDclRep with name, unparse, typerep;

abstract production dcl_rep_param
di::DclRep ::= pi::ParamDclRep {
  di.name = pi.name;
  di.unparse = "dcl_rep_param (" ++ pi.unparse ++ ")";
  di.is_param = true;
  di.param_rep = pi;
  di.typerep = pi.typerep;
  forwards to dcl_rep_error ();
}

abstract production param_dcl_rep
pi::ParamDclRep ::= id::String tr::TypeRep {
  pi.name = id;
  pi.unparse = "param_dcl_rep (\"" ++ id ++ "\", " ++ tr.unparse ++ ")";
  pi.typerep = tr ;
}

function paramDcl
DclRep ::= id::String t::TypeRep {
  return dcl_rep_param (param_dcl_rep (id, t));
}

-----------------------------------------------------------------------
-- Local Dcl
-----------------------------------------------------------------------

nonterminal LocalDclRep with name, unparse, typerep;

abstract production dcl_rep_local
di::DclRep ::= li::LocalDclRep {
  di.name = li.name;
  di.unparse = "dcl_rep_local (" ++ li.unparse ++ ")";
  di.is_local = true;
  di.local_rep = li;
  di.typerep = li.typerep;
  forwards to dcl_rep_error ();
}

abstract production local_dcl_rep
li::LocalDclRep ::= id::String tr::TypeRep {
  li.name = id;
  li.unparse = "local_dcl_rep (\"" ++ id ++ "\", " ++ tr.unparse ++ ")";
  li.typerep = tr;
}

function localDcl
DclRep ::= id::String t::TypeRep {
  return dcl_rep_local (local_dcl_rep (id,t));
}

