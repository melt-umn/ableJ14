grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

nonterminal Modifiers with         pp, basepp, modlist, errors;
nonterminal Modifier with          pp, basepp, errors, unparse;

synthesized attribute modlist :: [ Modifier ];

abstract production modifiers_none
mods::Modifiers ::= {
  mods.pp = "";
  mods.basepp = "";
  mods.modlist = [];
  mods.errors := [];
}

abstract production modifiers_snoc
mods::Modifiers ::= mods1::Modifiers modif::Modifier {
  mods.pp = mods1.pp ++ modif.pp;
  mods.basepp = mods1.basepp ++ modif.basepp;
  mods.modlist = mods1.modlist ++ [modif];
  mods.errors := mods1.errors ++ modif.errors;
}

abstract production modifiers_one
mods::Modifiers ::= modif::Modifier {
 forwards to modifiers_snoc (modifiers_none(), modif) ;
}

abstract production public
modif::Modifier ::= {
  modif.pp = "public ";
  modif.basepp = "public ";
  modif.errors := [];
  modif.unparse = "public ()";
}

abstract production protected
modif::Modifier ::= {
  modif.pp = "protected ";
  modif.basepp = "protected ";
  modif.errors := [];
  modif.unparse = "protected ()";
}

abstract production private
modif::Modifier ::= {
  modif.pp = "private ";
  modif.basepp = "private ";
  modif.errors := [];
  modif.unparse = "private ()";
}

abstract production static_mod
modif::Modifier ::= {
  modif.pp = "static ";
  modif.basepp = "static ";
  modif.errors := [];
  modif.unparse = "static ()";
}

abstract production abstract_mod
modif::Modifier ::= {
  modif.pp = "abstract ";
  modif.basepp = "abstract ";
  modif.errors := [];
  modif.unparse = "abstract_mod ()";
}

abstract production final
modif::Modifier ::= {
  modif.pp = "final ";
  modif.basepp = "final ";
  modif.errors := [];
  modif.unparse = "final ()";
}

abstract production native
modif::Modifier ::= {
  modif.pp = "native ";
  modif.basepp = "native ";
  modif.errors := [];
  modif.unparse = "native ()";
}

abstract production synchronized_mod
modif::Modifier ::= {
  modif.pp = "synchronized ";
  modif.basepp = "synchronized ";
  modif.errors := [];
  modif.unparse = "synchronized ()";
}

abstract production transient
modif::Modifier ::= {
  modif.pp = "transient ";
  modif.basepp = "transient ";
  modif.errors := [];
  modif.unparse = "transient ()";
}

abstract production volatile
modif::Modifier ::= {
  modif.pp = "volatile ";
  modif.basepp = "volatile ";
  modif.errors := [];
  modif.unparse = "volatile ()";
}

abstract production strictfp
modif::Modifier ::= {
  modif.pp = "strictfp ";
  modif.basepp = "strictfp ";
  modif.errors := [];
  modif.unparse = "strictfp ()";
}

function isPublic
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "public " || isPublic (tail (mods)));
}

function isProtected
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "protected " || isProtected (tail (mods)));
}

function isPrivate
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "private " || isPrivate (tail (mods)));
}

function isUnspecified
Boolean ::=mods::[ Modifier ] {
  return !isPublic (mods) && !isProtected (mods) && !isPrivate (mods);
}

function isStatic
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "static " || isStatic (tail (mods)));
}

function isAbstract
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "abstract " || isAbstract (tail (mods)));
}

function isFinal
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "final " || isFinal (tail (mods)));
}

function isNative
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "native " || isNative (tail (mods)));
}

function isSynchronized
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "synchronized " || isSynchronized (tail (mods)));
}

function isTransient
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "transient " || isTransient (tail (mods)));
}

function isVolatile
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "volatile " || isVolatile (tail (mods)));
}

function isStrictfp
Boolean ::=mods::[ Modifier ] {
  return if null (mods) then false else ((head (mods)).pp == "strictfp " || isStrictfp (tail (mods)));
}

function printModifiers
String ::= modifiers::[ Modifier ] {
 return if null (modifiers)
	then ""
	else (head (modifiers)).pp ++ (if null (tail (modifiers)) then "" else ", ") ++ printModifiers (tail (modifiers));
}

function unparseModifiers
String ::= modifiers::[ Modifier ] {
 return "[" ++ unparseModifiersHelper (modifiers) ++ "]";
}

function unparseModifiersHelper
String ::= modifiers::[ Modifier ] {
 return if null (modifiers)
	then ""
	else (head (modifiers)).unparse ++ (if null (tail (modifiers)) then "" else ", ") ++ unparseModifiersHelper (tail (modifiers));
}
