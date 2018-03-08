grammar edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:abstractsyntax:exprs;
import edu:umn:cs:melt:ableJ14:terminals;

-- This file is organized as follows:
-- 0. Some attributes for TypeRep
-- 1. productions for reference types
-- 2. productions for primitive types
-- 3. productions for interface types
-- 4. productions for the unknown type
-- 5. productions for the null type
-- 6. productions for the default type, that all TypeReps forwards to (directly or indirectly)


-- Simple name based equality checking
function nameBasedEquality
Boolean ::= t1::TypeRep t2::TypeRep
{ return t1.eqName == t2.eqName ; }

synthesized attribute eqName :: String ;
attribute eqName occurs on TypeRep ;

synthesized attribute typerep :: TypeRep ;

attribute pp occurs on TypeRep ;
attribute unparse occurs on TypeRep ;

synthesized attribute componentTypeRep :: TypeRep  occurs on TypeRep ;
-- to be used for type have have comonent types, such as arrays, or
-- the RLP type defined in the RLP extension.  If there are no component types
-- then this attribute should be the unknown type "unknownTypeRep".

--------------------------------------------------
-- Reference Types                              --
--------------------------------------------------
synthesized attribute classtyperep :: ClassTypeRep ;
attribute classtyperep occurs on TypeRep ;

synthesized attribute interfacetyperep :: InterfaceTypeRep ;
attribute interfacetyperep occurs on TypeRep ;


-- A production for default attribute definitions for Reference Types.
abstract production referenceTypeRep
t::TypeRep ::= orig::TypeRep
{  -- It is given as a child the TypeRep that forwards to it.
 t.isReferenceType = true ;

 -- type equality
 t.eqName = orig.eqName ;
 t.isEqualTo = nameBasedEquality (orig, t.isEqualTo_input) ;

 t.errors := [];
 t.pp = orig.pp ;
 t.unparse = "referenceTypeRep (" ++ orig.unparse ++ ")" ;

 -- subtype w/ conversion productions
 t.isSubTypeResult = mkSubTypeResult ( t.isSubType_ConvertBy ) ;

 t.isSubType_ConvertBy_Accum
   = standard_convertby_check_sofar (t.isSubType_ConvertBy_Accum_currCnvtBy,
                                     t.isSubTypeOf_input, 
                                     t.isSubType_ConvertBy_Accum_sofar) ;
 t.isSubType_ConvertBy
  = standard_convertby_check_sofar (mkInitCnvt(orig), 
                                    t.isSubTypeOf_input, [] ) ;

 t.amSuperTypeForNullType = true ;

 forwards to defaultTypeRep(orig);
}


abstract production classTypeRepDefs
t::TypeRep ::= classTR::ClassTypeRepDefs
{
 t.eqName = classTR.qualifiedName ;
 t.errors := [];
 t.isClassType = true;
 t.isReferenceType = true;
 t.pp = classTR.qualifiedName;
 t.unparse = "classTypeRepDefs (" ++ classTR.unparse ++ ")";
 forwards to referenceTypeRep(t);
}

abstract production classTypeRep
t::TypeRep ::= classTR::ClassTypeRep 
{
 t.eqName = classTR.qualifiedName ;
 t.errors := [];
 forwards to referenceTypeRep(t);
 t.classtyperep = classTR;
 t.isClassType = true;
 t.isReferenceType = true;

 t.pp = classTR.qualifiedName ;
 t.unparse = "classTypeRep (" ++ classTR.unparse ++ ")";
 t.superTypes_ConvertBy := classTR.superTypes_ConvertBy;

-- t.superTypes_ConvertBy
--   := (if (decorate classTR.superClass with {isEqualTo_input = retrieveTypeRep ("java.lang.Object", ...);}).isEqualTo
--       then []
--       else [ mkSubClassOfObject() ]  -- convert directly object 
--      )
--      ++ [ mkSubClassCnvt(classTR.superClass) ]  -- convert to immediate super class 
--      ++ mkInterfaceCnvtBys(classTR.interfaces)  -- convert to interface types ...
--      ;

-- t.isSubTypeResult = mkSubTypeResult ( t.isSubType_ConvertBy ) ;
--
-- t.isSubType_ConvertBy_Accum
--   = standard_convertby_check_sofar (t.isSubType_ConvertBy_Accum_currCnvtBy,
--                                     t.isSubTypeOf_input, 
--                                     t.isSubType_ConvertBy_Accum_sofar) ;
-- t.isSubType_ConvertBy
--  = standard_convertby_check_sofar (mkInitCnvt(t), 
--                                    t.isSubTypeOf_input, [] ) ;
}

attribute superTypes_ConvertBy occurs on ClassTypeRep;

aspect production class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production object_class_type_rep
ct::ClassTypeRep ::= modlist_::[ Modifier ] interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy := [];
}

aspect production string_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production boolean_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production char_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production byte_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production short_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production integer_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production long_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production float_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production double_class_type_rep
ct::ClassTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] parent::FullyQualifiedName interfaces_::[ FullyQualifiedName ] 
			fields_::[ EnvItem ] methods_::[ EnvItem ] constructors_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 ct.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]
      ++ [ mkSubClassCnvt (ct.superClass) ]
      ++ mkInterfaceCnvtBys (ct.interfaces)
      ;
}

aspect production unknown_class_type_rep ct::ClassTypeRep ::= {
 ct.superTypes_ConvertBy := [] ;
}

function mkInterfaceCnvtBys
[ConvertBy] ::= trs::[ TypeRep ]
{ return if null(trs) then []
         else mkSubInterfaceCnvt(head(trs)) :: mkInterfaceCnvtBys(tail(trs)) ; }


-- Interfaces
-------------

abstract production interfaceTypeRepDefs
t::TypeRep ::= interfaceTR::InterfaceTypeRepDefs
{
 t.eqName = interfaceTR.qualifiedName ;
 t.errors := [];
 t.isInterfaceType = true;
 t.isReferenceType = true;
 t.pp = interfaceTR.qualifiedName ;
 t.unparse = "interfaceTypeRepDefs (" ++ interfaceTR.unparse ++ ")";
 forwards to referenceTypeRep(t);
}

abstract production interfaceTypeRep
t::TypeRep ::= interfaceTR::InterfaceTypeRep 
{
 t.eqName = interfaceTR.qualifiedName ;
 t.errors := [];
 t.interfacetyperep = interfaceTR;
 forwards to referenceTypeRep(t);
 t.isInterfaceType = true;
 t.isReferenceType = true;

 t.pp = interfaceTR.qualifiedName;
 t.unparse = "interfaceTypeRep (" ++ interfaceTR.unparse ++ ")";
 t.superTypes_ConvertBy := interfaceTR.superTypes_ConvertBy;
}

attribute superTypes_ConvertBy occurs on InterfaceTypeRep;

aspect production interface_type_rep
it::InterfaceTypeRep ::= cname::String qualifiedName_::String modlist_::[ Modifier ] interfaces_::[ FullyQualifiedName ] fields_::[ EnvItem ] methods_::[ EnvItem ] innerTypes_::[ EnvItem ] environment::[ ScopeEnv ] {
 it.superTypes_ConvertBy
   :=    [ mkSubClassOfObject (environment) ]  -- convert directly object 
      ++ mkInterfaceCnvtBys (it.interfaces)  -- convert to interface types ...
      ;
}

aspect production unknown_interface_type_rep it::InterfaceTypeRep ::= {
 it.superTypes_ConvertBy := [] ;
}

--------------------------------------------------
-- Primitive Types                              --
--------------------------------------------------
abstract production primitiveTypeRep
t::TypeRep ::= orig::TypeRep
{
 t.isPrimitiveType = true ;
 t.amSuperTypeForNullType = false ; 

 t.errors := [];
 t.pp = orig.pp ;
 t.unparse = "primitiveTypeRep (" ++ orig.unparse ++ ")";

 -- type equality testing
 t.eqName = orig.eqName ;
 t.isEqualTo = nameBasedEquality (orig, t.isEqualTo_input) ;

 -- sub type checking
 t.isSubTypeResult = mkSubTypeResult ( t.isSubType_ConvertBy ) ;

 t.isSubType_ConvertBy_Accum
  = standard_convertby_check_sofar (t.isSubType_ConvertBy_Accum_currCnvtBy, 
                                    t.isSubTypeOf_input, 
                                    t.isSubType_ConvertBy_Accum_sofar) ;
 t.isSubType_ConvertBy
  = standard_convertby_check_sofar (mkInitCnvt(orig), 
                                    t.isSubTypeOf_input, [] ) ;


 forwards to defaultTypeRep(orig);
}



abstract production byteTypeRep
t::TypeRep ::=
{
 t.eqName = "byte" ;
 t.errors := [];
 t.pp = "byteTypeRep ()" ;
 t.unparse = "byteTypeRep ()" ;
 forwards to primitiveTypeRep(t);
 t.superTypes_ConvertBy := [ mkNamedIdCnvt("byte->short",shortTypeRep()) ] ;
}

abstract production shortTypeRep
t::TypeRep ::=
{
 t.eqName = "short" ;
 t.errors := [];
 t.pp = "shortTypeRep ()" ;
 t.unparse = "shortTypeRep ()" ;
 forwards to primitiveTypeRep(t);
 t.superTypes_ConvertBy := [ mkNamedIdCnvt("short->int",intTypeRep()) ] ;
}

abstract production charTypeRep
t::TypeRep ::=
{
 t.eqName = "char" ;
 t.errors := [];
 t.pp = "charTypeRep ()" ;
 t.unparse = "charTypeRep ()" ;
 forwards to primitiveTypeRep(t);
 t.superTypes_ConvertBy := [ mkNamedIdCnvt("char->int",intTypeRep()) ] ;
}

abstract production intTypeRep
t::TypeRep ::=
{
 t.eqName = "int" ;
 t.errors := [];
 t.pp = "int" ;
 t.unparse = "intTypeRep ()" ;
 forwards to primitiveTypeRep(t);
 t.superTypes_ConvertBy := [ mkNamedIdCnvt("int->long",longTypeRep()) ] ;
}


abstract production longTypeRep
t::TypeRep ::=
{
 t.eqName = "long" ;
 t.errors := [];
 t.pp = "long" ;
 t.unparse = "longTypeRep ()" ;
 forwards to primitiveTypeRep(t);
 t.superTypes_ConvertBy := [ mkNamedIdCnvt("long->float",floatTypeRep()) ] ;
}

abstract production floatTypeRep
t::TypeRep ::=
{
 t.eqName = "float" ;
 t.errors := [];
 t.pp = "float" ;
 t.unparse = "floatTypeRep ()" ;
 forwards to primitiveTypeRep(t);
 t.superTypes_ConvertBy := [ mkNamedIdCnvt("float->double",doubleTypeRep()) ] ;
}

abstract production doubleTypeRep
t::TypeRep ::=
{
 t.eqName = "double" ;
 t.errors := [];
 t.pp = "double" ;
 t.unparse = "doubleTypeRep ()" ;
 forwards to primitiveTypeRep(t);
 t.superTypes_ConvertBy := [] ;
}

abstract production booleanTypeRep
t::TypeRep ::=
{
 t.eqName = "boolean" ;
 t.errors := [];
 t.pp = "boolean" ;
 t.unparse = "booleanTypeRep ()" ;
 forwards to primitiveTypeRep(t);
 t.superTypes_ConvertBy := [] ; 
}

--------------------------------------------------
-- Array Type                                    --
--------------------------------------------------

abstract production arrayTypeRep
t::TypeRep ::= elementTypeRep::TypeRep dims::Integer
{
 t.isArrayType = true ;
 t.eqName = elementTypeRep.eqName ++ printDims (dims) ;
 t.isEqualTo = nameBasedEquality (t, t.isEqualTo_input) ;
 t.errors := [];
 t.pp = t.eqName ;
 t.unparse = "arrayTypeRep (" ++ elementTypeRep.unparse ++ ", " ++ toString (dims) ++ ")" ;
 t.superTypes_ConvertBy := mkArrayCnvtBys (elementTypeRep.superTypes_ConvertBy, dims);

 forwards to primitiveTypeRep(t);
}

-- S <: T <=> S [] <: T [] (Java's type-unsafe array subtyping rule)
function mkArrayCnvtBys
[ConvertBy] ::= cbs::[ConvertBy] dims::Integer
{ 
 return if null(cbs) 
	then []
        else mkConvertBy ("array version of " ++ head (cbs).convertName, idConvertBy, arrayTypeRep (head (cbs).superType, dims)) 
			:: mkArrayCnvtBys (tail (cbs), dims) ;
}

--------------------------------------------------
-- Null Type                                    --
--------------------------------------------------
synthesized attribute amSuperTypeForNullType :: Boolean ;
attribute amSuperTypeForNullType occurs on TypeRep ;


aspect production defaultTypeRep  t::TypeRep ::= forwardingTypeRep::TypeRep
{ t.amSuperTypeForNullType = false ;  }

abstract production nullTypeRep
t::TypeRep ::=
{
 t.isNullType = true ;

 t.eqName = "null" ;
 t.isEqualTo = nameBasedEquality (t, t.isEqualTo_input) ;
 t.errors := [];
 t.pp = "nullTypeRep ()" ;
 t.unparse = "nullTypeRep ()" ;
 forwards to defaultTypeRep(t);


 t.superTypes_ConvertBy := [] ;

 t.isSubTypeResult = mkSubTypeResult ( t.isSubType_ConvertBy ) ;

 t.isSubType_ConvertBy_Accum =
       (if ! t.isSubTypeOf_input.amSuperTypeForNullType  
        then []
        else [ [ mkNamedIdCnvt ("amSuperToNull", t.isSubTypeOf_input ) ] ]
       ) ++
       standard_convertby_check_sofar (t.isSubType_ConvertBy_Accum_currCnvtBy, 
                                       t.isSubTypeOf_input, 
                                       t.isSubType_ConvertBy_Accum_sofar) ;

 t.isSubType_ConvertBy = 
       (if ! t.isSubTypeOf_input.amSuperTypeForNullType  
        then []
        else [ [ mkNamedIdCnvt ("amSuperToNull", t.isSubTypeOf_input ) ] ]
       ) ++
       standard_convertby_check_sofar (mkInitCnvt(t), t.isSubTypeOf_input, [] ) ;
}

--------------------------------------------------
-- Void Type                                 --
--------------------------------------------------

abstract production voidTypeRep
t::TypeRep ::= {
 t.eqName = "void" ;
 t.errors := [];
 t.pp = "voidTypeRep ()" ;
 t.unparse = "voidTypeRep ()";
 t.isEqualTo = nameBasedEquality (t, t.isEqualTo_input) ;
 t.isVoidType = true;

 t.superTypes_ConvertBy := [] ;

 t.isSubTypeResult = mkSubTypeResult ( t.isSubType_ConvertBy ) ;

 t.isSubType_ConvertBy_Accum
  = standard_convertby_check_sofar (t.isSubType_ConvertBy_Accum_currCnvtBy, 
                                    t.isSubTypeOf_input, 
                                    t.isSubType_ConvertBy_Accum_sofar) ;

 t.isSubType_ConvertBy
  = standard_convertby_check_sofar (mkInitCnvt(t), 
                                    t.isSubTypeOf_input, [] ) ;
 forwards to defaultTypeRep (t);
}

--------------------------------------------------
-- Unknown Type                                 --
--------------------------------------------------
abstract production unknownTypeRep
t::TypeRep ::=
{
 t.isUnknownType = true ;

 t.eqName = "unknown" ;
 t.pp = "unknownTypeRep ()" ;

 t.isEqualTo = nameBasedEquality (t, t.isEqualTo_input) ;
 t.errors := [];

 forwards to defaultTypeRep(t);

 t.superTypes_ConvertBy := [] ;

 t.isSubTypeResult = mkSubTypeResult ( t.isSubType_ConvertBy ) ;

 t.isSubType_ConvertBy_Accum
  = standard_convertby_check_sofar (t.isSubType_ConvertBy_Accum_currCnvtBy, 
                                    t.isSubTypeOf_input, 
                                    t.isSubType_ConvertBy_Accum_sofar) ;

 t.isSubType_ConvertBy
  = standard_convertby_check_sofar (mkInitCnvt(t), 
                                    t.isSubTypeOf_input, [] ) ;
 t.classtyperep = unknown_class_type_rep ();
 t.interfacetyperep = unknown_interface_type_rep ();
}


--------------------------------------------------
-- Error Type                                 --
--------------------------------------------------
abstract production errorTypeRep
t::TypeRep ::= errs::[ Error ]
{
 t.eqName = "error" ;
 t.pp = "errorTypeRep ()" ;
 t.errors := errs;

 t.superTypes_ConvertBy := [] ;
 t.isSubTypeResult = mkSubTypeResult ( t.isSubType_ConvertBy ) ;
 t.isSubType_ConvertBy_Accum
  = standard_convertby_check_sofar (t.isSubType_ConvertBy_Accum_currCnvtBy, 
                                    t.isSubTypeOf_input, 
                                    t.isSubType_ConvertBy_Accum_sofar) ;
 t.isSubType_ConvertBy
  = standard_convertby_check_sofar (mkInitCnvt(t), 
                                    t.isSubTypeOf_input, [] ) ;

 forwards to defaultTypeRep(t);
}




--aspect production classTypeRepOldxx t::TypeRep ::= classname extends inters
--{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
--}

aspect production classTypeRep t::TypeRep ::= _
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production interfaceTypeRep t::TypeRep ::= _
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production booleanTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production charTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production byteTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production shortTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production intTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production longTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production floatTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production doubleTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production arrayTypeRep t::TypeRep ::= _ _
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}
aspect production nullTypeRep t::TypeRep ::=   
{ t.superTypes_ConvertBy <- [ mkToUnknownCnvt() ] ;
}

-- One might ask why must unknownTypeRep add these aspects? 
-- Two reasons:
-- 1. Unknown is defined by the host language, so extension writers should know about it.
-- 2. We avoid the use of defaults, which I don't fully trust.
--    Note that the default use of .isReference Type on nullTypeRep is done via 
--    forwarding, the "good" form of defaults.


--------------------------------------------------
-- Default Type Rep                             --
--------------------------------------------------
abstract production defaultTypeRep
t::TypeRep ::= forwardingTypeRep::TypeRep
{
 t.isReferenceType = false ;
 t.isPrimitiveType = false ;
 t.isInterfaceType = false ;
 t.isUnknownType = false ;
 t.isNullType = false ;
 t.isVoidType = false ;

 t.isClassType = false ;
 t.isArrayType = false ;

 t.eqName = "default";
 t.isEqualTo = false ;
 t.errors := [];
 t.pp = forwardingTypeRep.pp  ;
 t.unparse = error ("err: attr unparse not defined on forwarding prod " ++ forwardingTypeRep.pp );

 t.classtyperep = error ("err: attr classtyperep not defined on forwarding prod " ++ forwardingTypeRep.pp );
 t.interfacetyperep = error ("err: attr interfacetyperep not defined on forwarding prod " ++ forwardingTypeRep.pp );
 t.superTypes_ConvertBy := error ("err: attr superTypes_ConvertBy not defined on forwarding prod " ++ forwardingTypeRep.eqName );
 t.isSubTypeResult = error ("err: attr isSubTypeResult not defined on forwarding prod " ++ forwardingTypeRep.eqName );
 t.isSubType_ConvertBy_Accum = error ("err: attr isSubType_ConvertBy_Accum not defined on forwarding prod " ++ forwardingTypeRep.eqName );
 t.isSubType_ConvertBy = error ("err: attr isSubType_ConvertBy not defined on forwarding prod " ++ forwardingTypeRep.eqName );

 t.componentTypeRep = unknownTypeRep() ;
-- t.superTypes_ConvertBy := [] ;
-- t.isSubTypeResult = mkSubTypeResult ( t.isSubType_ConvertBy ) ;
-- t.isSubType_ConvertBy_Accum
--  = standard_convertby_check_sofar (t.isSubType_ConvertBy_Accum_currCnvtBy, 
--                                    t.isSubTypeOf_input, 
--                                    t.isSubType_ConvertBy_Accum_sofar) ;
-- t.isSubType_ConvertBy
--  = standard_convertby_check_sofar (mkInitCnvt(forwardingTypeRep), 
--                                    t.isSubTypeOf_input, [] ) ;
}





function printTypeReps
String ::= typeReps::[ TypeRep ] {
 return if null (typeReps)
	then ""
	else (head (typeReps)).eqName ++ (if null (tail (typeReps)) then "" else ", ") ++ printTypeReps (tail (typeReps));
}

function unparseTypeReps
String ::= typeReps::[ TypeRep ] {
 return "[" ++ unparseTypeRepsHelper (typeReps) ++ "]";
}

function unparseTypeRepsHelper
String ::= typeReps::[ TypeRep ] {
 return if null (typeReps)
	then ""
	else (head (typeReps)).unparse ++ (if null (tail (typeReps)) then "" else ", ") ++ unparseTypeRepsHelper (tail (typeReps));
}
