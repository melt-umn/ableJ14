grammar edu:umn:cs:melt:ableJ14:exts:rlp;

import edu:umn:cs:melt:ableJ14:terminals;
import edu:umn:cs:melt:ableJ14:abstractsyntax;
import edu:umn:cs:melt:ableJ14:concretesyntax;


-- RLP Type --
--------------

abstract production rlpTypeRep
t::TypeRep ::= pt::TypeRep 
{
 t.eqName = "rlp<" ++ pt.eqName ++ ">" ;
 t.errors := [];
 t.pp = "rlp<" ++ pt.pp ++ ">" ;
 t.superTypes_ConvertBy := [ ] ;

 t.isPeturbedType = true ; 
 t.componentTypeRep = pt;

 forwards to defaultTypeRep(t);
}


synthesized attribute isPeturbedType :: Boolean ;
attribute isPeturbedType occurs on TypeRep ;


aspect production defaultTypeRep  
t::TypeRep ::= forwardingTypeRep::TypeRep
{ t.isPeturbedType = false ; }


-- Symbolic Type --
-------------------
abstract production symTypeRep
t::TypeRep ::= ct::TypeRep
{
 t.eqName = "symbolic<" ++ ct.eqName ++ ">" ;
 t.errors := [];
 t.pp = "symbolic<" ++ ct.pp ++ ">" ;
 t.superTypes_ConvertBy := [ ] ;

 t.componentTypeRep = ct;

 forwards to defaultTypeRep(t);

 t.isPeturbedType = true ;    
 -- ToDo: why?  can we set this to false?
}
