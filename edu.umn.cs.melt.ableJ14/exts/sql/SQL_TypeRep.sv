grammar edu:umn:cs:melt:ableJ14:exts:sql ;
import edu:umn:cs:melt:ableJ14:abstractsyntax ;

nonterminal SQLTypeRep;

synthesized attribute isInteger :: Boolean occurs on SQLTypeRep;
synthesized attribute isFloat :: Boolean occurs on SQLTypeRep;
synthesized attribute isString :: Boolean occurs on SQLTypeRep;
synthesized attribute isBoolean :: Boolean occurs on SQLTypeRep;


function sqlInteger
Decorated SQLTypeRep ::= {
  return decorate i_sqlInteger() with {};
}
abstract production i_sqlInteger
top::SQLTypeRep ::= {
  top.isInteger = true;
  forwards to i_sqlDefault();
}

function sqlFloat
Decorated SQLTypeRep ::= {
  return decorate i_sqlFloat() with {};
}
abstract production i_sqlFloat
top::SQLTypeRep ::= {
  top.isFloat = true;
  forwards to i_sqlDefault();
}

function sqlString
Decorated SQLTypeRep ::= {
  return decorate i_sqlString() with {};
}
abstract production i_sqlString
top::SQLTypeRep ::= {
  top.isString = true;
  forwards to i_sqlDefault();
}

function sqlBoolean
Decorated SQLTypeRep ::= {
  return decorate i_sqlBoolean() with {};
}
abstract production i_sqlBoolean
top::SQLTypeRep ::= {
  top.isBoolean = true;
  forwards to i_sqlDefault();
}

function sqlDefault
Decorated SQLTypeRep ::= {
  return decorate i_sqlDefault() with {};
}
abstract production i_sqlDefault
top::SQLTypeRep ::= {
  top.isInteger = false;
  top.isFloat = false;
  top.isString = false;
  top.isBoolean = false; 
}

function getSQLTypeRep
Decorated SQLTypeRep ::= javaTypeRep::TypeRep {
  return case javaTypeRep of
		byteTypeRep () -> sqlInteger () |
		charTypeRep () -> sqlInteger () |
		shortTypeRep () -> sqlInteger () |
		intTypeRep () -> sqlInteger () |
		longTypeRep () -> sqlInteger () |
		floatTypeRep () -> sqlFloat () |
		doubleTypeRep () -> sqlFloat () |
		classTypeRep (string_class_type_rep (_, _, _, _, _, _, _, _, _, _)) -> sqlString () |
		booleanTypeRep () -> sqlBoolean () |
		_ -> sqlDefault ()
	 end;
}
