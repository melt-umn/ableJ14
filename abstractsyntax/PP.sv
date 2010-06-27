grammar edu:umn:cs:melt:ableJ14:abstractsyntax;

synthesized attribute pp :: String;
synthesized attribute basepp :: String;
autocopy attribute pp_indent :: Integer;

function space
String ::= i::Integer
{
  return if i < 1 then "" else " " ++ space(i-1);
}
