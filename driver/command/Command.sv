grammar edu:umn:cs:melt:ableJ14:driver:command ;

import silver:util:command;
import core;

synthesized attribute parseOnly :: Boolean;
attribute parseOnly occurs on PieceList, Command;

aspect production cRootAll
top::Command ::= c1::PieceList
{
  flagLookups <- [flagLookup("--parseonly", false)];

  uses <- ["\t--parseonly: only parse the specified file\n"];

  top.parseOnly = !null(findFlag("--parseonly", top.flags));
}
