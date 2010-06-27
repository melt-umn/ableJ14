grammar edu:umn:cs:melt:ableJ14:composed:java_alone:bin; 

import edu:umn:cs:melt:ableJ14:composed:java_alone;

function main
IO ::= args::String ioIn::IO
{
  return driver(args,ioIn,parse,parse) ;
}
