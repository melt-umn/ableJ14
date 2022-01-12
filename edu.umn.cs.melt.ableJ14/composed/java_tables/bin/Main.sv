grammar edu:umn:cs:melt:ableJ14:composed:java_tables:bin;
import edu:umn:cs:melt:ableJ14:composed:java_tables;

function main
IOVal<Integer> ::= args::[ String ] ioIn::IOToken {
  return driver(args, ioIn, parse, parse);
}
