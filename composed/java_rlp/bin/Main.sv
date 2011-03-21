grammar edu:umn:cs:melt:ableJ14:composed:java_rlp:bin;
import edu:umn:cs:melt:ableJ14:composed:java_rlp;

function main
IOVal<Integer> ::= args::[ String ] ioIn::IO {
  return driver(args, ioIn, parse, parse);
}
