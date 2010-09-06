grammar edu:umn:cs:melt:ableJ14:composed:java_complex:bin;
import edu:umn:cs:melt:ableJ14:composed:java_complex;

function main
IO ::= args::String ioIn::IO {
  return driver(args, ioIn, parse, parse);
}
