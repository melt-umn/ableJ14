grammar edu:umn:cs:melt:ableJ14:composed:java_pizza:bin;
import edu:umn:cs:melt:ableJ14:composed:java_pizza;

function main
IO ::= args::String ioIn::IO {
  return driver(args, ioIn, parse, parse);
}