grammar edu:umn:cs:melt:ableJ14:composed:java_autoboxing:bin;
import edu:umn:cs:melt:ableJ14:composed:java_autoboxing;

function main
IO ::= args::String ioIn::IO {
  return driver(args, ioIn, parse, parse);
}
