grammar edu:umn:cs:melt:ableJ14:composed:java_alone; 

-- Collect the host language and desired language extensions
-- into one grammar by exporting them.
exports edu:umn:cs:melt:ableJ14:host; 

-- Also, import the host language start nonterminal and
-- create a parser for the extended language.
imports edu:umn:cs:melt:ableJ14:host only Root_C; 

parser parse :: Root_C {
 edu:umn:cs:melt:ableJ14:composed:java_alone;
} 
