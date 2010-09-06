grammar edu:umn:cs:melt:ableJ14:exts:sql ;

import edu:umn:cs:melt:ableJ14:terminals ;

lexer class sql ;
lexer class sql_kwd ;

terminal SQL_Id_t  /[a-zA-Z_\$][0-9a-zA-Z_\$]*/ submits to { sql_kwd, java_kwd } ;

