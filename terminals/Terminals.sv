grammar edu:umn:cs:melt:ableJ14:terminals;

lexer class java_kwd ;

ignore terminal LineComment /[\/][\/].*/ ;
ignore terminal BlockComment /[\/][\*]([^\*]|[\r\n]|([\*]+([^\*\/]|[\r\n])))*[\*]+[\/]/ ;
ignore terminal tabs /[\t]+/;
ignore terminal spaces /[\ ]+/;
ignore terminal newlines /[\n]+/;
--ignore terminal whitespace /[\n\t\ ]+/ ;


terminal Id_t           /[a-zA-Z_\$][0-9a-zA-Z_\$]*/ submits to { java_kwd } ;

terminal Charconst_t       /[\'][^\'\\][\']|[\'][\\][btnfr\"\'\\][\']|[\'][\\][0-7][\']|[\'][\\][0-7][0-7][\']|[\'][\\][0-3][0-7][0-7][\']|[\'][\\][u]+[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?[\']/  ;
terminal Intconst_t      /([0-9]+|0[xX][0-9a-fA-F]+)[lL]?/ ;
terminal Floatconst_t   /((([0-9]+[\.][0-9]*|[\.][0-9]+)([eE][\-\+]?[0-9]+)?|[0-9]+[eE][\-\+]?[0-9]+)[fFdD]?)|([0-9]+([eE][\-\+]?[0-9]+)?[fFdD])/  ;
terminal Stringconst_t  /[\"]([^\"\\]|[\\][btnfr\"\'\\]|[\\][0-7]|[\\][0-7][0-7]|[\\][0-3][0-7][0-7]|[\\][u]+[0-9a-fA-F][0-9a-fA-F]?[0-9a-fA-F]?[0-9a-fA-F]?)*[\"]/ ;

terminal True_t         'true'       lexer classes { java_kwd } ;
terminal False_t        'false'      lexer classes { java_kwd } ;
terminal Null_t         'null'       lexer classes { java_kwd } ;

terminal While_t        'while'      lexer classes { java_kwd } ;
terminal For_t          'for'        lexer classes { java_kwd } ;
terminal If_t           'if'         lexer classes { java_kwd } ;
terminal Else_t         'else'       lexer classes { java_kwd } , precedence = 160;
terminal Do_t           'do'         lexer classes { java_kwd } ;
terminal Switch_t       'switch'     lexer classes { java_kwd } ;
terminal Case_t         'case'       lexer classes { java_kwd } ;
terminal Default_t      'default'    lexer classes { java_kwd } ;
terminal Break_t        'break'      lexer classes { java_kwd } ;
terminal Continue_t     'continue'   lexer classes { java_kwd } ;
terminal Assert_t       'assert'     lexer classes { java_kwd } ;

terminal Package_t      'package'    lexer classes { java_kwd } ;
terminal Import_t       'import'     lexer classes { java_kwd } ;

terminal Class_t        'class'      lexer classes { java_kwd } ;
terminal Extends_t      'extends'    lexer classes { java_kwd } ;
terminal Interface_t    'interface'  lexer classes { java_kwd } ;
terminal Implements_t   'implements' lexer classes { java_kwd } ;
terminal This_t         'this'       lexer classes { java_kwd } ;
terminal Super_t        'super'      lexer classes { java_kwd } ;
terminal Return_t       'return'     lexer classes { java_kwd } ;
terminal Throw_t        'throw'      lexer classes { java_kwd } ;
terminal Throws_t       'throws'     lexer classes { java_kwd } ;
terminal Try_t          'try'        lexer classes { java_kwd } ;
terminal Finally_t      'finally'    lexer classes { java_kwd } ;
terminal Catch_t        'catch'      lexer classes { java_kwd } ;

terminal Public_t       'public'     lexer classes { java_kwd } ;
terminal Protected_t    'protected'  lexer classes { java_kwd } ;
terminal Private_t      'private'    lexer classes { java_kwd } ;
terminal Static_t       'static'     lexer classes { java_kwd } ;
terminal Abstract_t     'abstract'   lexer classes { java_kwd } ;
terminal Final_t        'final'      lexer classes { java_kwd } ;
terminal Native_t       'native'     lexer classes { java_kwd } ;
terminal Synchronized_t 'synchronized' lexer classes { java_kwd } ;
terminal Transient_t    'transient'  lexer classes { java_kwd } ;
terminal Volatile_t     'volatile'   lexer classes { java_kwd } ;
terminal Strictfp_t     'strictfp'   lexer classes { java_kwd } ;

terminal Byte_t         'byte'       lexer classes { java_kwd } ;
terminal Short_t        'short'      lexer classes { java_kwd } ;
terminal Int_t          'int'        lexer classes { java_kwd } ;
terminal Long_t         'long'       lexer classes { java_kwd } ;
terminal Char_t         'char'       lexer classes { java_kwd } ;
terminal Float_t        'float'      lexer classes { java_kwd } ;
terminal Double_t       'double'     lexer classes { java_kwd } ;
terminal Boolean_t      'boolean'    lexer classes { java_kwd } ;
terminal Void_t         'void'       lexer classes { java_kwd } ;

terminal Const_t        'const'      lexer classes { java_kwd } ;
terminal Goto_t         'goto'       lexer classes { java_kwd } ;

terminal Lbrace_t       '{'           ;
terminal Rbrace_t       '}'           ;

terminal Semicolon_t    ';'           ;
terminal Comma_t        ','           ;

--------------------------------

terminal Eq_t       '='	precedence = 10,	association = right ;

--------------------------------

terminal MulEq_t    '*='	precedence = 10,	association = right ;
terminal DivEq_t    '/='	precedence = 10,	association = right ;

terminal ModEq_t    '%='	precedence = 10,	association = right ;
terminal PlusEq_t   '+='	precedence = 10,	association = right ;
terminal MinusEq_t  '-='	precedence = 10,	association = right ;
terminal LshiftEq_t '<<='	precedence = 10,	association = right ;
terminal RshiftEq_t '>>='	precedence = 10,	association = right ;
terminal UrshiftEq_t '>>>='	precedence = 10,	association = right ;
terminal AndEq_t    '&='	precedence = 10,	association = right ;
terminal XorEq_t    '^='	precedence = 10,	association = right ;
terminal OrEq_t     '|='	precedence = 10,	association = right ;


--------------------------------

terminal Question_t     '?'	precedence = 20,	association = right ;
terminal Colon_t        ':'	precedence = 20,	association = right ;

--------------------------------

terminal OrOr_t         '||'	precedence = 30,	association = left ;

terminal AndAnd_t       '&&'	precedence = 40,	association = left ;

terminal Or_t           '|'	precedence = 50,	association = left ;

terminal Xor_t          '^'	precedence = 60,	association = left ;

terminal And_t          '&'	precedence = 70,	association = left ;

--------------------------------

terminal EqEq_t         '=='	precedence = 80,	association = left ;
terminal NotEq_t        '!='	precedence = 80,	association = left ;

--------------------------------

terminal Lt_t           '<'	precedence = 90,	association = left ;
terminal Gt_t           '>'	precedence = 90,	association = left ;
terminal LtEq_t         '<='	precedence = 90,	association = left ;
terminal GtEq_t         '>='	precedence = 90,	association = left ;

--------------------------------

terminal InstanceOf_t   'instanceof'	precedence = 90,	association = left, lexer classes { java_kwd } ;

--------------------------------

terminal Lshift_t       '<<'	precedence = 100,	association = left ;
terminal Rshift_t       '>>'	precedence = 100,	association = left ;
terminal Urshift_t      '>>>'	precedence = 100,	association = left ;

--------------------------------

terminal Plus_t         '+'	precedence = 120,	association = left ;
terminal Minus_t        '-'	precedence = 120,	association = left ;

terminal Mul_t          '*'	precedence = 130,	association = left ;
terminal Div_t          '/'	precedence = 130,	association = left ;
terminal Mod_t          '%'	precedence = 130,	association = left ;

--------------------------------

terminal Comp_t         '~'	precedence = 140,	association = right ;
terminal Not_t          '!'	precedence = 140,	association = right ;
terminal New_t   	'new'	precedence = 140,	association = right, lexer classes { java_kwd } ;

--------------------------------

terminal PlusPlus_t     '++'	precedence = 140,	association = right ;
terminal MinusMinus_t   '--'	precedence = 140,	association = right ;

--------------------------------

terminal Lparen_t       '('	precedence = 140,	association = right ;
terminal Rparen_t       ')'	precedence = 140,	association = right ;

--------------------------------

terminal Lbrack_t       '['	precedence = 150,	association = left ;
terminal Rbrack_t       ']'	precedence = 150,	association = left ;

--------------------------------

terminal Dot_t          '.'	precedence = 150,	association = left ;
