lexer grammar MetaLexer;

GRAMMAR : 'grammar' ;
IMPORT : '@import' -> pushMode(IMPORT_BLOCK) ;
MACROS : '@macros' -> pushMode(MACROS_BLOCK) ;
LEXER : '@lexer' -> pushMode(LEXER_BLOCK) ;
PARSER : '@parser' -> pushMode(PARSER_BLOCK) ;
START : '@start' -> pushMode(START_BLOCK) ;
SEMICOLON : ';' ;
IDENT : [a-zA-Z_][a-zA-Z0-9_]* ;
WS : [ \t\r\n]+ -> skip ;

mode PARSER_MODE;
RETURNS : 'returns' ;
EPS : '_EPS' ;
LPAREN : '(' ;
RPAREN : ')' ;
LSQUARE : '[' ;
RSQUARE : ']' ;
LCURLY : '{' -> pushMode(CODE_0) ;
RCURLY : '}' -> popMode, popMode ;
COMMA : ',' ;
COLON : ':' ;
PARSER_SEMICOLON : ';' -> type(SEMICOLON) ;
STICK : '|' ;
PARSER_IDENT : IDENT -> type(IDENT) ;
ARG : IDENT '.' (~[,)])+ ;
PARSER_WS : WS -> skip ;

mode CODE_0;

CODE_0_LCURLY: '{' -> type(OTHER), pushMode(CODE_N) ;
CODE_RCURLY: '}' -> type(RCURLY), popMode ;
CODE_0_OTHER: ~[{}]+ -> type(OTHER) ;

mode CODE_N;

CODE_N_LCURLY: '{' -> type(OTHER), pushMode(CODE_N) ;
CODE_N_RCURLY: '}' -> type(OTHER), popMode ;
OTHER: ~[{}]+;


mode PARSER_BLOCK;
PARSER_LCURLY: '{' -> type(LCURLY), pushMode(PARSER_MODE) ;
PARSER_WS2 : WS -> skip ;

mode LEXER_BLOCK;
LEXER_LCURLY : LCURLY -> type(LCURLY) ;
LEXER_IDENT : IDENT -> type(IDENT) ;
LEXER_COLON : COLON -> type(COLON) ;
LEXER_SEMICOLON : SEMICOLON -> type(SEMICOLON) ;
REGEX : '"'(~["])+'"' ;
LEXER_RCURLY : RCURLY -> type(RCURLY), popMode ;
LEXER_WS : WS -> skip ;

mode START_BLOCK;
START_IDENT : IDENT -> type(IDENT) ;
START_COLON : COLON -> type(COLON) ;
START_SEMICOLON : SEMICOLON -> type(SEMICOLON) ;
START_WS : WS -> skip ;

mode IMPORT_BLOCK;
IMPORTS : '{' (~[}])* '}' -> popMode ;
IMPORT_WS : WS -> skip ;

mode MACROS_BLOCK;
MACROS2_LCURLY : '{' -> type(LCURLY), pushMode(CODE_1) ;
MACROS_RCURLY : '}' -> type(RCURLY), popMode ;
MACROS_WS : WS -> skip ;

mode CODE_1;

CODE_1_LCURLY: '{' -> type(OTHER), pushMode(CODE_N1) ;
CODE_1_RCURLY: '}' -> type(RCURLY), popMode, popMode ;
CODE_1_OTHER: ~[{}]+ -> type(OTHER) ;

mode CODE_N1;

CODE_N1_LCURLY: '{' -> type(OTHER), pushMode(CODE_N1) ;
CODE_N1_RCURLY: '}' -> type(OTHER), popMode ;
OTHER1: ~[{}]+ -> type(OTHER);