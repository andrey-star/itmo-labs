grammar Python2C;

/** Program */
program  returns [String value] @init {$value = "";}
    : programParts {$value += $programParts.fDefs + "\n" + $programParts.fs + "int main() {\n" + StringUtils.tabbed($programParts.main + "return 0;", 1) + "\n}\n";}
    ;

programParts returns [String fDefs, String fs, String main] @init {$fDefs = ""; $fs = ""; $main = "";}
    : (assignment LF+ {$main += $assignment.value + "\n";}
       | ifExp {$main += $ifExp.value;}
       | print LF+ {$main += $print.value + "\n";}
       | intFunction {$fDefs += $intFunction.def + ";\n"; $fs += $intFunction.value + "\n";}
      )*
    ;

/** Functions */
intFunction returns [String def, String value] @init {$def = ""; $value = "";}
    : DEF {$value += "int ";}
      IDENT {$value += $IDENT.text;}
      LPAREN {$value += "(";}
      (IDENT {$value += "int " + $IDENT.text;} (COMMA IDENT {$value += ", int " + $IDENT.text;})*)?
      RPAREN {$value += ")";}
      COLON LF+
      intFunctionBody {$def = $value; $value += " {\n" + $intFunctionBody.value + "}\n";}
    ;

intFunctionBody returns [String value] @init {$value = "";}
    : ((tabbedExp1 LF+ {$value += $tabbedExp1.value + "\n";}) | ifExpF {$value += $ifExpF.value;})* LEVEL1 ret LF+ {$value += "\t" + $ret.value + "\n";}
    ;

tabbedExp1 returns [String value] @init {$value = "";}
	: LEVEL1 assignment {$value += StringUtils.tabbed($assignment.value, 1);} | LEVEL1 print {$value += "\t" + $print.value;}
	;

tabbedExp2 returns [String value] @init {$value = "";}
	: LEVEL2 assignment {$value += StringUtils.tabbed($assignment.value, 2);} | LEVEL2 print {$value += "\t" + $print.value;}
	;

ret returns [String value]
    : RET exp {$value = "return " + $exp.value + ";";}
    ;

/** Function call */
print returns [String value]
    : PRINT LPAREN exp RPAREN {$value = "printf(\"%d\\n\", " + $exp.value + ");";}
    ;

intFunctionCall returns [String value] @init {$value = "";}
    : IDENT {$value += $IDENT.text;}
      LPAREN {$value += "(";}
      (exp {$value += $exp.value;} (COMMA exp {$value += ", " + $exp.value;})*)?
      RPAREN {$value += ")";}
    ;

/** If */
ifCond returns [String value] @init {$value = "";}
	: IF {$value += $IF.text + " ";}
      cond {$value += "(" + $cond.value + ")";}
      COLON
    ;

ifExp returns [String value] @init {$value = "";}
	: ifCond LF+ {$value += $ifCond.value + " {\n";}
	  (tabbedExp1 LF+ {$value += $tabbedExp1.value + "\n";})* {$value += "}\n";}
	;

ifExpF returns [String value] @init {$value = "";}
	: LEVEL1 ifCond LF+ {$value += "\t" + $ifCond.value + " {\n";}
	  (tabbedExp2 LF+ {$value += $tabbedExp2.value + "\n";})* {$value += "\t}\n";}
	  (LEVEL2 ret LF+ {$value += "\t" + $ret.value + "\n";})?
	;

/** Variable assignment */
assignment returns [String value] @init {$value = "";}
    : IDENT ASSIGN exp {$value = "int " + $IDENT.text + " = " + $exp.value + ";";}
    | IDENT ASSIGN READ_INT {$value = "int " + $IDENT.text + ";\nscanf(\"%d\", &" + $IDENT.text + ");";}
    ;

/** Conditions */
cond returns [String value] @init {$value = "";}
	: exp {$value += $exp.value;} cmp {$value += " " + $cmp.value + " ";} exp {$value += $exp.value;}
	;

cmp returns [String value]
	: LT {$value = $LT.text;}
	| GT {$value = $GT.text;}
	| LE {$value = $LE.text;}
	| GE {$value = $GE.text;}
	| EQ {$value = $EQ.text;}
	;

/** Arithmetic expressions **/
exp returns [String value] @init {$value = "";}
    : term {$value += $term.value;}
      ((PLUS {$value += " " + $PLUS.text + " ";} | MINUS {$value += " " + $MINUS.text + " ";})
       term {$value += $term.value;})*
    ;

term returns [String value] @init {$value = "";}
    : primary {$value += $primary.value;}
      ((STAR {$value += " " + $STAR.text + " ";} | SLASH  {$value += " " + $SLASH.text + " ";})
       primary {$value += $primary.value;})*
    ;

primary returns [String value] @init {$value = "";}
    : INT {$value = $INT.text;}
    | intFunctionCall {$value = $intFunctionCall.value;}
    | (PLUS {$value += $PLUS.text;} | MINUS {$value += $MINUS.text;})? IDENT {$value += $IDENT.text;}
    | (PLUS {$value += $PLUS.text;} | MINUS {$value += $MINUS.text;})? LPAREN exp RPAREN {$value += "(" + $exp.value + ")";}
    ;

/** Tokens */
READ_INT : 'int(input())' ;
PRINT : 'print' ;
DEF : 'def' ;
RET : 'return' ;
IF : 'if' ;
LT : '<' ;
GT : '>' ;
LE : '<=' ;
GE : '>=' ;
EQ : '==' ;
ASSIGN : '=' ;
PLUS : '+' ;
MINUS : '-' ;
STAR : '*' ;
SLASH : '/' ;
LPAREN : '(' ;
RPAREN : ')' ;
COMMA : ',' ;
COLON : ':' ;
LEVEL1 : '\t' ;
LEVEL2 : '\t\t' ;
LF : '\n' ;
IDENT : [a-zA-Z_][a-zA-Z_0-9]* ; // Identifier consists of letters/underscores/digits, can't start with a digit
INT : (PLUS | MINUS)?[0-9]+ ; // Define int as one or more digits with optional leading '+'/'-'

WS : [ \t\r]+ -> skip ; // Define whitespace rule, toss it out
