parser grammar MetaParser;

@header {
	import com.andreystar.grammar.*;
}

options { tokenVocab=MetaLexer; }

grammarDef returns [Grammar grammar] locals [String importsVal, String macrosVal]
  : grammarName (imports {$importsVal = $imports.value;})? (macros {$macrosVal = $macros.value;})? lexerRules parserRules startRule
    {$grammar = new Grammar($grammarName.value, $importsVal, $macrosVal, $lexerRules.value, $parserRules.value, $startRule.start, $startRule.retAttr);}
  ;

grammarName returns [String value]
  : GRAMMAR IDENT SEMICOLON {$value = $IDENT.text;}
  ;


// Imports
imports returns [String value]
  : IMPORT IMPORTS{$value = $IMPORTS.text;}
  ;

// Macros
macros returns [String value]
  : MACROS code{$value = $code.value;}
  ;

// Lexer
lexerRules returns [List<LexerRule> value] @init {$value = new ArrayList<>();}
  : LEXER LCURLY (myLexerRule {$value.add($myLexerRule.value);})+ RCURLY
  ;

myLexerRule returns [LexerRule value]
  : name=IDENT COLON regex=REGEX SEMICOLON {$value = new LexerRule($name.text, $regex.text);}
  ;


// Parser
parserRules returns [List<ParserRule> value] @init {$value = new ArrayList<>();}
  : PARSER LCURLY (myParserRule {$value.add($myParserRule.value);})+ RCURLY
  ;

myParserRule returns [ParserRule value]
  : name=IDENT LPAREN params=typedNames RPAREN RETURNS LSQUARE returnsVal=typedNames RSQUARE COLON rhss=ruleRhss SEMICOLON
    {$value = new ParserRule($name.text, $params.value, $returnsVal.value, $rhss.value);}
  ;

typedNames returns [List<TypedName> value] @init {$value = new ArrayList<>();}
  : (typedName {$value.add($typedName.value);} (COMMA typedName {$value.add($typedName.value);})*)?
  ;

typedName returns [TypedName value]
  : type=IDENT nameVal=IDENT {$value = new TypedName($type.text, $nameVal.text);}
  ;

ruleRhss returns [List<RuleRhs> value] @init {$value = new ArrayList<>();}
  : ruleRhs {$value.add($ruleRhs.value);} (STICK ruleRhs {$value.add($ruleRhs.value);})*
  ;

ruleRhs returns [RuleRhs value]
        locals [List<RhsAtom> atoms]
        @init {$atoms = new ArrayList<>();}
        @after {$value = new RuleRhs($atoms);}
  : (rhsAtom {$atoms.add($rhsAtom.value);})+
  ;

rhsAtom returns [RhsAtom value]
  : rhsNonTerminal {$value = $rhsNonTerminal.value;}
  | rhsTerminal {$value = $rhsTerminal.value;}
  | rhsEps {$value = $rhsEps.value;}
  ;

rhsEps returns [RhsEpsilon value]
  : EPS code? {$value = new RhsEpsilon($code.text);}
  ;

rhsTerminal returns [RhsTerminal value]
  : name=IDENT code? {$value = new RhsTerminal($name.text, $code.text);}
  ;

rhsNonTerminal returns [RhsNonTerminal value]
  : name=IDENT LPAREN argsVal=args RPAREN code? {$value = new RhsNonTerminal($name.text, $argsVal.value, $code.text);}
  ;

args returns [List<String> value] @init {$value = new ArrayList<>();}
  : (ARG {$value.add($ARG.text);} (COMMA ARG {$value.add($ARG.text);})*)?
  ;

startRule returns [String start, String retAttr]
  : START COLON startVal=IDENT retAttrVal=IDENT SEMICOLON {$start = $startVal.text; $retAttr = $retAttrVal.text;}
  ;

code returns [String value] @init {$value = "";}
  : LCURLY (OTHER {$value += $OTHER.text;})* RCURLY
  ;
