--# -path=.:../abstract:../common:prelude

concrete GrammarGer of Grammar = 
  NounGer, 
  VerbGer, 
  AdjectiveGer,
  AdverbGer,
  NumeralGer,
  SentenceGer,
  QuestionGer,
  RelativeGer,
  ConjunctionGer,
  PhraseGer,
  TextX - [Tense,TPres,TPast,TFut,TCond,TTAnt,Temp],
  IdiomGer,
  StructuralGer
  ** {

flags startcat = Phr ; unlexer = text ; lexer = text ;

} ;