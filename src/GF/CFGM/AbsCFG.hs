module AbsCFG where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq,Ord,Show)
data Grammars =
   Grammars [Grammar]
  deriving (Eq,Ord,Show)

data Grammar =
   Grammar Ident [Flag] [Rule]
  deriving (Eq,Ord,Show)

data Flag =
   StartCat Category
  deriving (Eq,Ord,Show)

data Rule =
   Rule Ident Name Profile Category [Symbol]
  deriving (Eq,Ord,Show)

data Profile =
   Profile [Ints]
  deriving (Eq,Ord,Show)

data Ints =
   Ints [Integer]
  deriving (Eq,Ord,Show)

data Symbol =
   CatS Category
 | TermS String
  deriving (Eq,Ord,Show)

data Name =
   Name [IdentParam] Category
  deriving (Eq,Ord,Show)

data Category =
   Category IdentParam Ident [Param]
  deriving (Eq,Ord,Show)

data IdentParam =
   IdentParam Ident [Param]
  deriving (Eq,Ord,Show)

data Param =
   Param Ident
  deriving (Eq,Ord,Show)

