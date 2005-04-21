module GF.CFGM.PrintCFG where

-- pretty-printer generated by the BNF converter

import GF.CFGM.AbsCFG
import Data.Char

-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)
  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString i)


instance Print SingleQuoteString where
  prt _ (SingleQuoteString i) = doc (showString i)



instance Print Grammars where
  prt i e = case e of
   Grammars grammars -> prPrec i 0 (concatD [prt 0 grammars])


instance Print Grammar where
  prt i e = case e of
   Grammar id flags rules -> prPrec i 0 (concatD [doc (showString "grammar") , prt 0 id , prt 0 flags , prt 0 rules , doc (showString "end") , doc (showString "grammar")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Flag where
  prt i e = case e of
   StartCat category -> prPrec i 0 (concatD [doc (showString "startcat") , prt 0 category])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print Rule where
  prt i e = case e of
   Rule fun profile category symbols -> prPrec i 0 (concatD [prt 0 fun , doc (showString ":") , prt 0 profile , doc (showString ".") , prt 0 category , doc (showString "->") , prt 0 symbols])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , doc (showString ";") , prt 0 xs])

instance Print Fun where
  prt i e = case e of
   Cons id -> prPrec i 0 (concatD [prt 0 id])
   Coerce  -> prPrec i 0 (concatD [doc (showString "_")])


instance Print Profile where
  prt i e = case e of
   Profile intss -> prPrec i 0 (concatD [doc (showString "[") , prt 0 intss , doc (showString "]")])


instance Print Ints where
  prt i e = case e of
   Ints ns -> prPrec i 0 (concatD [doc (showString "[") , prt 0 ns , doc (showString "]")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Symbol where
  prt i e = case e of
   CatS category -> prPrec i 0 (concatD [prt 0 category])
   TermS str -> prPrec i 0 (concatD [prt 0 str])

  prtList es = case es of
   [] -> (concatD [doc (showString ".")])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print Category where
  prt i e = case e of
   Category singlequotestring -> prPrec i 0 (concatD [prt 0 singlequotestring])



