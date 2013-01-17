        {-| Implements the -lttoolbox option to the pg command in the gf shell -}
module GF.Command.PG_lttoolbox where

import PGF.Morphology
import Text.Printf (printf)
import GF.Utils (split,uniq,replace)
import PGF.CId (showCId)

-- | This function export the GF grammar as a dictionaly in the format
-- expected by the lttoolbox tool suite (part of apertium)
-- Here is an example of lttoolbox file:
--   <dictionary>
--     <sdefs>
--       <sdef n="n"/>
--       <sdef n="pl"/>
--       <sdef n="sg"/>
--     </sdefs>
--     <pardefs> 
--       <pardef n="RegNounInfl">
--         <e><p><l/><r><s n="n"/><s n="sg"/></r></p></e>
--         <e><p><l>s</l><r><s n="n"/><s n="pl"/></r></p></e>
--       </pardef>
--     </pardefs>
--     <section id="Root" type="standard">
--       <e lm="cat"><i>cat</i><par n="RegNounInfl"/></e> <!-- A noun -->
--     </section>
--   </dictionary>
prLttoolboxLexicon :: Morpho -> String
prLttoolboxLexicon mo = unlines $ 
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE dictionary SYSTEM \"http://www.apertium.org/dtd/dix.dtd\">"
  , "<dictionary>" ] ++
  [ "<sdefs>" ] ++ map mkSdef tags ++ map mkSdef partOfSpeechs ++ [ "</sdefs>" ] ++
  [ "<section id=\"main\" type=\"standard\">" ] ++ entries ++ [ "</section>" ] ++
  [ "</dictionary>" ]
 where
  tags :: [String]
  tags = uniq $ do
    (_,lps) <- fullFormLexicon mo
    (lemma,p) <- lps
    cleanTags p
  partOfSpeechs :: [String]
  partOfSpeechs = uniq $ do
    (_,lps) <- fullFormLexicon mo
    (lemma,_) <- lps
    getPartOfSpeech (showCId lemma)
  mkSdef :: String -> String -- creates a XML tag like <sdef n="n"/>
  mkSdef = printf "<sdef n=\"%s\"/>" . cdata

  mkEntry :: String -> String -> [String] -> String
    -- creates a XML tag like <e><l>cats</l><r>cat_N<s n="Pl"></r></e>
  mkEntry form lemma tags = 
    printf "<e><p><l>%s</l><r>%s%s</r></p></e>" (cdata form) (cdata lemma) 
           (concatMap (printf "<s n=\"%s\"/>" . cdata) tags::String)
  entries :: [String]
  entries = do
    (form,lps) <- fullFormLexicon mo
    (lemma,analysis) <- lps
    let pos = getPartOfSpeech (showCId lemma)
    let tags = pos ++ cleanTags analysis
    return $ mkEntry form (showCId lemma) tags

  -- Sanitize a string to be used as cdata in XML
  cdata :: String -> String
  cdata = replace "&" "&amp;"
  
  -- cleanTags takes a GF analysis and convert it in a list of tags
  -- by spliting it and removing the parentheses
  cleanTags :: String -> [String]
  cleanTags s = do
    tag <- words s
    let clean_tag = replace "(" "" $ replace ")" "" $ tag
    case clean_tag of
      "s" -> fail "s is not an interesting tag"
      t -> return t

-- This expects the lemma to be in the form xxxxxxxxx_PoS 
-- (where PoS is the part of speech.) If so, it returns the later.
getPartOfSpeech :: (Monad m) => String -> m String
getPartOfSpeech s = do
  case split (reverse s) '_' of
    a:b:_ -> if length a > 0 then return (reverse a) else fail []
    _ -> fail []
