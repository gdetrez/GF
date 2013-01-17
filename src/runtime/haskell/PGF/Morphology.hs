module PGF.Morphology(Lemma,Analysis,Morpho,
                      buildMorpho,buildMorphoWithMWE,
                      isInMorpho, lookupMorpho,
                      fullFormLexicon, morphoMissing,
                      morphoKnown,morphoClassify,
                      missingWordMsg) where

import PGF.CId
import PGF.Data

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.Array.IArray
import Data.List (intersperse)
import Data.Char (isDigit) ----

-- these 4 definitions depend on the datastructure used

type Lemma = CId
type Analysis = String

newtype Morpho = Morpho (Map.Map String [(Lemma,Analysis)])

buildMorpho :: PGF -> Language -> Morpho
buildMorpho pgf lang =
  maybe (Morpho Map.empty) (collectWords False) (Map.lookup lang (concretes pgf))

-- | This is almoste the same function than previously except that
-- it allows multi word expression (MWE) in the generated Morphology.
-- So if the function ktb_V has the sequence "kick" ++ "the" ++ "bucket",
-- this will only generate one entry in the morphology, associating
--  "kick the bucket" -> ktb_V
-- where the previous function will generate three:
--  "kick" -> ktb_V
--  "the" -> ktb_V
--  "bucket" -> ktb_V
buildMorphoWithMWE :: PGF -> Language -> Morpho
buildMorphoWithMWE pgf lang =
  maybe (Morpho Map.empty) (collectWords True) (Map.lookup lang (concretes pgf))

collectWords :: Bool -> Concr -> Morpho
collectWords mwe pinfo = Morpho $ Map.fromListWith (++)
  [(t, [(fun,lbls ! l)]) | (CncCat s e lbls) <- Map.elems (cnccats pinfo)
                         , fid <- [s..e]
                         , PApply funid _ <- maybe [] Set.toList (IntMap.lookup fid (productions pinfo))
                         , let CncFun fun lins = cncfuns pinfo ! funid
                         , (l,seqid) <- assocs lins
                         , sym <- elems (sequences pinfo ! seqid)
                         , t <- (if mwe then sym2mwe else sym2tokns) sym]
  where
    sym2tokns (SymKS ts)      = ts
    sym2tokns (SymKP ts alts) = ts ++ [t | Alt ts ps <- alts, t <- ts]
    sym2tokns _               = []
    sym2mwe (SymKS ts)      = [unwords ts]
    sym2mwe (SymKP ts alts) = [unwords ts] ++ [unwords ts | Alt ts ps <- alts]
    sym2mwe _               = []

lookupMorpho :: Morpho -> String -> [(Lemma,Analysis)]
lookupMorpho (Morpho mo) s = maybe [] id $ Map.lookup s mo

isInMorpho :: Morpho -> String -> Bool
isInMorpho (Morpho mo) s = maybe False (const True) $ Map.lookup s mo

fullFormLexicon :: Morpho -> [(String,[(Lemma,Analysis)])]
fullFormLexicon (Morpho mo) = Map.toList mo

morphoMissing  :: Morpho -> [String] -> [String]
morphoMissing = morphoClassify False

morphoKnown    :: Morpho -> [String] -> [String]
morphoKnown = morphoClassify True

morphoClassify :: Bool -> Morpho -> [String] -> [String]
morphoClassify k mo ws = [w | w <- ws, k /= null (lookupMorpho mo w), notLiteral w] where
  notLiteral w = not (all isDigit w) ---- should be defined somewhere

missingWordMsg :: Morpho -> [String] -> String
missingWordMsg morpho ws = case morphoMissing morpho ws of
  [] -> ", but all words are known"
  ws -> "; unknown words: " ++ unwords ws

