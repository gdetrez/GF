-- | Translate to the core language
module Transfer.SyntaxToCore where

import Transfer.Syntax.Abs
import Transfer.Syntax.Print

import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

import Debug.Trace

type C a = State CState a

type CState = Integer



declsToCore :: [Decl] -> [Decl]
declsToCore m = evalState (declsToCore_ m) newState

declsToCore_ :: [Decl] -> C [Decl]
declsToCore_ =     deriveDecls
               >>> replaceCons 
               >>> compilePattDecls 
               >>> desugar 
               >>> optimize

optimize :: [Decl] -> C [Decl]
optimize =     removeUselessMatch
           >>> betaReduce

newState :: CState
newState = 0

--
-- * Pattern equations
--

compilePattDecls :: [Decl] -> C [Decl]
compilePattDecls [] = return []
compilePattDecls (d@(ValueDecl x _ _):ds) =
    do
    let (xs,rest) = span (isValueDecl x) ds
    d <- mergeDecls (d:xs)
    rs <- compilePattDecls rest
    return (d:rs)
compilePattDecls (d:ds) = liftM (d:) (compilePattDecls ds)

-- | Take a non-empty list of pattern equations for the same
--   function, and produce a single declaration.
mergeDecls :: [Decl] -> C Decl
mergeDecls ds@(ValueDecl x p _:_)
    = do let cs = [ (ps,rhs) | ValueDecl _ ps rhs <- ds ]
             (pss,rhss) = unzip cs
             n = length p
         when (not (all ((== n) . length) pss))
              $ fail $ "Pattern count mismatch for " ++ printTree x
         vs <- freshIdents n
         let cases = map (\ (ps,rhs) -> Case (mkPRec ps) rhs) cs
             c = ECase (mkERec (map EVar vs)) cases
             f = foldr (EAbs . VVar) c vs
         return $ ValueDecl x [] f
  where mkRec r f = r . zipWith (\i e -> f (Ident ("p"++show i)) e) [0..]
        mkPRec = mkRec PRec FieldPattern
        mkERec xs | null xs = EEmptyRec
                  | otherwise = mkRec ERec FieldValue xs

--
-- * Derived function definitions
--

deriveDecls :: [Decl] -> C [Decl]
deriveDecls ds = liftM concat (mapM der ds)
  where 
  ts = dataTypes ds
  der (DeriveDecl (Ident f) t) = 
      case lookup f derivators of
           Just d -> d t k cs
           _ -> fail $ "Don't know how to derive " ++ f
      where (k,cs) = getDataType ts t
  der d                = return [d]

type Derivator = Ident -> Exp -> [(Ident,Exp)] -> C [Decl]

derivators :: [(String, Derivator)]
derivators = [
              ("composOp", deriveComposOp),
              ("show", deriveShow),
              ("eq", deriveEq),
              ("ord", deriveOrd)
             ]

deriveComposOp :: Derivator
deriveComposOp t k cs = 
    do
    f <- freshIdent
    x <- freshIdent
    let co = Ident ("composOp_" ++ printTree t)
        e = EVar
        pv = VVar
        infixr 3 -->
        (-->) = EPiNoVar
        infixr 3 \->
        (\->) = EAbs
        mkCase ci ct = 
            do
            vars <- freshIdents (arity ct)
            -- FIXME: the type argument to f is wrong if the constructor
            -- has a dependent type
            -- FIXME: make a special case for lists?
            let rec v at = case at of
                                   EApp (EVar t') _ | t' == t -> apply (e f) [at, e v]
                                   _ -> e v
                calls = zipWith rec vars (argumentTypes ct)
            return $ Case (PCons ci (map PVar vars)) (apply (e ci) calls)
    ift <- abstractType (argumentTypes k) (\vs -> 
             let tc = apply (EVar t) vs in tc --> tc)
    ft <- abstractType (argumentTypes k) (\vs -> 
             let tc = apply (EVar t) vs in ift --> tc --> tc)
    cases <- mapM (uncurry mkCase) cs
    let cases' = cases ++ [Case PWild (e x)]
    fb <- abstract (arity k) $ const $ pv f \-> pv x \-> ECase (e x) cases'
    return $ [TypeDecl co ft,
              ValueDecl co [] fb]

deriveShow :: Derivator
deriveShow t k cs = fail $ "derive show not implemented"

deriveEq :: Derivator
deriveEq t k cs = fail $ "derive eq not implemented"

deriveOrd :: Derivator
deriveOrd t k cs = fail $ "derive ord not implemented"

--
-- * Constructor patterns and applications.
--

type DataConsInfo = Map Ident Int

consArities :: [Decl] -> DataConsInfo
consArities ds = Map.fromList [ (c, arity t) | DataDecl _ _ cs <- ds, 
                                               ConsDecl c t <- cs ]

-- | Get the arity of a function type.
arity :: Exp -> Int
arity = length . argumentTypes

-- | Get the argument type of a function type. Note that
--   the returned types may contains free variables 
--   which should be bound to the values of earlier arguments.
argumentTypes :: Exp -> [Exp]
argumentTypes e = case e of
                      EPi _ t e' -> t : argumentTypes e'
                      EPiNoVar t e' -> t : argumentTypes e'
                      _ -> []

-- | Fix up constructor patterns and applications.
replaceCons :: [Decl] -> C [Decl]
replaceCons ds = mapM f ds
  where
  cs = consArities ds 
  isCons id = id `Map.member` cs 
  f :: Tree a -> C (Tree a)
  f t = case t of
        -- get rid of the PConsTop hack
        PConsTop id p1 ps -> f (PCons id (p1:ps))
        -- replace patterns C where C is a constructor with (C)
        PVar id | isCons id -> return $ PCons id []
        -- eta-expand constructors. betaReduce will remove any beta 
        -- redexes produced here.
        EVar id | isCons id -> do
                               let Just n = Map.lookup id cs
                               vs <- freshIdents n
                               let c = apply t (map EVar vs)
                               return $ foldr (EAbs . VVar) c vs
        _ -> composOpM f t

--
-- * Do simple beta reductions.
--

betaReduce :: [Decl] -> C [Decl]
betaReduce = return . map f
 where
 f :: Tree a -> Tree a
 f t = case t of
              EApp (EAbs (VVar x) b) e | countFreeOccur x b == 1 -> f (subst x e b)
              _ -> composOp f t

--
-- * Remove useless pattern matching.
--

removeUselessMatch :: [Decl] -> C [Decl]
removeUselessMatch = return . map f
 where
 f :: Tree a -> Tree a
 f x = case x of
       -- replace \x -> case x of { y -> e } with \y -> e,
       -- if x is not free in e
       -- FIXME: this checks the result of the recursive call,
       --        can we do something about this?
       EAbs (VVar x) b -> 
           case f b of
                    ECase (EVar x') [Case (PVar y) e]
                          | x' == x && not (x `isFreeIn` e)
                              -> f (EAbs (VVar y) e)
                    e -> EAbs (VVar x) e
       -- for value declarations without patterns, compilePattDecls 
       -- generates pattern matching on the empty record, remove these
       ECase EEmptyRec [Case (PRec []) e] -> f e
       -- if the pattern matching is on a single field of a record expression
       -- with only one field, there is no need to wrap it in a record
       ECase (ERec [FieldValue x e]) cs | all (isSingleFieldPattern x) [ p | Case p _ <- cs]
              -> f (ECase e [ Case p r | Case (PRec [FieldPattern _ p]) r <- cs ])
       -- In cases: remove record field patterns which only bind unused variables
       Case (PRec fps) e -> Case (f (PRec (fps \\ unused))) (f e)
          where unused = [fp | fp@(FieldPattern l (PVar id)) <- fps, 
                               not (id `isFreeIn` e)]
       -- Remove wild card patterns in record patterns
       PRec fps -> PRec (map f (fps \\ wildcards))
          where wildcards = [fp | fp@(FieldPattern _ PWild) <- fps]
       _ -> composOp f x
 isSingleFieldPattern :: Ident -> Pattern -> Bool
 isSingleFieldPattern x p = case p of
                                PRec [FieldPattern y _] -> x == y
                                _ -> False

--
-- * Remove simple syntactic sugar.
--

desugar :: [Decl] -> C [Decl]
desugar = return . map f
 where
 f :: Tree a -> Tree a
 f x = case x of
              EIf exp0 exp1 exp2 -> ifBool          <| exp0 <| exp1 <| exp2
              EPiNoVar exp0 exp1 -> EPi VWild       <| exp0 <| exp1
              EOr  exp0 exp1     -> andBool         <| exp0 <| exp1
              EAnd exp0 exp1     -> orBool          <| exp0 <| exp1
              EEq  exp0 exp1     -> appIntBin "eq"  <| exp0 <| exp1
              ENe  exp0 exp1     -> appIntBin "ne"  <| exp0 <| exp1
              ELt  exp0 exp1     -> appIntBin "lt"  <| exp0 <| exp1
              ELe  exp0 exp1     -> appIntBin "le"  <| exp0 <| exp1
              EGt  exp0 exp1     -> appIntBin "gt"  <| exp0 <| exp1
              EGe  exp0 exp1     -> appIntBin "ge"  <| exp0 <| exp1
              EAdd exp0 exp1     -> appIntBin "add" <| exp0 <| exp1
              ESub exp0 exp1     -> appIntBin "sub" <| exp0 <| exp1
              EMul exp0 exp1     -> appIntBin "mul" <| exp0 <| exp1
              EDiv exp0 exp1     -> appIntBin "div" <| exp0 <| exp1
              EMod exp0 exp1     -> appIntBin "mod" <| exp0 <| exp1
              ENeg exp0          -> appIntUn  "neg" <| exp0
              _                  -> composOp f x
    where g <| x = g (f x)

--
-- * Integers
--

appIntUn :: String -> Exp -> Exp
appIntUn f e = EApp (var ("prim_"++f++"_Int")) e

appIntBin :: String -> Exp -> Exp -> Exp
appIntBin f e1 e2 = EApp (EApp (var ("prim_"++f++"_Int")) e1) e2

--
-- * Booleans
--

andBool :: Exp -> Exp -> Exp
andBool e1 e2 = ifBool e1 e2 (var "False")

orBool :: Exp -> Exp -> Exp
orBool e1 e2 = ifBool e1 (var "True") e2

ifBool :: Exp -> Exp -> Exp -> Exp
ifBool c t e = ECase c [Case (PCons (Ident "True") []) t,
                        Case (PCons (Ident "False") []) e]

--
-- * Substitution
--

subst :: Ident -> Exp -> Exp -> Exp
subst x e = f
 where 
 f :: Tree a -> Tree a
 f t = case t of
   ELet defs exp3 | x `Set.member` letDefBinds defs ->
         ELet [ LetDef id (f exp1) exp2 | LetDef id exp1 exp2 <- defs] exp3
   Case p e | x `Set.member` binds p -> t
   EAbs (VVar id) _ | x == id -> t
   EPi (VVar id) exp1 exp2 | x == id -> EPi (VVar id) (f exp1) exp2
   EVar i | i == x -> e
   _      -> composOp f t

--
-- * Abstract syntax utilities
--

var :: String -> Exp
var s = EVar (Ident s)

-- | Apply an expression to a list of arguments.
apply :: Exp -> [Exp] -> Exp
apply = foldl EApp

-- | Abstract a value over some arguments.
abstract :: Int -- ^ number of arguments
         -> ([Exp] -> Exp) -> C Exp
abstract n f = 
    do
    vs <- freshIdents n
    return $ foldr EAbs (f (map EVar vs)) (map VVar vs)

-- | Abstract a type over some arguments.
abstractType :: [Exp] -- ^ argument types
             -> ([Exp] -> Exp)
             -> C Exp
abstractType ts f = 
    do
    vs <- freshIdents (length ts)
    let pi (v,t) e = EPi (VVar v) t e
    return $ foldr pi (f (map EVar vs)) (zip vs ts)

-- | Get an identifier which cannot occur in user-written
--   code, and which has not been generated before.
freshIdent :: C Ident
freshIdent = do
             i <- get
             put (i+1)
             return (Ident ("x_"++show i))

freshIdents :: Int -> C [Ident]
freshIdents n = replicateM n freshIdent

-- | Get the variables bound by a set of let definitions.
letDefBinds :: [LetDef] -> Set Ident
letDefBinds defs = Set.fromList [ id | LetDef id _ _ <- defs]

letDefTypes :: [LetDef] -> [Exp]
letDefTypes defs = [ exp1 | LetDef _ exp1 _ <- defs ]

letDefRhss :: [LetDef] -> [Exp]
letDefRhss defs = [ exp2 | LetDef _ _ exp2 <- defs ]

-- | Get the free variables in an expression.
freeVars :: Exp -> Set Ident
freeVars = f
  where
  f :: Tree a -> Set Ident
  f t = case t of
   ELet defs exp3 -> 
        Set.unions $
           (Set.unions (f exp3:map f (letDefRhss defs)) Set.\\ letDefBinds defs)
           :map f (letDefTypes defs)
   ECase exp cases -> f exp `Set.union` 
                      Set.unions [ f e Set.\\ binds p | Case p e <- cases]
   EAbs (VVar id) exp -> Set.delete id (f exp)
   EPi (VVar id) exp1 exp2 -> f exp1 `Set.union` Set.delete id (f exp2)
   EVar i -> Set.singleton i
   _      -> composOpMonoid f t

isFreeIn :: Ident -> Exp -> Bool
isFreeIn x e = countFreeOccur x e > 0

-- | Count the number of times a variable occurs free in an expression.
countFreeOccur :: Ident -> Exp -> Int
countFreeOccur x = f
  where
  f :: Tree a -> Int
  f t = case t of
   ELet defs _ | x `Set.member` letDefBinds defs ->
            sum (map f (letDefTypes defs))
   Case p e | x `Set.member` binds p -> 0
   EAbs (VVar id) _ | id == x -> 0
   EPi (VVar id) exp1 _ | id == x -> f exp1
   EVar id | id == x -> 1
   _      -> composOpFold 0 (+) f t

-- | Get the variables bound by a pattern.
binds :: Pattern -> Set Ident
binds = f
 where 
 f :: Tree a -> Set Ident
 f p = case p of
                 -- replaceCons removes non-variable PVars
                 PVar id -> Set.singleton id 
                 _ -> composOpMonoid f p

-- | Checks if a declaration is a value declaration
--   of the given identifier.
isValueDecl :: Ident -> Decl -> Bool
isValueDecl x (ValueDecl y _ _) = x == y
isValueDecl _ _ = False

--
-- * Data types
--

type DataTypes = Map Ident (Exp,[(Ident,Exp)])

-- | Get a map of data type names to the type of the type constructor
--   and all data constructors with their types.
dataTypes :: [Decl] -> Map Ident (Exp,[(Ident,Exp)])
dataTypes ds = Map.fromList [ (i,(t,[(c,ct) | ConsDecl c ct <- cs])) | DataDecl i t cs <- ds]

getDataType :: DataTypes -> Ident -> (Exp,[(Ident,Exp)])
getDataType ts i = 
    fromMaybe (error $ "Data type " ++ printTree i ++ " not found")
              (Map.lookup i ts) 

--
-- * Utilities
--

infixl 1 >>>

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >>> g = (g =<<) . f
