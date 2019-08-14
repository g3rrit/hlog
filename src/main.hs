{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import Control.Monad.State
import Control.Monad.Except
import Data.Char
import qualified Data.MultiMap as MultiMap
import qualified Data.Map as Map
import qualified Test.LeanCheck as Lc

type HlogError = ExceptT String IO

type Atom = (String, [Term])

data Exp = Fact Atom
         | Rule Atom [Atom]
         | Assertion
         deriving Show

type Sub = Map.Map String Term

type Env = MultiMap.MultiMap String Exp

data Term = Var String
          | Fn String [Term]
          deriving Show


--------------------------------------------------------
-- TERM
--------------------------------------------------------

termArity :: Term -> Int
termArity (Fn _ xs) = length xs
termArity _ = 0

termVars :: Term -> [String]
termVars (Var x) = [x]
termVars (Fn _ xs) = xs >>= termVars

compareArity :: Term -> Term -> Bool
compareArity t0 t1 = (termArity t0) == (termArity t1)

unify :: Term -> Term -> Maybe Sub
unfiy (Var x) t1@(Var _) = Just $ Map.singleton x t1
unfiy (Var x) t1 = if elem x (termVars t1)
                   then Nothing
                   else Just $ Map.singleton x t1
unify t0 t1@(Var _) = unify t1 t0
unify (Fn n xs) (Fn p ys) = if n /= p
                            then Nothing
                            else unifyTerms xs ys

unifyTerms :: [Term] -> [Term] -> Maybe Sub
unifyTerms xs ys = unifyTerms' xs ys emptySub

unifyTerms' [] [] s = Just s
unifyTerms' xs ys s =
  if not ln
  then Nothing
  else let (x:xs', y:ys') = (xs, ys) in do
    s <- unify x y
    let xs'' = subTerms s xs'
    let ys'' = subTerms s ys'
    s' <- unifyTerms xs'' ys''
    return $ Map.union s s'
  where ln = (length xs) == (length ys)

markTerm :: Term -> Term
markTerm (Var n@(x:xs)) = if x == '_'
                          then Var n
                          else Var ('_':n)
markTerm (Fn n xs) = Fn n $ map markTerm xs

unmarkTerm :: Term -> Term
unmakrTerm (Var n@(x:xs)) = if x == '_'
                            then Var xs
                            else Var n
unmarkTerm (Fn n xs) = Fn n $ map unmarkTerm xs

--------------------------------------------------------
-- Substitution
--------------------------------------------------------

sub :: Sub -> String -> Maybe Term
sub su s = Map.lookup s su

emptySub :: Sub
emptySub = Map.empty

-- concatSub :: Sub -> Sub -> Maybe Sub
-- concatSub s0 s1 = if not ins
--                   then Nothing
--                   else Just $ Map.union s0 s1
--   where ins = foldr (&&) True $ Map.intersectionWith (==) s0 s1

subTerm :: Sub -> Term -> Term
subTerm s t =
  case t of
    Var x -> let mt = sub s x
             in case mt of
                  Just t' -> t
                  Nothing -> Var x
    Fn x xs -> Fn x (map (subTerm s) xs)

subTerms :: Sub -> [Term] -> [Term]
subTerms s ts = map (subTerm s) ts

-- concatSub :: Sub -> Sub -> Maybe Sub
-- concatSub s0 s1 = do
--   ms <- maybeSubs subs
--   return $ Map.map (\v -> foldr (\a b -> subTerm a b) v ms) sn
--   where mf s = \v -> subTerm s v
--         s0' = Map.map (mf s1) s0
--         s1' = Map.map (mf s0) s1
--         t0s = Map.elems $ Map.intersection s0' s1'
--         t1s = Map.elems $ Map.insersection s1' s0'
--         keys =  Map.keys $ Map.intersection s0' s1'
--         sn = Map.union s0' s1'
--         subs = zipWith (\a b -> unify a b) t0s t1s

-- maybeSubs :: [Maybe Sub] -> Maybe [Sub]
-- maybeSubs [] = Just []
-- maybeSubs (Nothing:_) = Nothing
-- maybeSubs ((Just x):xs) = do
--   ms <- maybeSubs xs
--   return $ x:ms


-- createSub :: String -> String -> Maybe Sub
-- createSub s0 s1
--   | isSym s0 && isSym s1 = if s0 == s1 then Just $ emptySub else Nothing
--   | isSym s0 && isVar s1 = Just $ Map.singleton s1 s0
--   | isVar s0 && isSym s1 = Just $ Map.singleton s0 s1
--   | isVar s0 && isVar s1 = Just $ emptySub

-- createSubs :: [String] -> [String] -> Maybe Sub
-- createSubs [] [] = Just emptySub
-- createSubs _ []  = Nothing
-- createSubs [] _  = Nothing
-- createSubs s0 s1 = foldr csub (Just emptySub) $ zip s0 s1
--   where csub (s0', s1') c = do
--           ns <- createSub s0' s1'
--           c' <- c
--           concatSub ns c'

--------------------------------------------------------
-- ENVIRONMENT
--------------------------------------------------------

initialEnv = MultiMap.empty

envAdd :: Exp -> Env -> Env
envAdd ex env = MultiMap.insert n ex env
  where n = name ex

expArity :: String -> Env -> Maybe Int
expArity s env = case exps of
                  [] -> Nothing
                  (x:_) -> case x of
                             Fact (s, ss) -> Just $ length ss
                             Rule (s, ss) _ -> Just $ length ss
  where exps = MultiMap.lookup s env
        l = length exps

--------------------------------------------------------
-- UTIL
--------------------------------------------------------

name :: Exp -> String
name (Fact (n, _)) = n
name (Rule (n, _) _) = n

isVar :: String -> Bool
isVar (x:_) = Data.Char.isUpper x || x == '_'

isSym :: String -> Bool
isSym x = not $ isVar x

vars :: [String] -> [String]
vars s = filter isVar s

syms :: [String] -> [String]
syms s = filter isSym s

markAtom :: Atom -> Atom
markAtom (s, ts) = (s, map markTerm ts)

filterMarkedSub :: Sub -> Sub
filterMarkedSub s = Map.map unmarkTerm $ Map.mapKeys (\(x:xs)-> xs) fs
  where fs = Map.filterWithKey (\k v -> case k of
                                          '_':xs -> True
                                          _      -> False) s

--------------------------------------------------------
-- PARSER
--------------------------------------------------------

type VarName = String
type SymName = String

type HlogResult = StateT Env HlogError [Exp]

ignoreList = [ '\n', '\r', '\t', ' ']

parseSymbol = (try parseSymbol' <|> many alphaNum)
  where parseSymbol' = do
          x <- lower
          xs <- many alphaNum
          return $ x:xs

parseVar = do
  x <- upper
  xs <- many alphaNum
  let r = Var (x:xs)
  return r

parseFn = do
  s <- parseSymbol
  char '('
  xs <- sepBy parseTerm (char ',')
  char ')'
  let r = Fn s xs
  return r

parseTerm = (try parseVar <|> parseFn)

parseAtom = do
  s <- parseSymbol
  char '('
  xs <- sepBy parseTerm (char ',')
  char ')'
  let r = (s, xs)
  env <- getState
  let mc = expArity s env
  case mc of
    Nothing -> return r
    Just c  -> if c == length xs
               then return r
               else unexpected $ "Invalid Number of Arguments for: " ++ (show r)

parseFact = do
  a <- parseAtom
  char '.'
  let r = Fact a
  modifyState $ envAdd r
  return r

parseRule = do
  s <- parseAtom
  string ":-"
  xs <- sepBy parseAtom (char ',')
  char '.'
  let r = Rule s xs
  modifyState $ envAdd r
  return r

parseAssertion = do
  string ":-"
  xs <- sepBy parseAtom (char ',')
  char '.'
  env <- getState
  let r = resolveAtoms env xs
  liftIO $ putStrLn $ show r
  return Assertion

parseExp :: ParsecT [Char] Env IO [Exp]
parseExp = many (try parseFact <|> try parseRule <|> parseAssertion)

--------------------------------------------------------
-- RESOLVER
--------------------------------------------------------

subAtoms :: Sub -> [Atom] -> [Atom]
subAtoms s al = map (subAtom s) al

subAtom :: Sub -> Atom -> Atom
subAtom s (n, ss) =
  (n, subTerms s ss)

resolveAtoms :: Env -> [Atom] -> Maybe Sub
resolveAtoms e [] = Just emptySub
resolveAtoms e l = resolveAtoms' e l 0
  where resolveAtoms' env l@(e:exs) i = do
          s0 <- resolveAtom env e i
          let exs' = subAtoms s0 exs
          case resolveAtoms env exs' of
            Just s1 -> return $ Map.union s0 s1
            Nothing -> resolveAtoms' env l (i + 1)

resolveAtom :: Env -> Atom -> Int -> Maybe Sub
resolveAtom env a@(n, _) i = matchExps env a exps i
  where exps = MultiMap.lookup n env

matchExps :: Env -> Atom -> [Exp] -> Int -> Maybe Sub
matchExps _ _ [] _ = Nothing
matchExps env a (e:es) i =
  case matchExp env a' e of
    Just r -> if i > 0
              then matchExps env a' es (i - 1)
              else Just $ filterMarkedSub r
    Nothing -> matchExps env a' es i
  where a' = markAtom a

matchExp :: Env -> Atom -> Exp -> Maybe Sub
matchExp env (s, ss) (Fact (s', ss')) = unifyTerms ss ss'
matchExp env (s, ss) (Rule head@(s', ss') as) = do
  mhs <- unifyTerms ss ss'
  let sa = subAtoms mhs as
  rs <- resolveAtoms env sa
  let (s'', ss'') = subAtom rs head
  unifyTerms ss ss''

--------------------------------------------------------
-- MAIN
--------------------------------------------------------

main = do
  let fpath = "test.txt"
  putStrLn "----- Hlog ------"
  source <- readFile fpath
  putStrLn $ "Parsing Source:\n" ++ source
  putStrLn "---------------"

  r <- runParserT parseExp initialEnv "" (filter (`notElem` ignoreList) source)
  case r of
    Left e -> putStrLn $ "Error: " ++ (show e)
    Right v -> putStrLn $ show v

  return ()
