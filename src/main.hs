{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import Control.Monad.State
import Control.Monad.Except
import Data.List
import Data.Char
import qualified Data.Map as Map hiding (show)
import qualified Test.LeanCheck as Lc

type HlogError = ExceptT String IO

type Atom = (String, [Term])

data Exp = Fact Atom
         | Rule Atom [Atom]
         | Assertion
         deriving Show

type Sub = Map.Map String Term

type Env = Map.Map String [Exp]

data Term = Var String
          | Fn String [Term]

--------------------------------------------------------
-- SHOW
--------------------------------------------------------

instance Show Term where
  show (Var x) = x
  show (Fn n ts) = case l of
                     [] -> n
                     (x:xs) -> n ++ "(" ++ (concat (intersperse ", " l)) ++ ")"
    where l = map show ts


showSub :: Sub -> String
showSub a = "{ " ++ l'' ++ " }"
  where l = Map.toList a
        l' = map (\(a, b) -> a ++ " -> " ++ (show b)) l
        l'' = concat (intersperse ", " l')

--------------------------------------------------------
-- TEST
--------------------------------------------------------

instance Lc.Listable Term where
  tiers = Lc.cons1 Var
          Lc.\/ Lc.cons2 Fn

unifyTest :: Term -> Term -> Bool
unifyTest t0 t1 = case unify t0 t1 of
                    Just r -> True
                    Nothing -> True

-- testEnv = MultiMap.fromList [ ("sum", Fact ("sum", [Var "X", Fn "z" [], Var  "X"])),
--                               ("sum", Rule ("sum", [Var "X",


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
unify (Var x) t1@(Var _) = Just $ Map.singleton x t1
unify (Var x) t1 = if elem x (termVars t1)
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
    return $ subConcat s s'
  where ln = (length xs) == (length ys)

markTerm :: Term -> Term
markTerm (Var n@(x:xs)) = if x == '_'
                          then Var n
                          else Var ('_':n)
markTerm (Fn n xs) = Fn n $ map markTerm xs

unmarkTerm :: Term -> Term
unmarkTerm (Var n@(x:xs)) = if x == '_'
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

subTerm :: Sub -> Term -> Term
subTerm s t =
  case t of
    Var x -> let mt = sub s x
             in case mt of
                  Just t' -> t'
                  Nothing -> Var x
    Fn x xs -> Fn x (map (subTerm s) xs)

subTerms :: Sub -> [Term] -> [Term]
subTerms s ts = map (subTerm s) ts

subConcat :: Sub -> Sub -> Sub
subConcat s0 s1 = if Map.null i
                  then u
                  else Map.map (subTerm i) u
  where u = Map.union s0 s1
        i = Map.intersection s1 s0

--------------------------------------------------------
-- ENVIRONMENT
--------------------------------------------------------

initialEnv = Map.empty

envGet :: String -> Env -> [Exp]
envGet n env = case Map.lookup n env of
                 Just l -> l
                 Nothing -> []

envAdd :: Exp -> Env -> Env
envAdd ex env = Map.insert n l' env
  where n = name ex
        l = envGet n env
        l' = l ++ [ex]

expArity :: String -> Env -> Maybe Int
expArity s env = case exps of
                  [] -> Nothing
                  (x:_) -> case x of
                             Fact (s, ss) -> Just $ length ss
                             Rule (s, ss) _ -> Just $ length ss
  where exps = envGet s env
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
filterMarkedSub s = if Map.null ns
                    then Map.map unmarkTerm $ Map.mapKeys (\(x:xs)-> xs) fs
                    else filterMarkedSub $ Map.map (\a -> subTerm ns a) s
  where sf ('_':xs) v = True
        sf _ v = False
        sf' ('_':xs) = True
        sf' _ = False
        fs = Map.filterWithKey sf s
        var = filter (not . sf') $ Map.foldr (\a b -> (termVars a) ++ b) [] fs
        ns = Map.filterWithKey (\a v -> a `elem` var) s

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

parseConstant = do
  s <- parseSymbol
  return  $ Fn s []

parseTerm = (try parseVar <|> try parseFn <|> parseConstant)

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
  case r of
    Just r' -> liftIO $ putStrLn $ showSub r'
    Nothing -> liftIO $ putStrLn "No Substitution"
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
            Just s1 -> return $ subConcat s0 s1
            Nothing -> resolveAtoms' env l (i + 1)

resolveAtoms env l@(e:exs) i n = do
          s0 <- resolveAtom env e i
          let exs' = subAtoms s0 exs
          case resolveAtoms env exs' of
            Just s1 -> if n == 0
                       then return $ subConcat s0 s1
                       else resolveAtoms
            Nothing -> resolveAtoms' env l (i + 1)



resolveAtom :: Env -> Atom -> Int -> Maybe Sub
resolveAtom env a@(n, _) i = matchExps env a exps i
  where exps = envGet n env

matchExps :: Env -> Atom -> [Exp] -> Int -> Maybe Sub
matchExps _ _ [] _ = Nothing
matchExps env a ((Fact (s', ss')):es) i =
  case matchExp of
    Just r -> if i > 0
              then matchExps env a' es (i - 1)
              else Just $ filterMarkedSub r
    Nothing -> matchExps env a' es i
  where a' = markAtom a
        (s, ss) = a'
        matchExp = unifyTerms ss ss'

matchExps env a ((Rule head@(s', ss') as):es) i =
  case matchExp of
    Just r -> if i > 0
              then matchExps env a' es (i - 1)
              else Just $ filterMarkedSub r
    Nothing -> matchExps env a' es i
  where a' = markAtom a
        (s, ss) = a'
        matchExp = do
          mhs <- unifyTerms ss ss'
          let sa = subAtoms mhs as
          rs <- resolveAtoms env sa
          let (s'', ss'') = subAtom rs head
          unifyTerms ss ss''





matchExp :: Env -> Atom -> Exp -> Maybe Sub
matchExp env (s, ss) (Fact (s', ss')) =
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
    Right _ -> putStrLn "Done"

  return ()
