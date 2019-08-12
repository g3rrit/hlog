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

data Atom = Atom String [String]
  deriving Show

data Exp = Fact Atom
         | Rule Atom [Atom]
         | Assertion
         deriving Show

type Sub = Map.Map String String

type Env = MultiMap.MultiMap String Exp

initialEnv = MultiMap.empty

--------------------------------------------------------
-- TEST
--------------------------------------------------------

--------------------------------------------------------
-- ENVIRONMENT
--------------------------------------------------------

envAdd :: Exp -> Env -> Env
envAdd ex env = MultiMap.insert n ex env
  where n = name ex

expArgs :: String -> Env -> Maybe Int
expArgs s env = case exps of
                  [] -> Nothing
                  (x:_) -> case x of
                             Fact (Atom s ss) -> Just $ length ss
                             Rule (Atom s ss) _ -> Just $ length ss
  where exps = MultiMap.lookup s env
        l = length exps

--------------------------------------------------------
-- UTIL
--------------------------------------------------------

name :: Exp -> String
name (Fact (Atom n _)) = n
name (Rule (Atom n _) _) = n

isVar :: String -> Bool
isVar (x:_) = Data.Char.isUpper x || x == '_'

isSym :: String -> Bool
isSym x = not $ isVar x

vars :: [String] -> [String]
vars s = filter isVar s

syms :: [String] -> [String]
syms s = filter isSym s

sub :: Sub -> String -> Maybe String
sub su s = Map.lookup s su

emptySub :: Sub
emptySub = Map.empty

concatSub :: Sub -> Sub -> Maybe Sub
concatSub s0 s1 = if not ins
                  then Nothing
                  else Just $ Map.union s0 s1
  where ins = foldr (&&) True $ Map.intersectionWith (==) s0 s1


createSub :: String -> String -> Maybe Sub
createSub s0 s1
  | isSym s0 && isSym s1 = if s0 == s1 then Just $ emptySub else Nothing
  | isSym s0 && isVar s1 = Just $ Map.singleton s1 s0
  | isVar s0 && isSym s1 = Just $ Map.singleton s0 s1
  | isVar s0 && isVar s1 = Just $ emptySub

createSubs :: [String] -> [String] -> Maybe Sub
createSubs [] [] = Just emptySub
createSubs _ []  = Nothing
createSubs [] _  = Nothing
createSubs s0 s1 = foldr csub (Just emptySub) $ zip s0 s1
  where csub (s0', s1') c = do
          ns <- createSub s0' s1'
          c' <- c
          concatSub ns c'


markVar :: Atom -> Atom
markVar (Atom s ss) = Atom s (map (\x -> if (isVar x) && ((head x) /= '_')then '_':x else x) ss)

filterMarkedSub :: Sub -> Sub
filterMarkedSub s =  Map.mapKeys (\(x:xs)-> xs) fs
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

parseVar = do
  x <- upper
  xs <- many alphaNum
  return $ x:xs

parseSymbol = (try parseSymbol' <|> many alphaNum)
  where parseSymbol' = do
          x <- lower
          xs <- many alphaNum
          return $ x:xs

parseName = parseSymbol <|> parseVar

parseAtom = do
  s <- parseSymbol
  char '('
  xs <- sepBy parseName (char ',')
  char ')'
  let r = Atom s xs
  env <- getState
  let mc = expArgs s env
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

applySubs :: Sub -> [Atom] -> [Atom]
applySubs s al = map (applySub s) al

applySub :: Sub -> Atom -> Atom
applySub s (Atom n ss) =
  Atom n (map (\a -> case sub s a of
                       Just r  -> r
                       Nothing -> a) ss)

resolveAtoms :: Env -> [Atom] -> Maybe Sub
resolveAtoms e [] = Just emptySub
resolveAtoms e l = resolveAtoms' e l 0
  where resolveAtoms' env l@(e:exs) i = do
          s0 <- resolveAtom env e i
          let exs' = applySubs s0 exs
          case resolveAtoms env exs' of
            Just s1 -> concatSub s0 s1
            Nothing -> resolveAtoms' env l (i + 1)

resolveAtom :: Env -> Atom -> Int -> Maybe Sub
resolveAtom env a@(Atom n _) i = matchExps env a exps i
  where exps = MultiMap.lookup n env

matchExps :: Env -> Atom -> [Exp] -> Int -> Maybe Sub
matchExps _ _ [] _ = Nothing
matchExps env a (e:es) i =
  case matchExp env a' e of
    Just r -> if i > 0
              then matchExps env a' es (i - 1)
              else Just $ filterMarkedSub r
    Nothing -> matchExps env a' es i
  where a' = markVar a

matchExp :: Env -> Atom -> Exp -> Maybe Sub
matchExp env (Atom s ss) (Fact (Atom s' ss')) = createSubs ss ss'
matchExp env (Atom s ss) (Rule head@(Atom s' ss') as) = do
  mhs <- createSubs ss ss'
  let sa = applySubs mhs as
  rs <- resolveAtoms env sa
  let (Atom s'' ss'') = applySub rs head
  createSubs ss ss''

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
