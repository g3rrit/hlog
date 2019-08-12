{-# LANGUAGE RankNTypes #-}
import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import Control.Monad.State
import Control.Monad.Except
import Data.Char
import qualified Data.MultiMap as MultiMap
import qualified Data.Map as Map
import qualified Test.LeanCheck as Lc

data Context = Context String

initialCtx = Context "test"

type HlogError = ExceptT String IO

data Atom = Atom String [String]
  deriving Show

data Exp = Fact Atom
         | Rule Atom [Atom]
         deriving Show

type Sub = Map.Map String String

type Env = MultiMap.MultiMap String Exp

--------------------------------------------------------
-- TEST
--------------------------------------------------------

instance (Show a, Show b) => Show (MultiMap.MultiMap a b) where
  show e = show $ MultiMap.elems e


newtype TestEnv = TestEnv Env
instance Lc.Listable TestEnv where
  tiers = [map (\(a', b') -> TestEnv $ MultiMap.fromList $ zip a' b')
           $ zip (Lc.tiers :: [[String]]) (Lc.tiers :: [[Exp]])]

testEnv = MultiMap.fromList [("f0", Fact (Atom "f0" ["b", "a"])),
                             ("f1", Fact (Atom "f0" ["c", "c"])),
                             ("f2", Fact (Atom "f2" ["a", "b"])),
                             ("r0", Rule (Atom "r0" ["X", "Y"]) [
                                 (Atom "f0" ["Y", "X"]),
                                 (Atom "f1" ["c", "c"]) ]),
                             ("r1", Rule (Atom "r1" ["c", "X"]) [
                                 (Atom "f0" ["X", "a"]) ]) ]


testAtoms = [Atom "f0" ["X", "a"], Atom "r0" ["X", "b"], Atom "r1" ["Y", "b"]]

testAtom = (Atom "r1" ["X", "Y"])
testRule = (Rule (Atom "r1" ["c", "X"]) [Atom "f0" ["X", "a"]])


atomFromEnv :: Env -> Int -> Int -> Atom
atomFromEnv e i n = case el !! n' of
                      Fact a -> a
                      Rule a as -> a
  where els = MultiMap.elems e
        i' = i `mod` (length els)
        el = els !! i'
        n' = n `mod` (length el)

testResolveAtoms e i n c = case resolveAtoms e (as n) of
                           Just r -> True
                           _ -> False
  where as 0 = []
        as n' = (atomFromEnv e (i + n') c) : as (n' - 1)

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
markVar (Atom s ss) = Atom s (map (\x -> if isVar x then '_':x else x) ss)

filterMarkedSub :: Sub -> Sub
filterMarkedSub s =  Map.map (\(x:xs) -> xs) fs
  where fs = Map.filter (\a -> case a of
                                 '_':xs -> True
                                 _      -> False) s

--------------------------------------------------------
-- PARSER
--------------------------------------------------------

type VarName = String
type SymName = String

type HlogResult = StateT Context HlogError [Exp]

ignoreList = [ '\n', '\r', '\t', ' ']

parseVar = do
  x <- upper
  xs <- many letter
  return $ x:xs

parseSymbol = do
  x <- lower
  xs <- many letter
  return $ x:xs

parseName = parseSymbol <|> parseVar

parseAtom = do
  s <- parseSymbol
  char '('
  xs <- sepBy parseName (char ',')
  char ')'
  return $ Atom s xs

parseFact = do
  a <- parseAtom
  char '.'
  return $ Fact a

parseRule = do
  s <- parseAtom
  string ":-"
  xs <- sepBy parseAtom (char ',')
  char '.'
  return $ Rule s xs

parseExp :: ParsecT [Char] Context IO [Exp]
parseExp = many (try parseFact <|> try parseRule)

--------------------------------------------------------
-- RESOLVER
--------------------------------------------------------

applySubs :: Sub -> [Atom] -> [Atom]
applySubs s al = map (applySub s) al

applySub :: Sub -> Atom -> Atom
applySub s (Atom n ss) = Atom n (map (\a -> case sub s a of
                                              Just r  -> r
                                              Nothing -> a) ss)


resolveAtoms :: Env -> [Atom] -> Maybe Sub
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
    Just r -> if i >= 0 then matchExps env a' es (i - 1) else Just $ filterMarkedSub r
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

  r <- runParserT parseExp initialCtx "" (filter (`notElem` ignoreList) source)
  case r of
    Left e -> putStrLn $ "Error: " ++ (show e)
    Right v -> putStrLn $ show v

  return ()
