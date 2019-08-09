import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import Control.Monad.State
import Control.Monad.Except
import Data.Char
import Data.MultiMap

data Context = Context String

initialCtx = Context "test"

type HlogError = ExceptT String IO

data Atom = Atom String [String]
  deriving Show

data Exp = Fact Atom
         | Rule Atom [Atom]
         | Assertion [Atom]
         deriving Show

type Sub = String -> Maybe String

type Env = Data.MultiMap String Exp


--------------------------------------------------------
-- UTIL
--------------------------------------------------------

isVar :: String -> Bool
isVar (x:_) = Data.Char.isUpper x

isSym :: String -> Bool
isSym x = not $ isVar x

vars :: [String] -> [String]
vars s = filter isVar s

syms :: [String] -> [String]
syms s = filter isSym s

concatSub :: Sub -> Sub -> Sub
concatSub s0 s1 = \s -> case s0 s of
                          Just r -> Just r
                          Nothing -> s0 s

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

applySub :: Sub -> [Atom] -> [Atom]
applySub s al = map (applySub' s) al
  where applySub' s a = undefined


resolveAtoms :: [Atom] -> Maybe Sub
resolveAtoms l = resolveExps' l 0
  where resolveAtoms' l@(e:exs) i = do
          s0 <- resolveAtom e i
          let exs' = applySub s0 exs
          case resolveAtoms exs' of
            Just s1 -> return $ concatSub s0 s1
            Nothing -> resolveAtoms' l (i + 1)

resolveAtom :: Atom -> Int -> Maybe Sub
resolveAtom a i =
  where exps = lookup


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
