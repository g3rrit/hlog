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
         deriving Show

type Sub = String -> Maybe String

type Env = Data.MultiMap String Exp


--------------------------------------------------------
-- UTIL
--------------------------------------------------------

name :: Exp -> String
name Fact (Atom n _) = n
name Rule (Atom n _) _ = n

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

createSub :: String -> String -> Maybe Sub
createSub s0 s1
  | isSym s0 && isSym s1 = Nothing
  | isSym s0 && isVar s1 = \s -> if s == s1 then Just s0 else Nothing
  | isVar s0 && isSym s1 = \s -> if s == s0 then Just s1 else Nothing
  | isVar s0 && isSym s1 = \s -> if s == s0
                                 then Just s1
                                 else if s == s1
                                      then Just s0
                                      else Nothing

createSubs :: [String] -> [String] -> Maybe Sub
createSubs [] [] = \s -> Nothing
createSubs _ []  = Nothing
createSubs [] _  = Nothing
createSubs (s0:s0') (s1:s1') = do
  su0 <- createSub s0 s1
  return $ concatSub sub0 $ createSubs s0' s1'


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


resolveAtoms :: Env -> [Atom] -> Maybe Sub
resolveAtoms e l = resolveExps' l 0
  where resolveAtoms' e l@(e:exs) i = do
          s0 <- resolveAtom e i
          let exs' = applySub s0 exs
          case resolveAtoms exs' of
            Just s1 -> return $ concatSub s0 s1
            Nothing -> resolveAtoms' l (i + 1)

resolveAtom :: Env -> Atom -> Int -> Maybe Sub
resolveAtom env a i = matchExp a exps i
  where exps = lookup (name a) env

matchExps :: Atom -> [Exps] -> Int -> Maybe Sub
matchExps _ [] _ = Nothing
matchExps a (e:es) i =
  case matchExp a e of
    Just r -> if i >= 0 then matchExps a es (i - 1) else Just r
    Nothing -> matchExps a es i

matchExp :: Atom -> Exp -> Maybe Sub
matchExp (Atom s ss) (Fact (Atom s' ss')) = createSubs ss ss'
matchExp (Atom s ss) (Rule (Atom s' ss') as) = undefined

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
