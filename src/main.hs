import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)
import Control.Monad.State
import Control.Monad.Except

data Context = Context String

initialCtx = Context "test"

type HlogError = ExceptT String IO

data Atom = Atom String [String]
  deriving Show

data Exp = Fact Atom
         | Rule Atom [Atom]
         | Assertion [Atom]
         deriving Show

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
