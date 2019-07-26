import Text.ParserCombinators.Parsec
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

parseExp = many (parseFact <|> parseRule)

parseFile :: String -> String -> HlogResult
parseFile fname source =
  case (parse parseExp fname source) of
    Right x -> return x
    Left e -> throwError $ show e


repl = do
  let fpath = "test.txt"
  liftIO $ putStrLn "----- Hlog ------"
  source <- liftIO $ readFile fpath
  liftIO $ putStrLn $ "Parsing Source:\n" ++ source
  liftIO $ putStrLn $ "---------------"
  t <- parseFile fpath (filter (`notElem` ignoreList) source)
  liftIO $ putStrLn $ show t


main = case (runExceptT (evalStateT repl initialCtx)) of
         Left
