module LuaParser
  ( parseLuaFile, parseLua, LuaStmt (..), LuaExpr (..), ParseError )
where
import Data.Maybe
import Numeric
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Hide a few names that are provided by Applicative.
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Language as L

data LuaStmt = Assign String LuaExpr
  deriving (Eq, Ord)
data LuaExpr = Num Double
             | Str String
             | Bln Bool
             | Arr [(LuaExpr, LuaExpr)]
  deriving (Eq, Ord)

instance Show LuaStmt where
  show (Assign v e) = v ++ " = " ++ show e

instance Show LuaExpr where
  show (Num n) = show n
  show (Str s) = "\"" ++ s ++ "\""
  show (Bln b) | b = "true" | otherwise = "false"
  show (Arr a)
    | null a    = "nil"
    | otherwise = "{" ++ concat (map showEntry a) ++ "}"
    where showEntry (k, e) = "[" ++ show k ++ "] = " ++ show e ++ ", "

lexer      = T.makeTokenParser 
               (L.emptyDef { L.commentLine = "--", 
                             L.identStart  = letter,
                             L.identLetter = alphaNum <|> char '_' })
whiteSpace = T.whiteSpace lexer
symbol     = T.symbol lexer
identifier = T.identifier lexer
brackets   = T.brackets lexer
braces     = T.braces lexer

number :: CharParser () Double
number = do
  s <- getInput
  (case readSigned readFloat s of
     [(n, s')] -> n <$ setInput s'
     _         -> empty)
    <* whiteSpace

luaString :: CharParser () String
luaString = between (char '\"') (char '\"') (many strchar) <* whiteSpace
  where
    strchar = char '\\' *> escapeChar
                <|> satisfy (`notElem` "\"\\")
    
escapeChar :: GenParser Char () Char
escapeChar = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
  where decode c r = r <$ char c

array :: GenParser Char () [(LuaExpr, LuaExpr)]
array = (symbol "nil" >> return []) <|>
        braces (checkLabels `fmap` cse)
  where
    -- common-separated entries followed by optional comma
    cse = catMaybes `fmap` sepBy (optional entry) (symbol ",")
    entry = do
      key <- optional (brackets expr <* symbol "=")
      exp <- expr
      return (key, exp)
    -- unlabeled entries are indexed from 1
    checkLabels es = zipWith (\ (k, e) n -> (maybe (Num n) id k, e)) es [1..]

bool :: GenParser Char () Bool
bool = (symbol "true" >> return True) <|> (symbol "false" >> return False)

stmt :: GenParser Char () LuaStmt
stmt = do
  var <- identifier
  symbol "="
  exp <- expr
  return $ Assign var exp

expr :: CharParser () LuaExpr
expr = Arr `fmap` array <|> Str `fmap` luaString <|> Num `fmap` number <|> Bln `fmap` bool

lua :: GenParser Char () LuaStmt
lua = whiteSpace >> stmt <* eof

parseLuaFile :: SourceName -> IO (Either ParseError LuaStmt)
parseLuaFile = parseFromFile lua

parseLua :: SourceName -> String -> Either ParseError LuaStmt
parseLua = parse lua
