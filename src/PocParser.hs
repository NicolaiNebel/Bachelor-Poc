module PocParser
       (
         parseProg
       , Program
       , Stmt(..)
       , LitVar
       )
where

import Text.Parsec hiding (token)

type Parser = Parsec String ()

type Program = [Stmt]
data Stmt = Loop LitVar [Stmt]
          | ArrAccess LitVar [LitVar]
  deriving (Show)

type LitVar = String


parseProg :: SourceName -> String -> Either ParseError Program
parseProg = parse (p_stmts <* eof)

p_stmts :: Parser [Stmt]
p_stmts = many p_stmt

p_stmt :: Parser Stmt
p_stmt = (try p_loop) <|> (p_access <* chr ';')

p_loop :: Parser Stmt
p_loop = Loop <$> (str "loop" *> p_var) <*> ((chr '{' *> p_stmts) <* chr '}')

p_access :: Parser Stmt 
p_access = ArrAccess <$> (p_var <* chr '[' ) <*> (p_vars <* chr ']')

p_vars :: Parser [LitVar]
p_vars = (:) <$> p_var <*>
  (   chr ',' *> p_vars       -- Extra vars
  <|> return []               -- Last var
  )

p_var :: Parser LitVar
p_var = token $ (:) <$> letter <*> many alphaNum

chr :: Char -> Parser Char
chr = token . char

str :: String -> Parser String
str = token . string

token :: Parser a -> Parser a
token = (<* spaces)
