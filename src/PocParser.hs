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

type Program = Kernel
data Kernel = Kernel [LitVar] [Stmt]
  deriving (Show)
data Stmt = ArrAccess LitVar [LitVar]
  deriving (Show)

type LitVar = String


parseProg :: SourceName -> String -> Either ParseError Program
parseProg = parse (p_kernel <* eof)

p_kernel :: Parser Kernel
p_kernel = Kernel <$> (str "Kernel" *> between (chr '(') (chr ')') p_vars)
                  <*> between (chr '{') (chr '}') p_stmts

p_stmts :: Parser [Stmt]
p_stmts = many p_stmt

p_stmt :: Parser Stmt
p_stmt = (p_access <* chr ';')

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
