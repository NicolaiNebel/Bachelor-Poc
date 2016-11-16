module PocParser
       (
         parseProg
       , Kernel (..)
       , Stmt(..)
       , Index(..)
       )
where

import Text.Parsec hiding (token)

type Parser = Parsec String ()

data Kernel = Kernel [String] [Stmt]
  deriving (Show)
data Stmt = ArrAccess String [Index]
  deriving (Show)

-- This name needs to go. And fast.
data Index = Var String | Inv
  deriving (Show, Eq)


parseProg :: SourceName -> String -> Either ParseError Kernel
parseProg = parse (p_kernel <* eof)

p_kernel :: Parser Kernel
p_kernel = Kernel <$> (str "Kernel" *> between (chr '(') (chr ')') p_vars)
                  <*> between (chr '{') (chr '}') p_stmts

p_stmts :: Parser [Stmt]
p_stmts = many p_stmt

p_stmt :: Parser Stmt
p_stmt = (p_access <* chr ';')

p_access :: Parser Stmt 
p_access = ArrAccess <$> (p_var <* chr '[' ) <*> (p_indexes <* chr ']')

p_indexes :: Parser [Index]
p_indexes = fmap (map Var) p_vars

p_vars :: Parser [String]
p_vars = (:) <$> p_var <*>
  (   chr ',' *> p_vars       -- Extra vars
  <|> return []               -- Last var
  )

p_var :: Parser String
p_var = token $ (:) <$> letter <*> many alphaNum

chr :: Char -> Parser Char
chr = token . char

str :: String -> Parser String
str = token . string

token :: Parser a -> Parser a
token = (<* spaces)
