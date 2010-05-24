module Parser (
    parseExp
  , parseDef
  ) where

import Env
import Exp
import Parsec

type SKIParser a = GenParser Char Env a

----------------------------------------------------------------

parseExp :: String -> Env -> Exp
parseExp cs env = case runParser apply env "" cs of
    Right x -> x
    Left  e -> error $ show e

parseDef :: String -> Env -> (Char,Exp)
parseDef cs env = case runParser def env "" cs of
    Right x -> x
    Left  e -> error $ show e

----------------------------------------------------------------

apply :: SKIParser Exp
apply = expr (combinator <|> variables)

def :: SKIParser (Char,Exp)
def = do
    c <- upper
    char '='
    exp0 <- expr combinator
    return (c,exp0)

----------------------------------------------------------------

expr :: SKIParser Exp -> SKIParser Exp
expr element = expression (term element)
  where
    expression p = foldl1 (:.) <$> many1 p

term :: SKIParser Exp -> SKIParser Exp
term element = element <|> factor element

factor :: SKIParser Exp -> SKIParser Exp
factor element = (char '(' *> expr element) <* char ')'

----------------------------------------------------------------

combinator :: SKIParser Exp
combinator = do
    c <- upper
    resolve c <$> getState

variables :: SKIParser Exp
variables = Var <$> lower
