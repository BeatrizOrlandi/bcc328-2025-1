{-# LANGUAGE OverloadedStrings #-}
module L.L1.Frontend.RecursiveParser
  ( l1Parser        -- String -> Either String L1
  , ParserError
  ) where

import Control.Applicative        ((<|>), empty)
import Data.Void                  (Void)
import Text.Megaparsec            ( ParseErrorBundle, Parsec, runParser
                                  , between, eof, many, manyTill
                                  , notFollowedBy, try )
import Text.Megaparsec.Char       ( alphaNumChar, letterChar, space1, string
                                  , char )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import L.L1.Frontend.Syntax
import Utils.Value  (Value(..))
import Utils.Var    (Var(..))

-- Tipos auxiliares
type Parser      = Parsec Void String
type ParserError = ParseErrorBundle String Void

--------------------------------------------------------------------------------
-- Espaços e símbolos
--------------------------------------------------------------------------------
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme  :: Parser a -> Parser a
lexeme  = L.lexeme sc

symbol  :: String -> Parser String
symbol  = L.symbol sc

parens  :: Parser a -> Parser a
parens  = between (symbol "(") (symbol ")")

semi, comma :: Parser String
semi  = symbol ";"
comma = symbol ","

reserved :: String -> Parser ()
reserved w = lexeme (try (string w *> notFollowedBy alphaNumChar))

--------------------------------------------------------------------------------
-- Tokens primitivos
--------------------------------------------------------------------------------
pInt, pString :: Parser Value
pInt    = VInt <$> lexeme (L.signed sc L.decimal)
pString = VStr <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pVar :: Parser Var
pVar  = lexeme $ Var <$> ((:) <$> letterChar <*> many alphaNumChar)

--------------------------------------------------------------------------------
-- Expressões
--------------------------------------------------------------------------------
table :: [[Operator Parser E1]]
table =
  [ [ InfixL (LMul   <$ symbol "*")
    , InfixL (LDiv   <$ symbol "/") ]
  , [ InfixL (LAdd   <$ symbol "+")
    , InfixL (LMinus <$ symbol "-") ]
  ]

pFactor, pExpr :: Parser E1
pFactor =
        (LVal <$> pInt)
    <|> (LVal <$> pString)
    <|> (LVar <$> pVar)
    <|> parens pExpr

pExpr = makeExprParser pFactor table

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------
pAssign, pRead, pPrint, pStmt :: Parser S1

pAssign = do
  v <- pVar
  _ <- symbol ":="
  e <- pExpr
  pure (LAssign v e)

pRead = do
  reserved "read"
  _        <- symbol "("
  VStr msg <- pString
  _        <- comma
  v        <- pVar
  _        <- symbol ")"
  pure (LRead msg v)

pPrint = do
  reserved "print"
  e <- parens pExpr
  pure (LPrint e)

pStmt = (try pRead <|> try pPrint <|> pAssign) <* semi

--------------------------------------------------------------------------------
-- Programa completo
--------------------------------------------------------------------------------
pProgram :: Parser L1
pProgram = sc *> (L1 <$> many pStmt) <* eof

-- Função pura exposta
l1Parser :: String -> Either String L1
l1Parser src =
  case runParser pProgram "<input>" src of
    Left  err -> Left  (show err)
    Right ast -> Right ast
