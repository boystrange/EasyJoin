{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of EasyJoin.                                    ║
│                                                                   ║
│ EasyJoin is free software: you can redistribute it and/or modify  ║
│ it under the terms of the GNU General Public License as published ║
│ by the Free Software Foundation, either version 3 of the License, ║
│ or (at your option) any later version.                            ║
│                                                                   ║
│ EasyJoin is distributed in the hope that it will be useful, but   ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with EasyJoin. If not, see <http://www.gnu.org/licenses/>.  ║
│                                                                   ║
│ Copyright 2019 Luca Padovani                                      ║
╘═══════════════════════════════════════════════════════════════════╝
-}

module TypeParser (parseType) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Type

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "0", "1" ]
           , Token.reservedOpNames = [ "·", "|", "*", "+", ";", "$" ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

typeParser :: Parser Type
typeParser = whiteSpace >> typeExpr >>= (\t -> reservedOp "$" >> return t)

typeExpr :: Parser Type
typeExpr = buildExpressionParser operators term

operators = [ [Prefix (reservedOp "*" >> return Star)]
            , [Infix  (reservedOp ";" >> return Then) AssocLeft]
            , [Infix  (reservedOp "|" >> return And) AssocLeft]
            , [Infix  (reservedOp "·" >> return And) AssocLeft]
            , [Infix  (reservedOp "+" >> return Or) AssocLeft]
            ]

term = parens typeExpr
       <|> liftM Tag identifier
       <|> (reserved "0" >> return Zero)
       <|> (reserved "1" >> return One)

parseType :: String -> Type
parseType s =
  case parse typeParser "" (s ++ "$") of
    Left e -> error $ show e
    Right t -> t
