module Parser where

import AST
import Tokens

data Parser = Parser
  { parserTokens :: [Token],
    -- errors :: [Error], TODO: implement errors
    parserPos :: Int
  }

hasTokens :: Parser -> Bool
hasTokens parser = parserPos parser < length (parserTokens parser) && currentTokenKind parser /= EOF

currentToken :: Parser -> Token
currentToken parser
  | hasTokens parser = parserTokens parser !! parserPos parser
  | otherwise = Token {tokenValue = "", tokenKind = EOF}

currentTokenKind :: Parser -> TokenKind
currentTokenKind parser = tokenKind $ currentToken parser

advance :: Parser -> (Token, Parser)
advance parser = (currentToken parser, parser {parserPos = parserPos parser + 1})

createParser :: [Token] -> Parser
createParser tokens = Parser {parserTokens = tokens, parserPos = 0}

parseStmt :: Parser -> Stmt
parseStmt parser = BlockStmt {body = []} -- TODO: implement

fromParser :: Parser -> [Stmt]
fromParser parser
  | hasTokens parser = parseStmt parser : fromParser (snd $ advance parser)
  | otherwise = []

parse :: [Token] -> Stmt
parse tokens =
  let body = [] :: [Stmt]
   in BlockStmt {body = body}
