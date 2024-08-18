module Parser where

import AST
import Tokens

data Parser = Parser
  { parserTokens :: [Token],
    -- errors :: [Error], TODO: implement errors
    parserPos :: Int
  }

--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------

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

errorMsgOrDefault :: Maybe String -> String -> String
errorMsgOrDefault (Just msg) _ = msg
errorMsgOrDefault Nothing defaultMsg = defaultMsg

expectError :: Maybe String -> Parser -> TokenKind -> (Token, Parser)
expectError maybeError parser expectedKind =
  let (token, updatedParser) = advance parser
      kind = tokenKind token
   in if kind /= expectedKind
        then error $ errorMsgOrDefault maybeError $ "Expected " ++ show expectedKind ++ " but got " ++ show kind
        else (token, updatedParser)

expected :: Parser -> TokenKind -> (Token, Parser)
expected = expectError Nothing

--------------------------------------------------------------------------------
-- STMT PARSING
--------------------------------------------------------------------------------

parseStmt :: Parser -> Stmt
parseStmt parser = BlockStmt {body = []} -- TODO: implement

fromParser :: Parser -> [Stmt]
fromParser parser
  | hasTokens parser = parseStmt parser : fromParser (snd $ advance parser)
  | otherwise = []

--------------------------------------------------------------------------------
-- PARSER
--------------------------------------------------------------------------------

createParser :: [Token] -> Parser
createParser tokens = Parser {parserTokens = tokens, parserPos = 0}

parse :: [Token] -> Stmt
parse tokens =
  let body = [] :: [Stmt]
   in BlockStmt {body = body}
