module Parser where

import Tokens

data Parser = Parser
  { parserTokens :: [Token],
    -- errors :: [Error], TODO: implement errors
    parserPos :: Int
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------

hasTokens :: Parser -> Bool
hasTokens parser =
  let pos = parserPos parser
      curr = parserTokens parser !! pos
   in pos < length (parserTokens parser) && tokenKind curr /= EOF

currentToken :: Parser -> Token
currentToken parser
  | hasTokens parser = parserTokens parser !! parserPos parser
  | otherwise = Token {tokenValue = "EOF", tokenKind = EOF}

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
