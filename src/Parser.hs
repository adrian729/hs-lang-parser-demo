module Parser where

import AST
import BindingPower
import Data.Map (Map)
import qualified Data.Map as Map
import Tokens

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

data Parser = Parser
  { parserTokens :: [Token],
    -- errors :: [Error], TODO: implement errors
    parserPos :: Int
  }
  deriving (Show)

type NudHandler = Parser -> (Expr, Parser)

type LedHandler = Parser -> Expr -> BindingPower -> (Expr, Parser)

type StmtHandler = Parser -> (Stmt, Parser)

type BindingPowerLookup = Map TokenKind BindingPower

type NudLookup = Map TokenKind NudHandler

type LedLookup = Map TokenKind LedHandler

type StmtLookup = Map TokenKind StmtHandler

data Lookups = Lookups
  { bindingPowerLookup :: BindingPowerLookup,
    nudLookup :: NudLookup,
    ledLookup :: LedLookup,
    stmtLookup :: StmtLookup
  }

--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------

-- parser

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

-- lookups
addLookup :: BindingPower -> TokenKind -> a -> (BindingPowerLookup, Map TokenKind a) -> (BindingPowerLookup, Map TokenKind a)
addLookup bp kind handler (bpLookup, handlerLookup) = (Map.insert kind bp bpLookup, Map.insert kind handler handlerLookup)

createLookups :: a -> BindingPower -> (BindingPowerLookup, Map TokenKind a) -> [TokenKind] -> (BindingPowerLookup, Map TokenKind a)
createLookups handler bp = foldr (\x -> addLookup bp x handler)
