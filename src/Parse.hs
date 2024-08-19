module Parse where

import AST
import Lookups
import Parser
import Tokens

--------------------------------------------------------------------------------
-- PARSE TOKENS
--------------------------------------------------------------------------------
fromParser :: Parser -> Lookups -> [Stmt]
fromParser parser lookups
  | hasTokens parser && currentTokenKind parser /= EOF =
      let (stmtItem, updatedParser) = parseStmt parser lookups
       in stmtItem : fromParser updatedParser lookups
  | otherwise = []

createParser :: [Token] -> Parser
createParser tokens = Parser {parserTokens = tokens, parserPos = 0}

parse :: [Token] -> Stmt
parse tokens = BlockStmt {body = fromParser (createParser tokens) createLookups}
