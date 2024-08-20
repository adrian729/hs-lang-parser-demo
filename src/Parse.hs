module Parse where

import AST
import Lookups
import Parser
import Tokens

--------------------------------------------------------------------------------
-- PARSE TOKENS
--------------------------------------------------------------------------------
fromParser :: Parser -> [Stmt]
fromParser parser
  | hasTokens parser =
      let (stmtItem, updatedParser) = parseStmt parser
       in stmtItem : fromParser updatedParser
  | otherwise = []

createParser :: [Token] -> Parser
createParser tokens = Parser {parserTokens = tokens, parserPos = 0}

parse :: [Token] -> Stmt
parse tokens = BlockStmt {body = fromParser $ createParser tokens}
