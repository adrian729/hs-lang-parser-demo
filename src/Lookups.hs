module Lookups where

import AST
import BindingPower
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Parser
import Tokens

-- TODO: see how to restructure or rename the module, it has all the parsing with the lookups ...

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

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
-- HANDLERS
--------------------------------------------------------------------------------

parsePrimaryExpr :: NudHandler
parsePrimaryExpr parser =
  let (current, updatedParser) = advance parser
      kind = tokenKind current
      expr = case kind of
        NUMBER -> NumberExpr (read (tokenValue current) :: Double)
        STRING -> StringExpr (tokenValue current)
        IDENTIFIER -> SymbolExpr (tokenValue current)
        _ -> error "Expected primary expression"
   in (expr, updatedParser)

parsePrefixExpr :: NudHandler
parsePrefixExpr parser =
  let (operator, pAfterOp) = advance parser
      (rightExpr, updatedParser) = parseExpr pAfterOp DEFAULT -- TODO: This should be UNARY instead of DEFAULT, need to check why it breaks...
   in (PrefixExpr {operator = operator, right = rightExpr}, updatedParser)

parseGroupingExpr :: NudHandler
parseGroupingExpr parser =
  let (_, pAfterOpen) = expected parser LPAREN
      (expr, pAfterExpr) = parseExpr pAfterOpen DEFAULT
      (_, updatedParser) = expected pAfterExpr RPAREN
   in (expr, updatedParser)

parseBinaryExpr :: LedHandler
parseBinaryExpr parser left bp =
  let (operator, updatedParser') = advance parser
      (right, updatedParser) = parseExpr updatedParser' bp -- Not clear if it should be DEFAULT or bp
      expr = BinaryExpr {left = left, operator = operator, right = right}
   in (expr, updatedParser)

parseAssignmentExpr :: LedHandler
parseAssignmentExpr parser left bp =
  let (operator, pAfterOp) = advance parser
      (valExpr, updatedParser) = parseExpr pAfterOp bp
   in (AssignmentExpr {assigne = left, operator = operator, value = valExpr}, updatedParser)

--------------------------------------------------------------------------------
-- PARSE NUD
--------------------------------------------------------------------------------

getNudHandler :: TokenKind -> NudHandler
getNudHandler kind = case Map.lookup kind (nudLookup lookups) of
  Just nudHandler -> nudHandler
  Nothing -> error $ "Expected nud handler for token " ++ show kind

parseNudExpr :: Parser -> (Expr, Parser)
parseNudExpr parser =
  let kind = currentTokenKind parser
      nudHandler = getNudHandler kind
   in nudHandler parser

--------------------------------------------------------------------------------
-- PARSE LED/BINARY EXPR
--------------------------------------------------------------------------------

getLedHandler :: TokenKind -> LedHandler
getLedHandler kind = case Map.lookup kind (ledLookup lookups) of
  Just ledHandler -> ledHandler
  Nothing -> error $ "Expected led handler for token " ++ show kind

parseLeftExpr :: Parser -> Expr -> BindingPower -> (Expr, Parser)
parseLeftExpr parser leftExpr bp =
  let kind = currentTokenKind parser
      currBp = getBp kind
   in if currBp > bp
        then
          let ledHandler = getLedHandler kind
           in ledHandler parser leftExpr currBp
        else (leftExpr, parser)

parseExpr :: Parser -> BindingPower -> (Expr, Parser)
parseExpr parser bp =
  let (leftExpr, updatedParser) = parseNudExpr parser
   in parseLeftExpr updatedParser leftExpr bp

--------------------------------------------------------------------------------
-- PARSE STMT
--------------------------------------------------------------------------------

getStmtHandler :: TokenKind -> Maybe StmtHandler
getStmtHandler kind = Map.lookup kind (stmtLookup lookups)

parseExprStmt :: Parser -> (Stmt, Parser)
parseExprStmt parser =
  let (expr, updatedParser') = parseExpr parser DEFAULT
      (_, updatedParser) = expected updatedParser' SEMI_COLON
   in (ExprStmt expr, updatedParser)

parseStmt :: Parser -> (Stmt, Parser)
parseStmt parser =
  let kind = currentTokenKind parser
      maybeStmtHandler = getStmtHandler kind
   in case maybeStmtHandler of
        Just stmtHandler -> stmtHandler parser
        Nothing -> parseExprStmt parser

isConstant :: TokenKind -> Bool
isConstant kind = kind == CONST

getVarDeclName :: Parser -> (String, Parser)
getVarDeclName parser =
  let errMsg = Just "Inside variable declaration expected to find variable name"
      (varNameToken, pAfterVarName) = expectError errMsg parser IDENTIFIER
      (_, pAfterAssig) = expected pAfterVarName ASSIGNMENT
   in (tokenValue varNameToken, pAfterAssig)

parseVarDeclStmt :: Parser -> (Stmt, Parser)
parseVarDeclStmt parser =
  let (varDeclToken, pAfterVarDecl) = advance parser
      isConstDecl = isConstant $ tokenKind varDeclToken
      (varName, pAfterAssig) = getVarDeclName pAfterVarDecl
      (assignedExpr, pAfterExpr) = parseExpr pAfterAssig ASSIG
      (_, updatedParser) = expected pAfterExpr SEMI_COLON
   in (VarDeclStmt {name = varName, isConst = isConstDecl, assignedVal = assignedExpr}, updatedParser)

-- parseExpr :: Parser -> BindingPower -> Lookups -> (Expr, Parser)

--------------------------------------------------------------------------------
-- HELPERS ADD HANDLERS AND BPs TO LOOKUPS
--------------------------------------------------------------------------------

getBp :: TokenKind -> BindingPower
getBp kind = fromMaybe NONE (Map.lookup kind (bindingPowerLookup lookups)) -- error $ "Expected binding power for token " ++ show kind

addLookup :: BindingPower -> TokenKind -> a -> (BindingPowerLookup, Map TokenKind a) -> (BindingPowerLookup, Map TokenKind a)
addLookup bp kind handler (bpLookup, handlerLookup) = (Map.insert kind bp bpLookup, Map.insert kind handler handlerLookup)

--------------------------------------------------------------------------------
-- CREATE LOOKUPS
--------------------------------------------------------------------------------

createLookups' :: a -> BindingPower -> (BindingPowerLookup, Map TokenKind a) -> [TokenKind] -> (BindingPowerLookup, Map TokenKind a)
createLookups' handler bp = foldr (\x -> addLookup bp x handler)

-- NUD:
createNudLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudLookups = createNudGroupingLookups . createNudPrefixLookups . createNudPrimaryLookups

-- Literals & symbols
createNudPrimaryLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudPrimaryLookups l = createLookups' parsePrimaryExpr PRIMARY l [NUMBER, STRING, IDENTIFIER]

createNudPrefixLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudPrefixLookups l = createLookups' parsePrefixExpr UNARY l [MINUS]

-- Grouping
createNudGroupingLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudGroupingLookups l = createLookups' parseGroupingExpr DEFAULT l [LPAREN]

-- LED:
createLedLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLookups = createLedMultiplicativeLookups . createLedAdditiveLookups . createLedRelationalLookups . createLedLogicalLookups . createLedAssignmentLookups

-- Assignment
createLedAssignmentLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedAssignmentLookups l = createLookups' parseAssignmentExpr ASSIG l [ASSIGNMENT, PLUS_EQUALS, MINUS_EQUALS, TIMES_EQUALS, DIVIDE_EQUALS, MODULO_EQUALS, POWER_EQUALS]

-- Logical
createLedLogicalLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLogicalLookups l = createLookups' parseBinaryExpr LOGICAL l [AND, OR, ELLIPSIS]

-- Relational
createLedRelationalLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedRelationalLookups l = createLookups' parseBinaryExpr RELATIONAL l [EQUAL, NOT_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL]

-- Additive
createLedAdditiveLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedAdditiveLookups l = createLookups' parseBinaryExpr ADDITIVE l [PLUS, MINUS]

-- Multiplicative
createLedMultiplicativeLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedMultiplicativeLookups l = createLookups' parseBinaryExpr MULTIPLICATIVE l [TIMES, DIVIDE, MODULO]

-- STMT:
createStmtLookups :: (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
createStmtLookups = createStmtVarDeclLookups

-- Var Decl
createStmtVarDeclLookups :: (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
createStmtVarDeclLookups l = createLookups' parseVarDeclStmt DEFAULT l [CONST, LET]

-- Finally
-- TODO: implement this, we need to create each lookup and have the bindingPower shared/updated for all lookups
lookups :: Lookups
lookups = Lookups {bindingPowerLookup = bp, nudLookup = nl, ledLookup = ll, stmtLookup = sl}
  where
    (bp'', nl) = createNudLookups (Map.empty, Map.empty)
    (bp', ll) = createLedLookups (bp'', Map.empty)
    (bp, sl) = createStmtLookups (bp', Map.empty)
