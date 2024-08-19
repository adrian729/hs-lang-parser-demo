module Lookups where

import AST
import BindingPower
import Data.Map (Map)
import qualified Data.Map as Map
import Parser
import Tokens

-- TODO: see how to restructure or rename the module, it has all the parsing with the lookups ...

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

type NudHandler = Parser -> Lookups -> (Expr, Parser)

type LedHandler = Parser -> Expr -> BindingPower -> Lookups -> (Expr, Parser)

type StmtHandler = Parser -> Lookups -> (Stmt, Parser)

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
parsePrimaryExpr parser _ =
  let (current, updatedParser) = advance parser
      kind = tokenKind current
      expr = case kind of
        NUMBER -> NumberExpr (read (tokenValue current) :: Double)
        STRING -> StringExpr (tokenValue current)
        IDENTIFIER -> SymbolExpr (tokenValue current)
        _ -> error "Expected primary expression"
   in (expr, updatedParser)

parseBinaryExpr :: LedHandler
parseBinaryExpr parser left bp lookups =
  let (operator, updatedParser') = advance parser
      (right, updatedParser) = parseExpr updatedParser' bp lookups -- Not clear if it should be DEFAULT or bp
      expr = BinaryExpr {left = left, operator = operator, right = right}
   in (expr, updatedParser)

parseAssignmentExpr :: LedHandler
parseAssignmentExpr parser left bp lookups =
  let (operator, pAfterOp) = advance parser
      (valExpr, updatedParser) = parseExpr pAfterOp bp lookups
   in (AssignmentExpr {assigne = left, operator = operator, value = valExpr}, updatedParser)

parsePrefixExpr :: NudHandler
parsePrefixExpr parser lookups =
  let (operator, pAfterOp) = advance parser
      (rightExpr, updatedParser) = parseExpr pAfterOp DEFAULT lookups
   in (PrefixExpr {operator = operator, right = rightExpr}, updatedParser)

--------------------------------------------------------------------------------
-- HELPERS ADD HANDLERS AND BPs TO LOOKUPS
--------------------------------------------------------------------------------

addBindingPower :: TokenKind -> BindingPower -> BindingPowerLookup -> BindingPowerLookup
addBindingPower = Map.insert

addDefaultBindingPower :: TokenKind -> BindingPowerLookup -> BindingPowerLookup
addDefaultBindingPower kind = addBindingPower kind DEFAULT

addLed :: TokenKind -> LedHandler -> LedLookup -> LedLookup
addLed = Map.insert

addStmt :: TokenKind -> StmtHandler -> StmtLookup -> StmtLookup
addStmt = Map.insert

led :: TokenKind -> BindingPower -> LedHandler -> (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
led kind bp handler (bpLookup, ledLookup) = (addBindingPower kind bp bpLookup, addLed kind handler ledLookup)

nud :: TokenKind -> NudHandler -> NudLookup -> NudLookup
nud = Map.insert

stmt :: TokenKind -> StmtHandler -> (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
stmt kind handler (bpLookup, stmtLookup) = (addBindingPower kind DEFAULT bpLookup, addStmt kind handler stmtLookup)

--------------------------------------------------------------------------------
-- PARSE NUD
--------------------------------------------------------------------------------

getNudHandler :: TokenKind -> Lookups -> NudHandler
getNudHandler kind lookups = case Map.lookup kind (nudLookup lookups) of
  Just nudHandler -> nudHandler
  Nothing -> error $ "Expected nud handler for token " ++ show kind

parseNudExpr :: Parser -> Lookups -> (Expr, Parser)
parseNudExpr parser lookups =
  let kind = currentTokenKind parser
      nudHandler = getNudHandler kind lookups
   in nudHandler parser lookups

--------------------------------------------------------------------------------
-- PARSE LED/BINARY EXPR
--------------------------------------------------------------------------------

getLedHandler :: TokenKind -> Lookups -> LedHandler
getLedHandler kind lookups = case Map.lookup kind (ledLookup lookups) of
  Just ledHandler -> ledHandler
  Nothing -> error $ "Expected led handler for token " ++ show kind

isLeft :: TokenKind -> BindingPower -> Lookups -> (BindingPower, Bool)
isLeft kind bp lookups =
  case Map.lookup kind (bindingPowerLookup lookups) of
    Just currBp -> (currBp, currBp > bp)
    Nothing -> error $ "Expected binding power for token " ++ show kind

parseLeftExpr :: Parser -> Expr -> BindingPower -> Lookups -> (Expr, Parser)
parseLeftExpr parser leftExpr bp lookups =
  let kind = currentTokenKind parser
      (leftBp, isLeftExpr) = isLeft kind bp lookups
   in if kind /= SEMI_COLON && isLeftExpr
        then
          let ledHandler = getLedHandler kind lookups
           in ledHandler parser leftExpr leftBp lookups
        else (leftExpr, parser)

parseExpr :: Parser -> BindingPower -> Lookups -> (Expr, Parser)
parseExpr parser bp lookups =
  let (leftExpr, updatedParser) = parseNudExpr parser lookups
   in parseLeftExpr updatedParser leftExpr bp lookups

--------------------------------------------------------------------------------
-- PARSE STMT
--------------------------------------------------------------------------------

getStmtHandler :: TokenKind -> Lookups -> Maybe StmtHandler
getStmtHandler kind lookups = Map.lookup kind (stmtLookup lookups)

parseExprStmt :: Parser -> Lookups -> (Stmt, Parser)
parseExprStmt parser lookups =
  let (expr, updatedParser') = parseExpr parser DEFAULT lookups
      (_, updatedParser) = expected updatedParser' SEMI_COLON
   in (ExprStmt expr, updatedParser)

parseStmt :: Parser -> Lookups -> (Stmt, Parser)
parseStmt parser lookups =
  let kind = currentTokenKind parser
      maybeStmtHandler = getStmtHandler kind lookups
   in case maybeStmtHandler of
        Just stmtHandler -> stmtHandler parser lookups
        Nothing -> parseExprStmt parser lookups

isConstant :: TokenKind -> Bool
isConstant kind = kind == CONST

getVarDeclName :: Parser -> (String, Parser)
getVarDeclName parser =
  let errMsg = Just "Inside variable declaration expected to find variable name"
      (varNameToken, pAfterVarName) = expectError errMsg parser IDENTIFIER
      (_, pAfterAssig) = expected pAfterVarName ASSIGNMENT
   in (tokenValue varNameToken, pAfterAssig)

parseVarDeclStmt :: Parser -> Lookups -> (Stmt, Parser)
parseVarDeclStmt parser lookups =
  let (varDeclToken, pAfterVarDecl) = advance parser
      isConstDecl = isConstant $ tokenKind varDeclToken
      (varName, pAfterAssig) = getVarDeclName pAfterVarDecl
      (assignedExpr, pAfterExpr) = parseExpr pAfterAssig ASSIG lookups
      (_, updatedParser) = expected pAfterExpr SEMI_COLON
   in (VarDeclStmt {name = varName, isConst = isConstDecl, assignedVal = assignedExpr}, updatedParser)

-- parseExpr :: Parser -> BindingPower -> Lookups -> (Expr, Parser)

--------------------------------------------------------------------------------
-- CREATE LOOKUPS
--------------------------------------------------------------------------------

-- NUD:
-- nud :: TokenKind -> NudHandler -> (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)

-- Literals & symbols
createNudLookups :: NudLookup -> NudLookup
createNudLookups = createNudPrefixLookups . createNudPrimaryLookups

createNudLookups' :: NudHandler -> [TokenKind] -> NudLookup -> NudLookup
createNudLookups' handler kinds nl = foldr (`nud` handler) nl kinds

createNudPrimaryLookups :: NudLookup -> NudLookup
createNudPrimaryLookups = createNudLookups' parsePrimaryExpr [NUMBER, STRING, IDENTIFIER]

createNudPrefixLookups :: NudLookup -> NudLookup
createNudPrefixLookups = createNudLookups' parsePrefixExpr [MINUS]

-- LED:
-- led :: TokenKind -> BindingPower -> LedHandler -> (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLookups = createLedMultiplicativeLookups . createLedAdditiveLookups . createLedRelationalLookups . createLedLogicalLookups . createLedAssignmentLookups

createLedLookups' :: LedHandler -> BindingPower -> [TokenKind] -> (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLookups' handler bp kinds (bpl, ll) = foldr (\x -> led x bp handler) (bpl, ll) kinds

-- Assignment
createLedAssignmentLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedAssignmentLookups = createLedLookups' parseAssignmentExpr ASSIG [ASSIGNMENT, PLUS_EQUALS, MINUS_EQUALS, TIMES_EQUALS, DIVIDE_EQUALS, MODULO_EQUALS, POWER_EQUALS]

-- Logical
createLedLogicalLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLogicalLookups = createLedLookups' parseBinaryExpr LOGICAL [AND, OR, ELLIPSIS]

-- Relational
createLedRelationalLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedRelationalLookups = createLedLookups' parseBinaryExpr RELATIONAL [EQUAL, NOT_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL]

-- Additive
createLedAdditiveLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedAdditiveLookups = createLedLookups' parseBinaryExpr ADDITIVE [PLUS, MINUS]

-- Multiplicative
createLedMultiplicativeLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedMultiplicativeLookups = createLedLookups' parseBinaryExpr MULTIPLICATIVE [TIMES, DIVIDE, MODULO]

-- STMT:
-- stmt :: TokenKind -> StmtHandler -> (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
createStmtLookups :: (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
createStmtLookups = createStmtVarDeclLookups

createStmtVarDeclLookups :: (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
createStmtVarDeclLookups (bpl, sl) = foldr (`stmt` parseVarDeclStmt) (bpl, sl) [CONST, LET]

-- Finally
-- TODO: implement this, we need to create each lookup and have the bindingPower shared/updated for all lookups
createLookups :: Lookups
createLookups = Lookups {bindingPowerLookup = bp, nudLookup = nl, ledLookup = ll, stmtLookup = sl}
  where
    nl = createNudLookups Map.empty
    (bp', ll) = createLedLookups (Map.empty, Map.empty)
    (bp, sl) = createStmtLookups (bp', Map.empty)
