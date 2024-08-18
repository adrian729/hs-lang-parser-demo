module Lookups where

import AST
import BindingPower
import Data.Map (Map)
import qualified Data.Map as Map
import Parser
import Tokens

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

type NudHandler = Parser -> (Expr, Parser)

type LedHandler = Parser -> Expr -> BindingPower -> Lookups -> (Expr, Parser)

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

parseBinaryExpr :: LedHandler
parseBinaryExpr parser left bp lookups =
  let (current, parser') = advance parser
      (newExpr, updatedParser) = parseExpr parser' bp lookups
      expr = BinaryExpr {left = left, operator = current, right = newExpr}
   in (expr, updatedParser)

--------------------------------------------------------------------------------
-- HELPERS ADD HANDLERS AND BPs TO LOOKUPS
--------------------------------------------------------------------------------

addBindingPower :: TokenKind -> BindingPower -> BindingPowerLookup -> BindingPowerLookup
addBindingPower = Map.insert

addDefaultBindingPower :: TokenKind -> BindingPowerLookup -> BindingPowerLookup
addDefaultBindingPower kind = addBindingPower kind DEFAULT

addLed :: TokenKind -> LedHandler -> LedLookup -> LedLookup
addLed = Map.insert

addNud :: TokenKind -> NudHandler -> NudLookup -> NudLookup
addNud = Map.insert

addStmt :: TokenKind -> StmtHandler -> StmtLookup -> StmtLookup
addStmt = Map.insert

led :: TokenKind -> BindingPower -> LedHandler -> (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
led kind bp handler (bpLookup, ledLookup) = (addBindingPower kind bp bpLookup, addLed kind handler ledLookup)

nud :: TokenKind -> NudHandler -> (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
nud kind handler (bpLookup, nudLookup) = (addBindingPower kind PRIMARY bpLookup, addNud kind handler nudLookup)

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
   in nudHandler parser

--------------------------------------------------------------------------------
-- PARSE LED/BINARY EXPR
--------------------------------------------------------------------------------

getLedHandler :: TokenKind -> Lookups -> LedHandler
getLedHandler kind lookups = case Map.lookup kind (ledLookup lookups) of
  Just ledHandler -> ledHandler
  Nothing -> error $ "Expected led handler for token " ++ show kind

isLeft :: TokenKind -> BindingPower -> Lookups -> Bool
isLeft kind bp lookups =
  case Map.lookup kind (bindingPowerLookup lookups) of
    Just currBp -> currBp > bp
    Nothing -> error $ "Expected binding power for token " ++ show kind

parseLeftExpr :: Parser -> Expr -> BindingPower -> Lookups -> (Expr, Parser)
parseLeftExpr parser leftExpr bp lookups =
  let kind = currentTokenKind parser
   in if isLeft kind bp lookups
        then
          let ledHandler = getLedHandler kind lookups
           in ledHandler parser leftExpr bp lookups
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
        Just stmtHandler -> stmtHandler parser
        Nothing -> parseExprStmt parser lookups

--------------------------------------------------------------------------------
-- CREATE LOOKUPS
--------------------------------------------------------------------------------

-- TODO: WE WERE DOING THIS, https://www.youtube.com/watch?v=1BanGrbOcjs&list=PL_2VhOvlMk4XDeq2eOOSDQMrbZj9zIU_b&index=12 at 39.53

-- led :: TokenKind -> BindingPower -> LedHandler -> (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
-- nud :: TokenKind -> NudHandler -> (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
-- stmt :: TokenKind -> StmtHandler -> (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)

-- NUD:
-- Literals & symbols
createNudLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudLookups (bp, nl) = foldr (`nud` parsePrimaryExpr) (bp, nl) [NUMBER, STRING, IDENTIFIER]

-- LED:
createLedLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLookups = createLedMultiplicativeLookups . createLedAdditiveLookups . createLedRelationalLookups . createLedLogicalLookups

createLedKindLookups :: BindingPower -> [TokenKind] -> (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedKindLookups bp kinds (bpLookup, ledLookup) = foldr (\x -> led x bp parseBinaryExpr) (bpLookup, ledLookup) kinds

-- Logical
createLedLogicalLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLogicalLookups = createLedKindLookups LOGICAL [AND, OR, ELLIPSIS]

-- Relational
createLedRelationalLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedRelationalLookups = createLedKindLookups RELATIONAL [EQUAL, NOT_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL]

-- Additive
createLedAdditiveLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedAdditiveLookups = createLedKindLookups ADDITIVE [PLUS, MINUS]

-- Multiplicative
createLedMultiplicativeLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedMultiplicativeLookups = createLedKindLookups MULTIPLICATIVE [TIMES, DIVIDE, MODULO]

-- STMT:
-- TODO: ADD STMT HANDLERS TO LOOKUPS

-- Finally
-- TODO: implement this, we need to create each lookup and have the bindingPower shared/updated for all lookups
createLookups :: Lookups
createLookups = Lookups {bindingPowerLookup = bp, nudLookup = nl, ledLookup = ll, stmtLookup = sl}
  where
    (bp', nl) = createNudLookups (Map.empty, Map.empty)
    (bp, ll) = createLedLookups (bp', Map.empty)
    sl = Map.empty
