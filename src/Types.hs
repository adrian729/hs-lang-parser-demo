module Types where

import AST
import BindingPower
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Parser
import Tokens

type TypeNudHandler = Parser -> (Type, Parser)

type TypeLedHandler = Parser -> Type -> BindingPower -> (Type, Parser)

type TypeNudLookup = Map TokenKind TypeNudHandler

type TypeLedLookup = Map TokenKind TypeLedHandler

data TypeLookups = TypeLookups
  { tbindingPowerLookup :: BindingPowerLookup,
    tnudLookup :: TypeNudLookup,
    tledLookup :: TypeLedLookup
  }

--------------------------------------------------------------------------------
-- HANDLERS
--------------------------------------------------------------------------------

parseSymbolType :: TypeNudHandler
parseSymbolType parser =
  let (identifier, updatedParser) = expected parser IDENTIFIER
      value = tokenValue identifier
   in (SymbolType value, updatedParser)

-- For ArrayType as nud, f.e. []number
-- parseArrayType :: TypeNudHandler
-- parseArrayType parser =
--   let (_, pAfterLBracket) = expected parser LBRACKET
--       (_, pAfterRBracket) = expected pAfterLBracket RBRACKET
--       (underlyingType, updatedParser) = parseType pAfterRBracket DEFAULT
--    in (ArrayType underlyingType, updatedParser)

-- For ArrayType as led, f.e. number[]
parseArrayType :: TypeLedHandler
parseArrayType parser left _ =
  let (_, pAfterLBracket) = expected parser LBRACKET
      (_, updatedParser) = expected pAfterLBracket RBRACKET
   in (ArrayType left, updatedParser)

--------------------------------------------------------------------------------
-- PARSE NUD
--------------------------------------------------------------------------------

kindTypeNudHandler :: TokenKind -> TypeNudHandler
kindTypeNudHandler kind = case Map.lookup kind (tnudLookup typeLookups) of
  Just tnudHandler -> tnudHandler
  Nothing -> error $ "Expected nud handler for token " ++ show kind

parseNudType :: Parser -> (Type, Parser)
parseNudType parser =
  let kind = currentTokenKind parser
      tnudHandler = kindTypeNudHandler kind
   in tnudHandler parser

--------------------------------------------------------------------------------
-- PARSE LED/BINARY TYPE
--------------------------------------------------------------------------------
-- TODO: I think this impl is overkill, check it again and refactor/simplify.. it was copypasted from Lookups
kindTypeLedHandler :: TokenKind -> TypeLedHandler
kindTypeLedHandler kind = case Map.lookup kind (tledLookup typeLookups) of
  Just tledHandler -> tledHandler
  Nothing -> error $ "Expected type led handler for token " ++ show kind

parseLeftType :: Parser -> Type -> BindingPower -> (Type, Parser)
parseLeftType parser leftType bp
  -- do > for more balanced/less levels in the AST, >= for right-heavy with more levels tree when same bp
  | currBp > bp =
      let (ltype, updatedParser) = kindTypeLedHandler kind parser leftType currBp
       in parseLeftType updatedParser ltype bp
  | otherwise = (leftType, parser)
  where
    kind = currentTokenKind parser
    currBp = getTypeBp kind

--------------------------------------------------------------------------------
-- PARSE
--------------------------------------------------------------------------------

parseType :: Parser -> BindingPower -> (Type, Parser)
parseType parser bp =
  let (leftType, updatedParser) = parseNudType parser
   in parseLeftType updatedParser leftType bp

--------------------------------------------------------------------------------
-- HELPERS ADD HANDLERS AND BPs TO LOOKUPS
--------------------------------------------------------------------------------

getTypeBp :: TokenKind -> BindingPower
getTypeBp kind = fromMaybe DEFAULT (Map.lookup kind (tbindingPowerLookup typeLookups)) -- error $ "Expected binding power for token " ++ show kind

--------------------------------------------------------------------------------
-- CREATE TYPE LOOKUPS
--------------------------------------------------------------------------------

-- NUD:
createTypeNudLookups :: (BindingPowerLookup, TypeNudLookup) -> (BindingPowerLookup, TypeNudLookup)
createTypeNudLookups = createIdentifierTypeNudLookups

-- Identifier
createIdentifierTypeNudLookups :: (BindingPowerLookup, TypeNudLookup) -> (BindingPowerLookup, TypeNudLookup)
createIdentifierTypeNudLookups l = createLookups parseSymbolType PRIMARY l [IDENTIFIER]

-- For Array Type as nud, f.e. []number
-- createArrayTypeNudLookups :: (BindingPowerLookup, TypeNudLookup) -> (BindingPowerLookup, TypeNudLookup)
-- createArrayTypeNudLookups l = createLookups parseArrayType DEFAULT l [LBRACKET]

-- LED:
createTypeLedLookups :: (BindingPowerLookup, TypeLedLookup) -> (BindingPowerLookup, TypeLedLookup)
createTypeLedLookups = createArrayTypeLedLookups

-- For Array Type as led, f.e. number[]
createArrayTypeLedLookups :: (BindingPowerLookup, TypeLedLookup) -> (BindingPowerLookup, TypeLedLookup)
createArrayTypeLedLookups l = createLookups parseArrayType MEMBER l [LBRACKET]

-- Finally:
typeLookups :: TypeLookups
typeLookups = TypeLookups {tbindingPowerLookup = bp, tnudLookup = nl, tledLookup = ll}
  where
    (bp', nl) = createTypeNudLookups (Map.empty, Map.empty)
    (bp, ll) = createTypeLedLookups (bp', Map.empty)
