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

parseArrayType :: TypeNudHandler
parseArrayType parser =
  let (_, pAfterLBracket) = expected parser LBRACKET
      (_, pAfterRBracket) = expected pAfterLBracket RBRACKET
      (underlyingType, updatedParser) = parseType pAfterRBracket DEFAULT
   in (ArrayType underlyingType, updatedParser)

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

-- TODO: make it so that it works for ArrayType as well... keep checking video from 37.05 as I think he has the same error and will try to fix it now https://www.youtube.com/watch?v=Uz8Yi_udGNY&list=PL_2VhOvlMk4XDeq2eOOSDQMrbZj9zIU_b&index=10
parseType :: Parser -> BindingPower -> (Type, Parser)
parseType parser bp =
  let (leftType, updatedParser) = parseNudType parser
   in parseLeftType updatedParser leftType bp

--------------------------------------------------------------------------------
-- HELPERS ADD HANDLERS AND BPs TO LOOKUPS
--------------------------------------------------------------------------------

getTypeBp :: TokenKind -> BindingPower
getTypeBp kind = fromMaybe NONE (Map.lookup kind (tbindingPowerLookup typeLookups)) -- error $ "Expected binding power for token " ++ show kind

--------------------------------------------------------------------------------
-- CREATE TYPE LOOKUPS
--------------------------------------------------------------------------------

-- NUD:
createTypeNudLookups :: (BindingPowerLookup, TypeNudLookup) -> (BindingPowerLookup, TypeNudLookup)
createTypeNudLookups = createArrayTypeNudLookups . createIdentifierTypeNudLookups

-- Identifier
createIdentifierTypeNudLookups :: (BindingPowerLookup, TypeNudLookup) -> (BindingPowerLookup, TypeNudLookup)
createIdentifierTypeNudLookups l = createLookups parseSymbolType DEFAULT l [IDENTIFIER]

-- Array Type f.e. []number
createArrayTypeNudLookups :: (BindingPowerLookup, TypeNudLookup) -> (BindingPowerLookup, TypeNudLookup)
createArrayTypeNudLookups l = createLookups parseArrayType DEFAULT l [LBRACKET]

-- LED:

-- Finally:
typeLookups :: TypeLookups
typeLookups = TypeLookups {tbindingPowerLookup = bp, tnudLookup = nl, tledLookup = Map.empty}
  where
    (bp, nl) = createTypeNudLookups (Map.empty, Map.empty)
