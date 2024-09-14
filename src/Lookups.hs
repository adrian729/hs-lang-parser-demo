module Lookups where

import AST
import BindingPower as BP
import Data.Map
import qualified Data.Map as Map
import Data.Maybe
import Parser
import Tokens
import Types

-- TODO: see how to restructure or rename the module, it has all the parsing with the lookups ...

insertIfNotMember :: (Ord k, Show k) => String -> k -> a -> Map k a -> Map k a
insertIfNotMember errMsg key value m
  | notMember key m = Map.insert key value m
  | otherwise = error errMsg

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

-- parseNudExpr :: Parser -> (Expr, Parser)
parsePrefixExpr :: NudHandler
parsePrefixExpr parser =
  let (operator, pAfterOp) = advance parser
      (rightExpr, updatedParser) = parseNudExpr pAfterOp
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

-- STRUCT INSTANTIATION EXPR // ARRAY LITERAL EXPR

parseStructInstProps :: (Map String Expr, Parser) -> (Map String Expr, Parser)
parseStructInstProps (props, parser)
  | hasTokens parser && kind /= RBRACE =
      let (propNameToken, pAfterPropName) = expected parser IDENTIFIER
          (_, pAfterColon) = expected pAfterPropName COLON
          (propExpr, pAfterPropExpr) = parseExpr pAfterColon LOGICAL -- minimum BP for prop expr
          (_, updatedParser) =
            if currentTokenKind pAfterPropExpr /= RBRACE
              then expected pAfterPropExpr Tokens.COMMA
              else (currentToken pAfterPropExpr, pAfterPropExpr)
       in parseStructInstProps (Map.insert (tokenValue propNameToken) propExpr props, updatedParser)
  | otherwise = (props, parser)
  where
    kind = currentTokenKind parser

parseStructInstantiation :: LedHandler
parseStructInstantiation parser (SymbolExpr structName) _ =
  let (_, pAfterLBrace) = expected parser LBRACE
      (structInstanceProps, pAfterProps) = parseStructInstProps (Map.empty, pAfterLBrace)
      (_, updatedParser) = expected pAfterProps RBRACE
   in (StructInstantiationExpr {structName = structName, structInstanceProps = structInstanceProps}, updatedParser) -- TODO: impl
parseStructInstantiation _ notSymbolExpr _ = error $ "Expected symbol expression for struct instantiation but found " ++ show notSymbolExpr

-- ARRAY LITERAL/INSTANTIATION EXPR

parseNestedArray :: Parser -> Type -> (Type, Parser)
parseNestedArray parser underlyingType
  | kind == LBRACKET =
      let (_, pAfterLBracket) = expected parser LBRACKET
          (_, pAfterRBracket) = expected pAfterLBracket RBRACKET
       in parseNestedArray pAfterRBracket (ArrayType underlyingType)
  | otherwise = (underlyingType, parser)
  where
    kind = currentTokenKind parser

parseArrayContent :: Parser -> ([Expr], Parser)
parseArrayContent parser
  | hasTokens parser && kind /= RBRACE =
      let (expr, pAfterExpr) = parseExpr parser LOGICAL
       in if currentTokenKind pAfterExpr /= RBRACE
            then
              let (_, pAfterComma) = expected pAfterExpr Tokens.COMMA
                  (nextContent, updatedParser) = parseArrayContent pAfterComma
               in (expr : nextContent, updatedParser)
            else ([expr], pAfterExpr)
  | otherwise = ([], parser)
  where
    kind = currentTokenKind parser

parseArrayLiteral :: LedHandler
parseArrayLiteral parser (SymbolExpr structName) _ =
  let (arrayType, pAfterArrayType) = parseNestedArray parser (SymbolType structName)
      (_, pAfterLBrace) = expected pAfterArrayType LBRACE
      (content, pAfterContent) = parseArrayContent pAfterLBrace
      (_, updatedParser) = expected pAfterContent RBRACE
   in (ArrayLiteralExpr {arrayType = arrayType, content = content}, updatedParser)
parseArrayLiteral _ notSymbolExpr _ = error $ "Expected symbol expression for array literal but found " ++ show notSymbolExpr

--------------------------------------------------------------------------------
-- PARSE NUD
--------------------------------------------------------------------------------

kindNudHandler :: TokenKind -> NudHandler
kindNudHandler kind = case Map.lookup kind (nudLookup lookups) of
  Just nudHandler -> nudHandler
  Nothing -> error $ "Expected nud handler for token " ++ show kind

parseNudExpr :: Parser -> (Expr, Parser)
parseNudExpr parser =
  let kind = currentTokenKind parser
      nudHandler = kindNudHandler kind
   in nudHandler parser

--------------------------------------------------------------------------------
-- PARSE LED/BINARY EXPR
--------------------------------------------------------------------------------

kindLedHandler :: TokenKind -> LedHandler
kindLedHandler kind = case Map.lookup kind (ledLookup lookups) of
  Just ledHandler -> ledHandler
  Nothing -> error $ "Expected led handler for token " ++ show kind

parseLeftExpr :: Parser -> Expr -> BindingPower -> (Expr, Parser)
parseLeftExpr parser leftExpr bp
  -- do > for more balanced/less levels in the AST, >= for right-heavy with more levels tree when same bp
  | currBp > bp =
      let (expr, updatedParser) = kindLedHandler kind parser leftExpr currBp
       in parseLeftExpr updatedParser expr bp
  | otherwise = (leftExpr, parser)
  where
    kind = currentTokenKind parser
    currBp = getBp kind

parseExpr :: Parser -> BindingPower -> (Expr, Parser)
parseExpr parser bp =
  let (leftExpr, updatedParser) = parseNudExpr parser
   in parseLeftExpr updatedParser leftExpr bp

--------------------------------------------------------------------------------
-- PARSE STMT
--------------------------------------------------------------------------------

kindStmtHandler :: TokenKind -> Maybe StmtHandler
kindStmtHandler kind = Map.lookup kind (stmtLookup lookups)

parseExprStmt :: Parser -> (Stmt, Parser)
parseExprStmt parser =
  let (expr, pAfterExpr) = parseExpr parser DEFAULT
      (_, updatedParser) = expected pAfterExpr SEMI_COLON
   in (ExprStmt expr, updatedParser)

parseStmt :: Parser -> (Stmt, Parser)
parseStmt parser =
  let kind = currentTokenKind parser
      maybeStmtHandler = kindStmtHandler kind
   in case maybeStmtHandler of
        Just stmtHandler -> stmtHandler parser
        Nothing -> parseExprStmt parser

-- VAR DECL STMT

getType :: Parser -> (Maybe Type, Parser)
getType parser =
  if currentTokenKind parser == COLON
    then
      let (_, pAfterColon) = advance parser
          (typeToken, pAfterType) = parseType pAfterColon DEFAULT
       in (Just typeToken, pAfterType)
    else (Nothing, parser)

validateEmptyVarDecl :: Parser -> Maybe Type -> Bool -> (Maybe Expr, Parser)
validateEmptyVarDecl parser maybeType isConstant
  | isConstant = error "Cannot define constant without providing a value"
  | currentTokenKind parser == SEMI_COLON && isNothing maybeType = error "Missing either right-hand side of the assignment or type annotation"
  | otherwise = (Nothing, parser)

parseVarDeclAssign :: Parser -> Maybe Type -> Bool -> (Maybe Expr, Parser)
parseVarDeclAssign parser maybeType isConstant
  | currentTokenKind parser == ASSIGNMENT =
      let (_, pAfterAssig) = expected parser ASSIGNMENT
          (expr, updatedParser) = parseExpr pAfterAssig ASSIG
       in (Just expr, updatedParser)
  | otherwise = validateEmptyVarDecl parser maybeType isConstant

parseVarDeclStmt :: Parser -> (Stmt, Parser)
parseVarDeclStmt parser =
  let (varDeclToken, pAfterVarDecl) = advance parser
      isConstant = CONST == tokenKind varDeclToken
      errMsg = Just "Inside variable declaration expected to find variable name"
      (varNameToken, pAfterVarName) = expectError errMsg pAfterVarDecl IDENTIFIER
      (maybeType, pAfterType) = getType pAfterVarName
      (maybeExpr, pAfterAssig) = parseVarDeclAssign pAfterType maybeType isConstant
      (_, updatedParser) = expected pAfterAssig SEMI_COLON
   in ( VarDeclStmt
          { name = tokenValue varNameToken,
            isConst = isConstant,
            assignedVal = maybeExpr,
            explicitType = maybeType
          },
        updatedParser
      )

-- STRUCT DECL STMT

parseStructProp :: Parser -> Bool -> (String, StructDeclProperty, Parser)
parseStructProp parser isStatic =
  let (nameToken, pAfterName) = expected parser IDENTIFIER
      errMsg = "Expected to find colon following property name inside struct declaration"
      (_, pAfterColon) = expectError (Just errMsg) pAfterName COLON
      (propertyType, pAfterStructType) = parseType pAfterColon DEFAULT
      (_, updatedParser) = expected pAfterStructType SEMI_COLON
   in (tokenValue nameToken, StructDeclProperty {propertyType = propertyType, isStatic = isStatic}, updatedParser) -- TODO: impl

parseStructMethod :: Parser -> Bool -> (String, StructDeclMethod, Parser)
parseStructMethod = undefined -- TODO: impl struct methods

parseStructPropOrMethod :: Bool -> (Map String StructDeclProperty, Map String StructDeclMethod, Parser) -> (Map String StructDeclProperty, Map String StructDeclMethod, Parser)
parseStructPropOrMethod isStatic (props, methods, parser)
  | kind == IDENTIFIER =
      let (propName, newProp, updatedParser) = parseStructProp parser isStatic
          errMsg = error $ "Duplicated property name " ++ propName
          newProps = insertIfNotMember errMsg propName newProp props
       in (newProps, methods, updatedParser)
  | kind == FN =
      let (methodName, newMethod, updatedParser) = parseStructMethod parser isStatic
       in (props, Map.insert methodName newMethod methods, updatedParser)
  | otherwise = error $ "Expected either property or method declaration but found " ++ show kind
  where
    kind = currentTokenKind parser

parseStructPropsAndMethods :: (Map String StructDeclProperty, Map String StructDeclMethod, Parser) -> (Map String StructDeclProperty, Map String StructDeclMethod, Parser)
parseStructPropsAndMethods (props, methods, parser)
  | hasTokens parser && kind /= RBRACE =
      let isStatic = kind == STATIC
          pAfterStatic = if isStatic then snd (expected parser STATIC) else parser
          updatedStructAndParser = parseStructPropOrMethod isStatic (props, methods, pAfterStatic)
       in parseStructPropsAndMethods updatedStructAndParser
  | otherwise = (props, methods, parser)
  where
    kind = currentTokenKind parser

parseStructDeclStmt :: Parser -> (Stmt, Parser)
parseStructDeclStmt parser =
  let (_, pAfterStruct) = expected parser STRUCT
      (structNameToken, pAfterStructName) = expected pAfterStruct IDENTIFIER
      (_, pAfterLBrace) = expected pAfterStructName LBRACE
      (props, methods, pAfterPropsMethods) = parseStructPropsAndMethods (Map.empty, Map.empty, pAfterLBrace)
      (_, pAfterRBrace) = expected pAfterPropsMethods RBRACE
   in ( StructDeclStmt
          { name = tokenValue structNameToken,
            properties = props,
            methods = methods
          },
        pAfterRBrace
      )

--------------------------------------------------------------------------------
-- HELPERS ADD HANDLERS AND BPs TO LOOKUPS
--------------------------------------------------------------------------------

getBp :: TokenKind -> BindingPower
getBp kind = fromMaybe DEFAULT (Map.lookup kind (bindingPowerLookup lookups)) -- error $ "Expected binding power for token " ++ show kind

--------------------------------------------------------------------------------
-- CREATE LOOKUPS
--------------------------------------------------------------------------------

-- NUD:
createNudLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudLookups = createNudGroupingLookups . createNudPrefixLookups . createNudPrimaryLookups

-- Literals & symbols
createNudPrimaryLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudPrimaryLookups l = createLookups parsePrimaryExpr PRIMARY l [NUMBER, STRING, IDENTIFIER]

createNudPrefixLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudPrefixLookups l = createLookups parsePrefixExpr UNARY l [MINUS]

-- Grouping
createNudGroupingLookups :: (BindingPowerLookup, NudLookup) -> (BindingPowerLookup, NudLookup)
createNudGroupingLookups l = createLookups parseGroupingExpr DEFAULT l [LPAREN]

-- LED:
createLedLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLookups = createLedArrayInstantiationLookups . createLedCallLookups . createLedMultiplicativeLookups . createLedAdditiveLookups . createLedRelationalLookups . createLedLogicalLookups . createLedAssignmentLookups

-- Assignment
createLedAssignmentLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedAssignmentLookups l = createLookups parseAssignmentExpr ASSIG l [ASSIGNMENT, PLUS_EQUALS, MINUS_EQUALS, TIMES_EQUALS, DIVIDE_EQUALS, MODULO_EQUALS, POWER_EQUALS]

-- Logical
createLedLogicalLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedLogicalLookups l = createLookups parseBinaryExpr LOGICAL l [AND, OR, ELLIPSIS]

-- Relational
createLedRelationalLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedRelationalLookups l = createLookups parseBinaryExpr RELATIONAL l [EQUAL, NOT_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL]

-- Additive
createLedAdditiveLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedAdditiveLookups l = createLookups parseBinaryExpr ADDITIVE l [PLUS, MINUS]

-- Multiplicative
createLedMultiplicativeLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedMultiplicativeLookups l = createLookups parseBinaryExpr MULTIPLICATIVE l [TIMES, DIVIDE, MODULO]

-- Call/Member
createLedCallLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedCallLookups l = createLookups parseStructInstantiation CALL l [LBRACE]

-- Array instantiation
createLedArrayInstantiationLookups :: (BindingPowerLookup, LedLookup) -> (BindingPowerLookup, LedLookup)
createLedArrayInstantiationLookups l = createLookups parseArrayLiteral CALL l [LBRACKET]

-- STMT:
createStmtLookups :: (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
createStmtLookups = createStmtStructDeclLookups . createStmtVarDeclLookups

-- Var Decl
createStmtVarDeclLookups :: (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
createStmtVarDeclLookups l = createLookups parseVarDeclStmt DEFAULT l [CONST, LET]

createStmtStructDeclLookups :: (BindingPowerLookup, StmtLookup) -> (BindingPowerLookup, StmtLookup)
createStmtStructDeclLookups l = createLookups parseStructDeclStmt DEFAULT l [STRUCT]

-- Finally:
-- TODO: implement this, we need to create each lookup and have the bindingPower shared/updated for all lookups
lookups :: Lookups
lookups = Lookups {bindingPowerLookup = bp, nudLookup = nl, ledLookup = ll, stmtLookup = sl}
  where
    (bp'', nl) = createNudLookups (Map.empty, Map.empty)
    (bp', ll) = createLedLookups (bp'', Map.empty)
    (bp, sl) = createStmtLookups (bp', Map.empty)
