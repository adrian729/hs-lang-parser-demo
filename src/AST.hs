module AST where

import Data.Map
import Tokens

data Expr
  = NumberExpr Double
  | StringExpr String
  | SymbolExpr String
  | BinaryExpr
      { left :: Expr,
        operator :: Token,
        right :: Expr
      }
  | AssignmentExpr -- a = 1 + 2; a += 4; foo.bar -= 33;
      { assigne :: Expr,
        operator :: Token,
        value :: Expr
      }
  | PrefixExpr
      { operator :: Token,
        right :: Expr
      }
  | StructInstantiationExpr
      { structName :: String,
        structInstanceProps :: Map String Expr
      }
  | ArrayLiteralExpr
      { arrayType :: Type,
        content :: [Expr]
      }
  deriving (Show)

-- STATEMENTS
data Stmt
  = BlockStmt
      { body :: [Stmt]
      }
  | ExprStmt
      { expr :: Expr
      }
  | VarDeclStmt
      { name :: String,
        isConst :: Bool,
        assignedVal :: Maybe Expr,
        explicitType :: Maybe Type
      }
  | StructDeclStmt
      { name :: String,
        properties :: Map String StructDeclProperty,
        methods :: Map String StructDeclMethod
      }
  deriving (Show)

data StructDeclProperty = StructDeclProperty
  { propertyType :: Type,
    isStatic :: Bool -- determines if the property is static (shared over all instances of the struct)
  }
  deriving (Show)

data StructDeclMethod = StructDeclMethod
  { isStaticMethod :: Bool, -- determines if the method is static
    methodType :: Type -- TODO: implement FN types and change this
  }
  deriving (Show)

-- TYPES

data Type
  = SymbolType String -- T
  | ArrayType Type -- T[]
  deriving (Show)
