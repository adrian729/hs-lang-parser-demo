module AST where

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
  deriving (Show)

-- TYPES

data Type
  = SymbolType String -- T
  | ArrayType Type -- T[]
  deriving (Show)
