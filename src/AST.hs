module AST where

import Tokens

data Expr
  = NumberExpr Double -- TODO: decide if using a type NumberExpr = Double instead or like this with names
  | StringExpr String
  | SymbolExpr String
  | BinaryExpr
      { left :: Expr,
        operator :: Token,
        right :: Expr
      }
  | AssignmentExpr
      { assigne :: Expr,
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
  deriving (Show)

isBlockStmt :: Stmt -> Bool
isBlockStmt (BlockStmt _) = True
isBlockStmt _ = False
