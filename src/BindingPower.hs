module BindingPower where

data BindingPower
  = NONE
  | DEFAULT
  | COMMA
  | ASSIG
  | LOGICAL
  | RELATIONAL
  | ADDITIVE
  | MULTIPLICATIVE
  | UNARY
  | CALL
  | MEMBER
  | PRIMARY
  deriving (Enum, Show, Eq, Ord)
