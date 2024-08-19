module Tokens where

data TokenKind
  = -- BASIC
    EOF
  | NUMBER
  | STRING
  | IDENTIFIER
  | -- GROUPING
    LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | -- COMPARE
    EQUAL
  | NOT_EQUAL
  | LESS
  | LESS_EQUAL
  | GREATER
  | GREATER_EQUAL
  | -- LOGICAL
    AND
  | OR
  | NOT
  | -- ELLIPSIS
    ELLIPSIS
  | -- PUNCTUATION
    COMMA
  | SEMI_COLON
  | COLON
  | DOT
  | QUESTION
  | -- ARITHMETIC
    PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MODULO
  | POWER
  | -- ASSIGNMENT
    ASSIGNMENT
  | -- ASSIGNMENT w ARITHMETIC
    PLUS_PLUS
  | MINUS_MINUS
  | PLUS_EQUALS
  | MINUS_EQUALS
  | TIMES_EQUALS
  | DIVIDE_EQUALS
  | MODULO_EQUALS
  | POWER_EQUALS
  | -- RESERVED KEYWORDS
    LET
  | CONST
  | CLASS
  | NEW
  | IMPORT
  | FROM
  | FN
  | IF
  | ELSE
  | FOREACH
  | WHILE
  | FOR
  | EXPORT
  | TYPEOF
  | IN
  deriving (Enum, Show, Eq, Ord)

data Token = Token
  { tokenValue :: String,
    tokenKind :: TokenKind
  }
  deriving (Show)

isOneOfMany :: Token -> [TokenKind] -> Bool
isOneOfMany (Token _ kind) kinds = kind `elem` kinds

reservedKeywords :: [TokenKind]
reservedKeywords = [LET, CONST, CLASS, NEW, IMPORT, FROM, FN, IF, ELSE, FOREACH, WHILE, FOR, EXPORT, TYPEOF, IN]

isReservedKeyword :: Token -> Bool
isReservedKeyword (Token _ kind) = kind `elem` reservedKeywords

debug :: Token -> String
debug token
  | isOneOfMany token [EOF, NUMBER, STRING, IDENTIFIER] = show (tokenKind token) ++ " " ++ tokenValue token
  | otherwise = show (tokenKind token)
