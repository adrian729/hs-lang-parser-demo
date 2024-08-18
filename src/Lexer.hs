module Lexer where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.Regex.TDFA
import Tokens

isMatch :: String -> String -> Bool
isMatch str regex = str =~ regex :: Bool

matchExpr :: String -> String -> String
matchExpr str regex = str =~ regex :: String

data RegexPattern = RegexPattern
  { regex :: String, -- regex pattern
    handler :: Lexer -> String -> Lexer -- handles the regexp and adds the token to the lexer
  }

data Lexer = Lexer
  { pos :: Int,
    source :: String,
    tokens :: [Token],
    patterns :: [RegexPattern]
  }

-- Helper functions
at :: Lexer -> Char
at lexer = source lexer !! pos lexer

remainder :: Lexer -> String
remainder lexer = drop (pos lexer) (source lexer)

atEOF :: Lexer -> Bool
atEOF lexer = pos lexer >= length (source lexer)

reservedKWLookup :: Map String TokenKind
reservedKWLookup =
  Map.fromList
    [ ("let", LET),
      ("const", CONST),
      ("class", CLASS),
      ("new", NEW),
      ("import", IMPORT),
      ("from", FROM),
      ("function", FN),
      ("if", IF),
      ("else", ELSE),
      ("foreach", FOREACH),
      ("while", WHILE),
      ("for", FOR),
      ("export", EXPORT),
      ("typeof", TYPEOF),
      ("in", IN)
    ]

isReservedSymbol :: String -> Bool
isReservedSymbol str = Map.member str reservedKWLookup

-- Handlers
skipHandler :: Lexer -> String -> Lexer
skipHandler lexer patternRegex =
  let matchValue = matchExpr (remainder lexer) patternRegex
   in lexer
        { pos = pos lexer + length matchValue
        }

symbolHandler :: Lexer -> String -> Lexer
symbolHandler lexer patternRegex =
  let matchValue = matchExpr (remainder lexer) patternRegex
      isReservedKW = isReservedSymbol matchValue
      kind = if isReservedKW then reservedKWLookup Map.! matchValue else IDENTIFIER
   in lexer
        { pos = pos lexer + length matchValue,
          tokens = (Token {tokenValue = matchValue, tokenKind = kind}) : tokens lexer
        }

defaultHandler :: TokenKind -> Lexer -> String -> Lexer
defaultHandler tokenKind lexer patternRegex =
  let matchValue = matchExpr (remainder lexer) patternRegex
   in lexer
        { pos = pos lexer + length matchValue,
          tokens = (Token {tokenValue = matchValue, tokenKind = tokenKind}) : tokens lexer
        }

-- TODO: check all patterns are defined
-- Sorting of the patterns MATTERS
definedPatterns :: [RegexPattern]
definedPatterns =
  [ -- SYMBOL
    RegexPattern
      { regex = "\\`[a-zA-Z_][a-zA-Z0-9_]*",
        handler = symbolHandler
      },
    -- SKIPS
    RegexPattern
      { regex = "\\`[[:space:]|[:blank:]]+",
        handler = skipHandler
      },
    -- COMMENTS
    RegexPattern
      { regex = "\\`\\/\\/.*",
        handler = skipHandler
      },
    -- NUMBER
    RegexPattern
      { regex = "\\`[[:digit:]]+(\\.[[:digit:]]+)?",
        handler = defaultHandler NUMBER
      },
    -- STRING
    RegexPattern
      { regex = "\\`\"[^\"]*\"",
        handler = defaultHandler STRING
      },
    -- GROUPING
    RegexPattern
      { regex = "\\`\\(",
        handler = defaultHandler LPAREN
      },
    RegexPattern
      { regex = "\\`\\)",
        handler = defaultHandler RPAREN
      },
    RegexPattern
      { regex = "\\`\\{",
        handler = defaultHandler LBRACE
      },
    RegexPattern
      { regex = "\\`\\}",
        handler = defaultHandler RBRACE
      },
    RegexPattern
      { regex = "\\`\\[",
        handler = defaultHandler LBRACKET
      },
    RegexPattern
      { regex = "\\`\\]",
        handler = defaultHandler RBRACKET
      },
    -- COMPARE
    RegexPattern
      { regex = "\\`\\=\\=",
        handler = defaultHandler EQUAL
      },
    RegexPattern
      { regex = "\\`\\!\\=",
        handler = defaultHandler NOT_EQUAL
      },
    RegexPattern
      { regex = "\\`<\\=",
        handler = defaultHandler LESS_EQUAL
      },
    RegexPattern
      { regex = "\\`<",
        handler = defaultHandler LESS
      },
    RegexPattern
      { regex = "\\`>\\=",
        handler = defaultHandler GREATER_EQUAL
      },
    RegexPattern
      { regex = "\\`>",
        handler = defaultHandler GREATER
      },
    -- ASSIGNMENT w ARITHMETIC
    RegexPattern
      { regex = "\\`\\+\\+",
        handler = defaultHandler PLUS_PLUS
      },
    RegexPattern
      { regex = "\\`\\-\\-",
        handler = defaultHandler MINUS_MINUS
      },
    RegexPattern
      { regex = "\\`\\+\\=",
        handler = defaultHandler PLUS_EQUALS
      },
    RegexPattern
      { regex = "\\`\\-\\=",
        handler = defaultHandler MINUS_EQUALS
      },
    RegexPattern
      { regex = "\\`\\*\\=",
        handler = defaultHandler TIMES_EQUALS
      },
    RegexPattern
      { regex = "\\`\\/\\=",
        handler = defaultHandler DIVIDE_EQUALS
      },
    RegexPattern
      { regex = "\\`\\%\\=",
        handler = defaultHandler MODULO_EQUALS
      },
    RegexPattern
      { regex = "\\`\\^\\=",
        handler = defaultHandler POWER_EQUALS
      },
    -- ASSIGNMENT
    RegexPattern
      { regex = "\\`\\=",
        handler = defaultHandler ASSIGNMENT
      },
    -- LOGICAL
    RegexPattern
      { regex = "\\`\\&\\&",
        handler = defaultHandler AND
      },
    RegexPattern
      { regex = "\\`\\|\\|",
        handler = defaultHandler OR
      },
    RegexPattern
      { regex = "\\`\\!",
        handler = defaultHandler NOT
      },
    -- ELLIPSIS
    RegexPattern
      { regex = "\\`\\.\\.",
        handler = defaultHandler ELLIPSIS
      },
    -- PUNCTUATION
    RegexPattern
      { regex = "\\`\\,",
        handler = defaultHandler COMMA
      },
    RegexPattern
      { regex = "\\`\\;",
        handler = defaultHandler SEMI_COLON
      },
    RegexPattern
      { regex = "\\`\\:",
        handler = defaultHandler COLON
      },
    RegexPattern
      { regex = "\\`\\.",
        handler = defaultHandler DOT
      },
    RegexPattern
      { regex = "\\`\\?",
        handler = defaultHandler QUESTION
      },
    -- ARITHMETIC
    RegexPattern
      { regex = "\\`\\+",
        handler = defaultHandler PLUS
      },
    RegexPattern
      { regex = "\\`\\-",
        handler = defaultHandler MINUS
      },
    RegexPattern
      { regex = "\\`\\*",
        handler = defaultHandler TIMES
      },
    RegexPattern
      { regex = "\\`\\/",
        handler = defaultHandler DIVIDE
      },
    RegexPattern
      { regex = "\\`\\%",
        handler = defaultHandler MODULO
      },
    RegexPattern
      { regex = "\\`\\^",
        handler = defaultHandler POWER
      }
  ]

iteratePatterns :: Lexer -> [RegexPattern] -> Lexer
iteratePatterns _ [] = error "No matching pattern found"
iteratePatterns lexer (pattern : patterns)
  | isMatch (remainder lexer) (regex pattern) = handler pattern lexer $ regex pattern -- if match, handle and break
  | otherwise = iteratePatterns lexer patterns

iterateLexer :: Lexer -> Lexer
iterateLexer lexer
  | atEOF lexer = lexer {tokens = Token {tokenValue = "EOF", tokenKind = EOF} : tokens lexer}
  | otherwise = iterateLexer $ iteratePatterns lexer (patterns lexer)

createLexer :: String -> Lexer
createLexer source =
  Lexer
    { pos = 0,
      source = source,
      tokens = [],
      patterns = definedPatterns
    }

-- TODO: use better DS than List and append to the end instead of reverse it later. I think appending to the end to a list costs a lot.
tokenize :: String -> [Token]
tokenize source = reverse $ tokens $ iterateLexer $ createLexer source

printTokens :: [Token] -> IO ()
printTokens [] = do
  putStrLn "reached EOF"
printTokens (t : ts) = do
  print t
  printTokens ts
