module Lexer where

import Tokens
import Text.Regex.TDFA

isMatch :: String -> String -> Bool
isMatch str regex = str =~ regex :: Bool

matchExpr :: String -> String -> String
matchExpr str regex = str =~ regex :: String

data RegexPattern = RegexPattern {
    regex :: String, -- regex pattern
    handler :: Lexer -> String -> Lexer -- handles the regexp and adds the token to the lexer
}

data Lexer = Lexer {
    pos :: Int,
    source :: String,
    tokens :: [Token],
    patterns :: [RegexPattern]
}

at :: Lexer -> Char
at lexer = source lexer !! pos lexer

remainder :: Lexer -> String
remainder lexer = drop (pos lexer) (source lexer)

atEOF :: Lexer -> Bool
atEOF lexer = pos lexer >= length (source lexer)

skipHandler :: Lexer -> String -> Lexer
skipHandler lexer patternRegex  = let matchValue = matchExpr (remainder lexer) patternRegex in
    lexer {
        pos = pos lexer + length matchValue
    }

numberHandler :: Lexer -> String -> Lexer
numberHandler lexer patternRegex  = let matchValue = matchExpr (remainder lexer) patternRegex in
    lexer {
        pos = pos lexer + length matchValue,
        tokens = (Token { tokenValue = matchValue, tokenKind = NUMBER }) : tokens lexer
    }

defaultHandler :: TokenKind -> Lexer -> String -> Lexer
defaultHandler tokenKind lexer patternRegex = let matchValue = matchExpr (remainder lexer) patternRegex in
    lexer {
        pos = pos lexer + length matchValue,
        tokens = (Token { tokenValue = matchValue, tokenKind = tokenKind }) : tokens lexer
    }

-- TODO: not sure the reg exp is correct because TDFA kinda sucks... but seems to be the best option with HS
-- Sorting of the patterns MATTERS
definedPatterns :: [RegexPattern]
definedPatterns = [
    -- SKIPS
        RegexPattern {
            regex = "\\`[[:space:]|[:blank:]]+",
            handler = skipHandler
        },
    -- NUMBER
        RegexPattern {
            regex = "\\`[0-9]+(\\.[0-9]+)?",
            handler = numberHandler
        },
    -- GROUPING
        RegexPattern {
            regex = "\\`\\(",
            handler = defaultHandler LPAREN
        },
        RegexPattern {
            regex = "\\`\\)",
            handler = defaultHandler RPAREN
        },
        RegexPattern {
            regex = "\\`\\{",
            handler = defaultHandler LBRACE
        },
        RegexPattern {
            regex = "\\`\\}",
            handler = defaultHandler RBRACE
        },
        RegexPattern {
            regex = "\\`\\[",
            handler = defaultHandler LBRACKET
        },
        RegexPattern {
            regex = "\\`\\]",
            handler = defaultHandler RBRACKET
        },
        -- COMPARE
        RegexPattern {
            regex = "\\`\\=\\=",
            handler = defaultHandler EQUAL
        },
        RegexPattern {
            regex = "\\`\\!\\=",
            handler = defaultHandler NOT_EQUAL
        },
        RegexPattern {
            regex = "\\`<\\=",
            handler = defaultHandler LESS_EQUAL
        },
        RegexPattern {
            regex = "\\`<",
            handler = defaultHandler LESS
        },
        RegexPattern {
            regex = "\\`>\\=",
            handler = defaultHandler GREATER_EQUAL
        },
        RegexPattern {
            regex = "\\`>",
            handler = defaultHandler GREATER
        },
        -- ASSIGNMENT w ARITHMETIC
        RegexPattern {
            regex = "\\`\\+\\+",
            handler = defaultHandler PLUS_PLUS
        },
        RegexPattern {
            regex = "\\`\\-\\-",
            handler = defaultHandler MINUS_MINUS
        },
        RegexPattern {
            regex = "\\`\\+\\=",
            handler = defaultHandler PLUS_EQUALS
        },
        RegexPattern {
            regex = "\\`\\-\\=",
            handler = defaultHandler MINUS_EQUALS
        },
        RegexPattern {
            regex = "\\`\\*\\=",
            handler = defaultHandler TIMES_EQUALS
        },
        RegexPattern {
            regex = "\\`\\/\\=",
            handler = defaultHandler DIVIDE_EQUALS
        },
        RegexPattern {
            regex = "\\`\\%\\=",
            handler = defaultHandler MODULO_EQUALS
        },
        RegexPattern {
            regex = "\\`\\^\\=",
            handler = defaultHandler POWER_EQUALS
        },
        -- ASSIGNMENT
        RegexPattern {
            regex = "\\`\\=",
            handler = defaultHandler ASSIGNMENT
        },
        -- LOGICAL
        RegexPattern {
            regex = "\\`\\&\\&",
            handler = defaultHandler AND
        },
        RegexPattern {
            regex = "\\`\\|\\|",
            handler = defaultHandler OR
        },
        RegexPattern {
            regex = "\\`\\!",
            handler = defaultHandler NOT
        },
        -- ELLIPSIS
        RegexPattern {
            regex = "\\`\\.\\.",
            handler = defaultHandler ELLIPSIS
        },
        -- PUNCTUATION
        RegexPattern {
            regex = "\\`\\,",
            handler = defaultHandler COMMA
        },
        RegexPattern {
            regex = "\\`\\;",
            handler = defaultHandler SEMI_COLON
        },
        RegexPattern {
            regex = "\\`\\:",
            handler = defaultHandler COLON
        },
        RegexPattern {
            regex = "\\`\\.",
            handler = defaultHandler DOT
        },
        RegexPattern {
            regex = "\\`\\?",
            handler = defaultHandler QUESTION
        },
        -- ARITHMETIC
        RegexPattern {
            regex = "\\`\\+",
            handler = defaultHandler PLUS
        },
        RegexPattern {
            regex = "\\`\\-",
            handler = defaultHandler MINUS
        },
        RegexPattern {
            regex = "\\`\\*",
            handler = defaultHandler TIMES
        },
        RegexPattern {
            regex = "\\`\\/",
            handler = defaultHandler DIVIDE
        },
        RegexPattern {
            regex = "\\`\\%",
            handler = defaultHandler MODULO
        },
        RegexPattern {
            regex = "\\`\\^",
            handler = defaultHandler POWER
        },
        -- RESERVED KEYWORDS
        RegexPattern {
            regex = "\\`let",
            handler = defaultHandler LET
        },
        RegexPattern {
            regex = "\\`const",
            handler = defaultHandler CONST
        },
        RegexPattern {
            regex = "\\`class",
            handler = defaultHandler CLASS
        },
        RegexPattern {
            regex = "\\`new",
            handler = defaultHandler NEW
        },
        RegexPattern {
            regex = "\\`import",
            handler = defaultHandler IMPORT
        },
        RegexPattern {
            regex = "\\`from",
            handler = defaultHandler FROM
        },
        RegexPattern {
            regex = "\\`function",
            handler = defaultHandler FN
        },
        RegexPattern {
            regex = "\\`if",
            handler = defaultHandler IF
        },
        RegexPattern {
            regex = "\\`else",
            handler = defaultHandler ELSE
        },
        RegexPattern {
            regex = "\\`foreach",
            handler = defaultHandler FOREACH
        },
        RegexPattern {
            regex = "\\`while",
            handler = defaultHandler WHILE
        },
        RegexPattern {
            regex = "\\`for",
            handler = defaultHandler FOR
        },
        RegexPattern {
            regex = "\\`export",
            handler = defaultHandler EXPORT
        },
        RegexPattern {
            regex = "\\`typeof",
            handler = defaultHandler TYPEOF
        },
        RegexPattern {
            regex = "\\`in",
            handler = defaultHandler IN
        },
        -- STRING
        RegexPattern {
            regex = "\\`\"[^\"]*\"",
            handler = defaultHandler STRING
        },
        -- ID
        RegexPattern {
            regex = "\\`[a-zA-Z_][a-zA-Z0-9_]*",
            handler = defaultHandler ID
        }
    ]

createLexer :: String -> Lexer
createLexer source = Lexer {
    pos = 0,
    source = source,
    tokens = [],
    patterns = definedPatterns -- TODO: define all of our patterns
}

iteratePatterns :: Lexer -> [RegexPattern] -> Lexer
iteratePatterns _ [] = error "No matching pattern found"
iteratePatterns lexer (pattern:patterns)
    | isMatch (remainder lexer) (regex pattern) = handler pattern lexer $ regex pattern -- if match, handle and break
    | otherwise = iteratePatterns lexer patterns

iterateLexer :: Lexer -> Lexer
iterateLexer lexer
    | atEOF lexer = lexer { tokens = Token { tokenValue = "EOF", tokenKind = EOF } : tokens lexer }
    | otherwise = iterateLexer $ iteratePatterns lexer (patterns lexer)

tokenize :: String -> [Token]
tokenize source = tokens $ iterateLexer $ createLexer source

printTokens :: [Token] -> IO ()
printTokens [] = do
    putStrLn "reached EOF"
printTokens (t:ts) = do
    print t
    printTokens ts