module Main where

import Lexer
import Parse
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Show.Pretty (pPrint)

examplePath :: String
examplePath = "resources/05.lang"

printHeader :: String -> IO ()
printHeader header = do
  putStrLn ""
  putStrLn "----------------"
  putStrLn header
  putStrLn "----------------"
  putStrLn ""

main :: IO ()
main = do
  handle <- openFile examplePath ReadMode
  contents <- hGetContents handle
  printHeader "Code"
  putStrLn contents
  let tokens = tokenize contents
  printHeader "TOKENS"
  printTokens tokens
  printHeader "AST"
  pPrint $ createParser tokens
  pPrint $ parse tokens
  putStrLn ""

-- Pratt Parser
{-
    Pratt parsing is a parsing approach that builds the Abstract Syntax Tree (AST) by leveraging
    operator precedence and binding powers.

    Compared to conventional recursive descent parsers, Pratt parsers offer a more declarative and
    intuitive way to define and parse expressions.
-}

-- Binding Power
{-
    Binding power is how tightly a token binds to its neighboring tokens. Another way to think of
    this is attraction. A + token will have a lower binding power than a * token.

    (highest precedence == lowest binding power)
-}

-- Null Denotation (NUD)
{-
    type nud_handler func(p *parser) ast.Expr // NUD handler in Go

    A token which has a LUD handler, means it expects nothing to its left.

    Common examples of this type of token are prefix & unary expressions.
-}

-- Left Denotation (LED)
{-
    type led_handler func(p *parser, left ast.Expr, bp binding_power) ast.Expr // LED handler in Go

    Tokens which have a LED handler expect to be between or after ome other expression to its left.

    Examples of this type of handler include binary expressions and all infix expressions.

    Postfix expressions also fall under the LED handler.
-}

-- Lookup Tables
{-
    By using lookup tables along with NUD/LED handler functions, we can create the parser almost
    entirely without having to manage the recursion ourselves.

    Here is an example of lookup table (in Go):

    type stmt_handler func(p *parser) ast.Stmt
    type nud_handler func(p *parser) ast.Expr
    type led_handler func(p *parser, left ast.Expr, bp binding_power) ast.Expr

    type stmt_lookup map[lexer.TokenKind] stmt_handler
    type nud_lookup map[lexer.TokenKind] nud_handler
    type led_lookup map[lexer.TokenKind] led_handler
    type bp_lookup map[lexer.TokenKind] binding_power

    var bp_lu = bp_lookup{}
    var nud_lu = nud_lookup{}
    var led_lu = led_lookup{}
    var stmt_lu = stmt_lookup{}
-}
