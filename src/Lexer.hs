-- Define a module named Lexer
module Lexer where

-- Import various components from the Parsec library, which is used for parsing text
import Text.Parsec ( alphaNum, letter, oneOf, eof, (<|>) )
import Text.Parsec.Text.Lazy ( Parser )
import qualified Data.Text.Lazy as L
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Expr as Ex

import Data.Functor.Identity ( Identity )

-- These type aliases are specific to setting up operator parsers:
-- Op a: Defines a type for an individual operator, where a is the type of the expression the operator works on. Ex.Operator is a type from Text.Parsec.Expr used to define individual operators like binary or unary operators.
-- Operators a: An alias for a table of operators (a list of lists of Op a), used to define operator precedence and associativity for parsing expressions. It is essential for correctly parsing expressions like arithmetic operations that need to respect order and grouping.
type Op a = Ex.Operator L.Text () Identity a -- () is the type of the state, which is not used in this case, like void in C
type Operators a = Ex.OperatorTable L.Text () Identity a

-- List of reserved keywords in the language
reservedNames :: [String]
reservedNames = [
    "let",
    "in",
    "fix",
    "rec",
    "if",
    "then",
    "else"
  ]

-- List of operators recognized by the lexer
reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "+",
    "*",
    "-",
    "=",
    -- TODO-1: Add handling for cons operator ✓
    ":",
    -- TODO-2: Add handling for concat operator ✓
    "++"
  ]

-- Lexer configuration using Parsec's token parser generator
lexer :: Tok.GenTokenParser L.Text () Identity
lexer = Tok.makeTokenParser $ Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-," -- TODO-1: Add comma ✓
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~," -- TODO-1: Add comma ✓
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }

-- Helper functions wrapping Parsec's token parsing functions, providing an API for the lexer
reserved :: String -> Parser ()        -- Parse a reserved name
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()      -- Parse a reserved operator
reservedOp = Tok.reservedOp lexer

identifier :: Parser String            -- Parse an identifier
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a         -- Parse expressions within parentheses
parens = Tok.parens lexer

brackets   = Tok.brackets lexer
symbol     = Tok.symbol lexer

semiSep :: Parser a -> Parser [a]      -- Parse semicolon-separated expressions
semiSep = Tok.semiSep lexer

semi :: Parser String                  -- Parse a semicolon
semi = Tok.semi lexer

-- Parse a complete content within optional whitespace and ensuring end of file
-- basically, it's a wrapper that skip whitespaces and parse the content
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer   -- Skip leading whitespace
  r <- p                 -- Parse the actual content
  eof                    -- Expect end of file
  return r

