{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseExpr,
  parseModule
) where

-- Import necessary functions and types from the Parsec library
import Text.Parsec
    ( optional, (<|>), many, many1, parse, try, sepBy, ParseError )
import Text.Parsec.Text.Lazy (Parser) -- Import the Parser type configured for lazy Text

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax

-- TODO-0: Bug! Check the github issues/pull-requests to try to fix this: 
--       https://github.com/sdiehl/write-you-a-haskell  ✓
integer :: Parser Integer
integer = Tok.natural lexer

variable :: Parser Expr
variable = do
  Var <$> identifier

number :: (Lit -> a) -> Parser a
number c = do
  c . LInt . fromIntegral <$> integer

bool :: (Lit -> a) -> Parser a
bool c = (reserved "True" >> return (c (LBool True)))
    <|> (reserved "False" >> return (c (LBool False)))

-- https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec.html#g:2
list :: (Lit -> a) -> Parser a
list c = do
    _ <- symbol "["
    elements <- sepBy expr (Tok.symbol lexer ",")
    _ <- symbol "]"
    return $ c (LArray elements)
  -- TODO-1: Handle parsing a list ✓
  -- Suggestion: use the sepBy command (see link above)
  -- error ""

fix :: Parser Expr
fix = do
  reservedOp "fix"
  Fix <$> expr -- put expr in parens

-- TODO-2: use patterns instead of identifiers for args
lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many parsePattern  -- Parse patterns instead of identifiers
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args  -- Construct lambda using patterns

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let x e1 <$> expr

letrecin :: Parser Expr
letrecin = do
  reserved "let"
  reserved "rec"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  Let x e1 <$> expr

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  If cond tr <$> expr

aexp :: Parser Expr
aexp =
      parens expr
      -- TODO-1: Add parsing for list ✓
  <|> list Lit
  <|> bool Lit
  <|> number Lit
  <|> ifthen
  <|> fix
  <|> try letrecin
  <|> letin
  <|> lambda
  <|> variable

term :: Parser Expr
term = aexp >>= \x ->
                (many1 aexp >>= \xs -> return (foldl App x xs))
                <|> return x

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

table :: Operators Expr
-- TODO-1: Add cons operator. Make sure you have proper associativity! ✓
-- TODO-CONCAT: Add concat operator. Make sure you have proper associativity! ✓
table = [
    [
      infixOp "*" (Op Mul) Ex.AssocLeft
    ],
    [
      infixOp "+" (Op Add) Ex.AssocLeft
    , infixOp "-" (Op Sub) Ex.AssocLeft
    ],
    [
      infixOp ":" (Op Cons) Ex.AssocRight
    , infixOp "++" (Op Concat) Ex.AssocRight
    ],
    [
      infixOp "==" (Op Eql) Ex.AssocLeft
    ]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

type Binding = (String, Expr)

-- TODO-2: use patterns instead of identifiers for args
letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  patterns <- many parsePattern  -- Parse patterns instead of identifiers
  reservedOp "="
  body <- expr
  return (name, foldr Lam body patterns)  -- Construct lambda using patterns

-- TODO-2: use patterns instead of identifiers for args
letrecdecl :: Parser (String, Expr)
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- identifier
  patterns <- many parsePattern  -- Parse patterns instead of identifiers
  reservedOp "="
  body <- expr
  -- Wrap the body in a `Fix` and construct the function with patterns
  return (name, Fix $ foldr Lam body (PVar name : patterns))

parsePattern :: Parser Pattern
parsePattern = parsePList <|> parsePLit <|> parsePVar <|> parsePCons

parsePList :: Parser Pattern
parsePList = do
  _ <- Tok.symbol lexer "["
  elements <- sepBy expr (Tok.symbol lexer ",")
  _ <- Tok.symbol lexer "]"
  return $ PLit (LArray elements)

parsePVar :: Parser Pattern
parsePVar = PVar <$> identifier

parsePLit :: Parser Pattern
parsePLit = PLit <$> parseLiteral

parseLiteral :: Parser Lit
parseLiteral = parseLitBool <|> parseLitInt

parseLitInt :: Parser Lit
parseLitInt = LInt <$> integer

parseLitBool :: Parser Lit
parseLitBool = LBool True <$ reserved "True"
           <|> LBool False <$ reserved "False"

parsePCons :: Parser Pattern
parsePCons = do
  _ <- Tok.symbol lexer "("
  head <- identifier
  reservedOp ":"
  tail <- identifier
  _ <- Tok.symbol lexer ")"
  return $ PCons head tail
  
val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule = parse (contents modl)
