module Syntax where

-- Define a type alias 'Var' for String to represent variable names
type Var = String

-- TODO-2: Change lambdas to use Pattern struct ✓
data Expr -- Define a data type 'Expr' for representing expressions in a language
  = Var Var                 -- A variable
  | App Expr Expr           -- An application of one expression to another
  | Lam Pattern Expr        -- A lambda (anonymous function) expression with a parameter and a body
  | Let Var Expr Expr       -- A let-binding for defining local variables
  | Lit Lit                 -- A literal value, Lit constructor takes a Lit value(defined below)
  | If Expr Expr Expr       -- An if expression with a condition, then branch, and else branch
  | Fix Expr                -- A fix expression for defining recursive functions
  | Op Binop Expr Expr      -- An operation (e.g., addition) applied to two expressions
  deriving (Show, Eq, Ord)  -- Derive Show, Eq, and Ord instances for convenience

-- TODO-1: Add an array literal (LArray) ✓
data Lit -- Define a data type 'Lit' for literal values
  = LInt Integer            -- An integer literal
  | LBool Bool              -- A boolean literal
  | LArray [Expr]           -- An array literal                   
  deriving (Show, Eq, Ord)  -- Derive Show, Eq, and Ord instances for convenience

-- TODO-1: Add a Cons operator ✓
-- TODO-CAT: Add a Concat operator ✓
data Binop = Add | Sub | Mul | Eql | Cons | Concat -- Define a data type 'Binop' for binary operations
  deriving (Eq, Ord, Show)

-- TODO-2: Add a Pattern struct, capture the following cases: ✓
-- let f x = ...      -- any identifier
-- let f 0 = ...      -- any int literal
-- let f True = ...   -- any bool literal
-- let f [] = ...     -- empty list
-- let f (x:xs) = ... -- non empty list
data Pattern = PVar Var | PCons Var Var | PLit Lit 
  deriving (Show, Eq, Ord)

type Decl = (String, Expr)

-- Define a data type 'Program' for representing entire programs
-- consisting of a list of declarations and a main expression
data Program = Program [Decl] Expr deriving (Show, Eq) 

