{-# LANGUAGE InstanceSigs #-}

module Eval (
  runEval,
  TermEnv,
  emptyTmenv,
  Value(..)
) where

import Syntax

import Control.Monad.Identity ( Identity(runIdentity) )
import qualified Data.Map as Map
import Control.Exception (throw, Exception)

-- Represents the runtime values your expressions can evaluate to, such as integers (VInt), booleans (VBool), closures (VClosure), and arrays (VArray).
data Value
  = VInt Integer
  | VBool Bool
  -- | VClosure String Expr TermEnv
  | VArray [Value]
  | VClosure [(Pattern, Expr)] TermEnv -- This one is flexible per your implementation
  -- TODO-1: Create a way to store arrays (VArray) ✓
  -- TODO-2: Edit VClosure to store a list of patterns and expressions 

-- TermEnv: A mapping from variable names to their corresponding values (Value). This is used to maintain the context during evaluation, especially when dealing with variables and scopes.
type TermEnv = Map.Map String Value
-- Interpreter t: An alias for Identity t, used here to simplify the structure of evaluation functions without involving complex monadic operations.
type Interpreter t = Identity t

instance MonadFail Identity where
  fail :: String -> Identity a
  fail = error

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show :: Value -> String
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure{} = "<<closure>>"
  -- TODO-1: Show VArr 
  show (VArray xs) = "[" ++ showArray xs ++ "]"
    where
      showArray [] = ""
      showArray [x] = show x
      showArray (x:xs) = show x ++ ", " ++ showArray xs
 

-- TODO-2: add a checkeq function to compare literals and values
checkeq :: Lit -> Value -> Bool
checkeq (LInt i) (VInt v) = i == v
checkeq (LBool b) (VBool v) = b == v
checkeq (LArray ls) (VArray vs) =
    -- Check if the lengths of both lists are the same and recursively check each corresponding element
    length ls == length vs && all (uncurry checkeq) (zip (map evalLit ls) vs)
  where
    evalLit :: Expr -> Lit  -- Assuming you have a function to evaluate Expr to Lit, adjust as per actual implementation
    evalLit (Lit l) = l
    evalLit _ = error "Non-literal in array literal"
checkeq _ _ = False

-- TODO-2: Add a match function to handle pattern matching
-- match :: [(Pattern, Expr)] -> Value -> (Expr, TermEnv)
-- When matching against a pattern, you can check:
-- 1. Is the pattern a PVar? -> always match, this is the generic case
-- 2. Is the pattern a PLit? -> match if the argument is equivalent to the literal
-- 3. Is the pattern a (x:xs) structure? -> match if the argument is a non-empty list
-- 4. Otherwise, check another pattern
match :: [(Pattern, Expr)] -> Value -> TermEnv -> (Expr, TermEnv)
match [] _ _ = error "Pattern match failed"
match ((PLit l, body):ps) val env
  | checkeq l val = (body, env)
  | otherwise = match ps val env
match ((PCons x xs, body):ps) (VArray (v:vs)) env =
  (body, Map.insert x v (Map.insert xs (VArray vs) env))
match ((PCons x xs, body):ps) (VArray []) env =
  (body, Map.insert x (VArray []) (Map.insert xs (VArray []) env))
match ((PVar x, body):_) val env = (body, Map.insert x val env)
match (_:ps) val env = match ps val env

-- TermEnv: This is a dictionary mapping variable names (String) to their evaluated values (Value).
-- Expr: The expression to be evaluated, which can be any construct defined in your Expr data type.
-- Interpreter Value: This is an alias for Identity Value, meaning the function will compute and return a Value directly without involving complex monadic effects.
eval :: TermEnv -> Expr -> Interpreter Value
eval env expr = case expr of
  Lit (LInt k)  -> return $ VInt k
  Lit (LBool k) -> return $ VBool k

  -- TODO-1: Handle evaluating arrays
  -- Suggestion: Use a recursive call to evaluate each element one at a time
  Lit (LArray xs) -> do
    let arr = map (runIdentity . eval env) xs
    return $ VArray arr

  -- This case fetches the value of a variable from the environment. The use of Just implies an assumption that the variable is always present in the environment
  Var x -> do
    let Just v = Map.lookup x env
    return v

  -- TODO-1: Add the Cons Operator
  -- Suggestion: Create a separate handler for this case
  --             because Cons is not the same type as other binop
  Op Cons a b -> do
    a' <- eval env a    -- Evaluate the first expression
    b' <- eval env b    -- Evaluate the second expression
    case (a', b') of
      (val, VArray vals) -> return $ VArray (val : vals)  -- Prepend if types are compatible
      _ -> fail "Type mismatch for Cons operation"        -- Fail if types are not compatible

  -- TODO-CONCAT: Add the Concat Operator
  Op Concat a b -> do
    a' <- eval env a  -- Evaluate the first expression to get the first list
    b' <- eval env b  -- Evaluate the second expression to get the second list
    case (a', b') of
      (VArray vals1, VArray vals2) -> return $ VArray (vals1 ++ vals2)  -- Concatenate lists
      _ -> fail "Concat operation requires both operands to be arrays"  -- Handle type errors

  Op op a b -> do
    VInt a' <- eval env a
    VInt b' <- eval env b
    return $ binop op a' b'

  Lam x body ->
    -- TODO-2: Change VClosure to store a list of patterns and expressions
    return $ VClosure [(x, body)] env

  App fun arg -> do
    -- TODO-2: Implement pattern matching in App
    -- Evaluate the function expression to get a function value
    funcVal <- eval env fun
    -- Evaluate the argument expression
    argVal <- eval env arg
    -- Apply the function to the argument, using pattern matching for closures
    case funcVal of
        VClosure patterns cloEnv -> applyClosure patterns argVal cloEnv
        _ -> fail "Type error: trying to apply a non-function value"
          
  Let x e body -> do
    e' <- eval env e
    let nenv = Map.insert x e' env
    eval nenv body

  If cond tr fl -> do
    VBool br <- eval env cond
    if br
    then eval env tr
    else eval env fl

  Fix e -> do
    eval env (App e (Fix e))

-- Helper function to apply a closure to an argument
applyClosure :: [(Pattern, Expr)] -> Value -> TermEnv -> Interpreter Value
applyClosure patterns argVal env = do
    let (expr, newEnv) = match patterns argVal env
    eval newEnv expr

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

-- TODO-2: Make sure that when you have a new definition for a function, you append the 
--         (pattern, body) to the environment instead of overwriting it
runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex = runIdentity $ do
  val <- eval env ex
  let updatedEnv = Map.insertWith extendPatterns nm val env
  return (val, updatedEnv)

-- Add new patterns to an existing function closure
extendPatterns :: Value -> Value -> Value
extendPatterns (VClosure newPatterns _) (VClosure existingPatterns env) = VClosure (existingPatterns ++ newPatterns) env
extendPatterns _ _ = error "Trying to extend non-closure value"