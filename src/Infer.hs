{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Infer where

import Prelude hiding (foldr)

import Type
import Syntax

import Control.Monad.State
    ( MonadState(put, get), foldM, replicateM, evalState, State )
import Control.Monad.Except
    ( MonadError(throwError), foldM, replicateM, runExceptT, ExceptT )

import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- TypeEnv: Represents the type environment, a mapping from variable identifiers (Var) to their schemes (Scheme). Schemes are generalized types that may contain type variables, allowing for polymorphic functions.
newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
  deriving (Monoid, Semigroup)

-- Unique: A simple structure for generating unique type variables
newtype Unique = Unique { count :: Int }

-- Infer: The inference monad, a combination of ExceptT for error handling (specifically TypeError) and State for managing stateful computations like generating unique identifiers.
type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar Type

-- TypeError: Enumerates possible type errors such as unification failures, infinite type errors, and unbound variable errors. These are critical for reporting where and how type inference fails.
data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

-- runInfer: Executes an inference computation, initializing with a Unique counter and handling errors to return either a TypeError or a successfully inferred type wrapped in a Scheme.
runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> Var -> Maybe Type.Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply :: Subst -> Type -> Type
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TArrow` t2) = apply s t1 `TArrow` apply s t2
  -- TODO-1: How do you apply a substitution to an array?
  apply s (TArray t) = TArray (apply s t)

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArrow` t2) = ftv t1 `Set.union` ftv t2
  -- TODO-1: What are the free variables of an array?
  ftv (TArray t) = ftv t

instance Substitutable Scheme where
  apply :: Subst -> Scheme -> Scheme
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv :: Scheme -> Set.Set TVar
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply :: Substitutable a => Subst -> [a] -> [a]
  apply = fmap . apply
  ftv :: Substitutable a => [a] -> Set.Set TVar
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply :: Subst -> TypeEnv -> TypeEnv
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv :: TypeEnv -> Set.Set TVar
  ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (l `TArrow` r) (l' `TArrow` r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return nullSubst
-- TODO-1: Unify the TArray type
unify (TArray t1) (TArray t2) = unify t1 t2
unify t1 t2 = throwError $ UnificationFail t1 t2


bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ Map.singleton a t

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Binop -> Type
ops Add = typeInt `TArrow` typeInt `TArrow` typeInt
ops Mul = typeInt `TArrow` typeInt `TArrow` typeInt
ops Sub = typeInt `TArrow` typeInt `TArrow` typeInt
ops Eql = typeInt `TArrow` typeInt `TArrow` typeBool

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

extendEnvWithPattern :: TypeEnv -> Pattern -> Type -> TypeEnv
extendEnvWithPattern env pat t = case pat of
  PVar x -> env `extend` (x, Forall [] t)
  PCons x xs -> env `extend` (x, Forall [] t) `extend` (xs, Forall [] (TArray t))
  PLit _ -> env

inferPattern :: TypeEnv -> Pattern -> Type -> Infer (Subst, Type)
inferPattern env pat t = case pat of
  PVar x -> return (nullSubst, t)
  PCons head tail -> do
    elemType <- fresh  -- Fresh type variable for elements
    (s1, t1) <- inferPattern env (PVar head) elemType
    (s2, t2) <- inferPattern env (PVar tail) (TArray elemType)  -- Tail must be an array of elemType
    s3 <- unify (apply s2 t2) (TArray elemType)  -- Unify the expected type t with TArray elemType
    return (s3 `compose` s2 `compose` s1, TArray (apply s3 t))  -- Combine substitutions and return them
  PLit (LBool _) -> return (nullSubst, TCon "Bool")  -- The type of a boolean literal is Bool
  PLit (LInt _) -> return (nullSubst, TCon "Int")  -- The type of an integer literal is Int
  PLit (LArray _) -> return (nullSubst, TArray t)  -- The type of an array literal is TArray t
  PLit _ -> return (nullSubst, t)


infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

  Var x -> lookupEnv env x

  -- TODO-2: Handle the different pattern values of `x`
  --         Each has its own implications for typing
  Lam pat e -> do
    -- Generate a fresh type variable for the pattern
    tv <- fresh
    -- Infer the type of the pattern and update the environment
    (patSubst, patType) <- inferPattern env pat tv
    -- Apply the inferred pattern substitution to the environment
    let env' = apply patSubst env
    -- Extend the environment with the new bindings from the pattern
    let env'' = extendEnvWithPattern env' pat patType
    -- Infer the type of the body in the updated environment
    (bodySubst, bodyType) <- infer env'' e
    -- Combine the substitutions
    let finalSubst = bodySubst `compose` patSubst
    -- Return the combined substitution and the function type
    return (finalSubst, patType `TArrow` bodyType)

  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArrow t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TArrow` tv `TArrow` tv `TArrow` tv)

  Fix e1 -> do
    tv <- fresh
    inferPrim env [e1] ((tv `TArrow` tv) `TArrow` tv)

  -- TODO-1: Handle the Cons operator
  -- Suggestion: Separate this from the other ops because the constraint
  --             is more generic than the other ops
  Op Cons e1 e2 -> do
    (s1, t1) <- infer env e1  -- Infer the type of the first element
    (s2, t2) <- infer (apply s1 env) e2  -- Infer the type of the second element, applying previous substitution
    s3 <- unify (apply s2 t2) (TArray t1)  -- Ensure the second expression is a list of the type of the first expression
    return (s3 `compose` s2 `compose` s1, apply s3 (TArray t1))

  -- TODO-CONCAT: Handle the Concat operator
  Op Concat e1 e2 -> do
    (s1, t1) <- infer env e1  -- Infer type of the first list
    (s2, t2) <- infer (apply s1 env) e2  -- Infer type of the second list, applying substitution from the first
    s3 <- unify (apply s2 t1) (apply s2 t2)  -- Ensure both lists have the same type
    return (s3 `compose` s2 `compose` s1, apply s3 t1)
  
  Op op e1 e2 -> do
    inferPrim env [e1, e2] (ops op)

  Lit (LInt _)  -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)
  -- TODO-1: Handle an Array literal
  -- Suggestion: Use foldM with a folding function that unifies 
  --             the result of infering on each element of the array
  Lit (LArray es) -> do
    -- Start with a fresh type variable for the array elements
    tv <- fresh
    -- Fold over the elements to unify their types
    (s, _) <- foldM (inferAndUnifyArrayElement env) (nullSubst, tv) es
    -- Return the substitution and the array type
    return (s, TArray (apply s tv))

inferAndUnifyArrayElement :: TypeEnv -> (Subst, Type) -> Expr -> Infer (Subst, Type)
inferAndUnifyArrayElement env (s, t) expr = do
  (s', t') <- infer (apply s env) expr
  s'' <- unify t (apply s' t')
  let sFinal = s'' `compose` s' `compose` s
  return (sFinal, apply sFinal t)
 

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer (apply s env) exp
    return (s' `compose` s, tf . TArrow t)

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = do
  ty <- inferExpr env ex
  inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (TArrow a b) = fv a ++ fv b
    fv (TCon _)   = []
    -- TODO-1: Handle TArray
    fv (TArray t) = fv t

    normtype (TArrow a b) = TArrow (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"
    -- TODO-1: Handle TArray
    normtype (TArray t) = TArray (normtype t)
