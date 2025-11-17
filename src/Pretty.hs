{-# Language FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Pretty (
  ppscheme,
  pptype,
  ppexpr,
  ppsignature,
  ppenv,
  ppdecl
) where

import Type
import Syntax
import Infer

import Prelude hiding ((<>))

import Text.PrettyPrint
    ( (<+>),
      (<>),
      hcat,
      integer,
      parens,
      punctuate,
      render,
      space,
      text,
      Doc )
import qualified Data.Map as Map

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id


class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Var where
    ppr :: Int -> Var -> Doc
    ppr _ = text

instance Pretty TVar where
    ppr :: Int -> TVar -> Doc
    ppr _ (TV x) = text x

instance Pretty Type where
  ppr :: Int -> Type -> Doc
  ppr p (TArrow a b) = parensIf (isArrow a) (ppr p a) <+> text "->" <+> ppr p b
    where
      isArrow TArrow{} = True
      isArrow _ = False
  ppr p (TVar a) = ppr p a
  ppr _ (TCon a) = text a
  -- TODO-1: Add the TArray printing
  ppr p (TArray a) = text "[" <> ppr p a <> text "]"

instance Pretty Scheme where
  ppr :: Int -> Scheme -> Doc
  ppr p (Forall [] t) = ppr p t
  ppr p (Forall ts t) = text "forall" <+> hcat (punctuate space (fmap (ppr p) ts)) <> text "." <+> ppr p t

instance Pretty Binop where
  ppr :: Int -> Binop -> Doc
  ppr _ Add = text "+"
  ppr _ Sub = text "-"
  ppr _ Mul = text "-"
  ppr _ Eql = text "=="
  -- TODO-1: Add cons handling
  ppr _ Cons = text ":"
  -- TODO-2: Add concat handling
  ppr _ Concat = text "++"

instance Pretty Pattern where
  ppr :: Int -> Pattern -> Doc
  ppr p (PVar x) = ppr p x  -- variable pattern
  ppr p (PLit l) = ppr p l  -- literal pattern
  ppr p (PCons head tail) = ppr p head <+> text ":" <+> ppr p tail  -- constructor pattern for lists

instance Pretty Expr where
  ppr :: Int -> Expr -> Doc
  ppr p (Var a) = ppr p a
  ppr p (App a b) = parensIf (p > 0) $ ppr (p+1) a <+> ppr p b
  ppr p (Lam a b) = text "\\" <> ppr p a <+> text  "->" <+> ppr p b
  ppr p (Let a b c) = text "let" <> ppr p a <+> text  "=" <+> ppr p b <+> text "in" <+> ppr p c
  ppr p (Lit a) = ppr p a
  ppr p (Op o a b) = parensIf (p>0) $ ppr p a <+> ppr p o <+> ppr p b
  ppr p (Fix a) = parensIf (p>0) $ text "fix" <> ppr p a
  ppr p (If a b c) =
    text "if" <> ppr p a <+> text "then" <+> ppr p b <+> text "else" <+> ppr p c

instance Pretty Lit where
  ppr :: Int -> Lit -> Doc
  ppr _ (LInt i) = integer i
  ppr _ (LBool True) = text "True"
  ppr _ (LBool False) = text "False"
  -- TODO-1: Add LArray case
  ppr p (LArray xs) = text "[" <> hcat (punctuate (text ", ") (fmap (ppr p) xs)) <> text "]"
-- TODO-2: Add the Pretty Pattern instance

instance Show TypeError where
  show :: TypeError -> String
  show (UnificationFail a b) =
    concat ["Cannot unify types: \n\t", pptype a, "\nwith \n\t", pptype b]
  show (InfiniteType (TV a) b) =
    concat ["Cannot construct the infinite type: ", a, " = ", pptype b]
  show (UnboundVariable a) = "Not in scope: " ++ a

ppscheme :: Scheme -> String
ppscheme = render . ppr 0

pptype :: Type -> String
pptype = render . ppr 0

ppexpr :: Expr -> String
ppexpr = render . ppr 0

ppsignature :: (String, Scheme) -> String
ppsignature (a, b) = a ++ " : " ++ ppscheme b

ppdecl :: (String, Expr) -> String
ppdecl (a, b) = "let " ++ a ++ " = " ++ ppexpr b

ppenv ::  TypeEnv -> [String]
ppenv (TypeEnv env) = ppsignature <$> Map.toList env
