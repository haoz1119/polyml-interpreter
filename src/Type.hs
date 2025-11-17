module Type where -- Define a module named Type

-- Define a newtype for type variables, which wraps a String.
newtype TVar = TV String
  deriving (Show, Eq, Ord)


-- Define the Type data type representing types in a language
data Type
  = TVar TVar            -- A type variable
  | TCon String          -- A type constructor, which could represent simple types like "Int", "Bool", etc.
  | TArrow Type Type     -- Represents function types, where one Type is the input and the other is the output
  -- TODO-1: Add a type for arrays (TArray) ✓
  | TArray Type
  deriving (Show, Eq, Ord) -- Automatically derive instances for Show, Eq, and Ord

-- Make TArrow right-associative to facilitate function type declarations
-- e.g., Type -> Type -> Type is parsed as Type -> (Type -> Type)
infixr `TArrow`

-- Define the Scheme data type for type schemes
-- Type schemes represent generalized types, such as those in polymorphic functions
data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

-- Define constant typeInt to represent the "Int" type using a type constructor
typeInt :: Type
typeInt  = TCon "Int"

-- Define constant typeBool to represent the "Bool" type using a type constructor
typeBool :: Type
typeBool = TCon "Bool"
