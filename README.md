# PolyML

A statically-typed functional programming language interpreter featuring Hindley-Milner type inference, pattern matching, and first-class functions. Implemented in Haskell.

## Overview

PolyML is a full-featured interpreter for an ML-dialect functional language, showcasing advanced type system implementation and functional programming language design. The interpreter performs complete type inference using the Hindley-Milner algorithm, enabling polymorphic functions without explicit type annotations.

### Key Features

- **Hindley-Milner Type Inference** - Complete type inference with let-polymorphism and automatic generalization
- **Pattern Matching** - Comprehensive pattern matching on literals, variables, and destructured lists
- **First-Class Functions** - Lambda expressions, closures, and higher-order functions
- **Recursive Functions** - Native support via fixpoint combinator
- **Algebraic Data Types** - Lists with structural operations (cons, concat)
- **Interactive REPL** - Read-Eval-Print Loop for interactive development

## Quick Start

```bash
# Build and run the interpreter
cabal build
cabal run

# Load and execute a program file
Poly> :load test.ml
```

## Language Examples

### Type Inference

The interpreter automatically infers polymorphic types:

```ocaml
Poly> let id x = x;
id : forall a. a -> a

Poly> let compose f g = \x -> f (g x);
compose : forall a b c. (a -> b) -> (c -> a) -> c -> b

Poly> let const x y = x;
const : forall a b. a -> b -> a
```

### Pattern Matching

Functions can pattern match on multiple cases:

```ocaml
let rec length [] = 0
let rec length (x:xs) = 1 + (length xs)

let rec map f [] = []
let rec map f (x:xs) = (f x):(map f xs)
```

### Recursive Functions

```ocaml
let rec factorial n =
  if (n == 0)
  then 1
  else (n * (factorial (n-1)));

let rec fib n =
  if (n == 0) then 0
  else if (n == 1) then 1
  else ((fib (n-1)) + (fib (n-2)));
```

### Higher-Order Functions

```ocaml
let twice f x = f (f x);
let on g f = \x y -> g (f x) (f y);

-- Function composition
let compose f g = \x -> f (g x);
let inc x = x + 1;
let double x = x * 2;
let incDouble = compose double inc;
```

## Architecture

The interpreter implements a complete language processing pipeline:

```
Source Code → Lexer → Parser → Type Inference → Evaluator → Result
```

### Core Components

| Module | Purpose |
|--------|---------|
| **Lexer.hs** | Tokenization and lexical analysis |
| **Parser.hs** | Recursive descent parser, builds AST |
| **Syntax.hs** | Abstract Syntax Tree definitions |
| **Type.hs** | Type system representation |
| **Infer.hs** | Hindley-Milner type inference algorithm |
| **Eval.hs** | Expression evaluation and runtime |
| **Pretty.hs** | Pretty-printing for types and expressions |

### Type System

The type system supports:
- **Type Variables** - Polymorphic type parameters
- **Type Constructors** - `Int`, `Bool`, custom types
- **Function Types** - `a -> b` with right-associativity
- **Parametric Types** - `[a]` for homogeneous lists
- **Type Schemes** - `forall a. a -> a` for polymorphism

### Implementation Highlights

- **Constraint-based type inference** using unification
- **Substitution-based type checking** for correctness
- **Environment-passing evaluation** with lexical scoping
- **Closure representation** for first-class functions
- **Pattern matching compilation** to decision trees

## REPL Commands

```ocaml
:type <expr>    -- Show the type of an expression
:load <file>    -- Load and execute a file
:quit           -- Exit the interpreter
```

## Building from Source

### Prerequisites

- GHC (Glasgow Haskell Compiler) >= 8.0
- Cabal >= 3.4

### Dependencies

```yaml
- base >= 4.6
- parsec >= 3.1      # Parser combinators
- mtl >= 2.2         # Monad transformers
- containers >= 0.5  # Data structures
- repline >= 0.1     # REPL infrastructure
```

### Installation

```bash
git clone https://github.com/haozhou1919/poly-lang.git
cd poly-lang
cabal install
```

## Technical Details

### Type Inference Algorithm

PolyML implements Algorithm W, a constraint-based type inference algorithm:

1. **Generate** - Traverse AST and generate type constraints
2. **Unify** - Solve constraints using Robinson's unification
3. **Generalize** - Abstract type variables into polymorphic schemes
4. **Instantiate** - Specialize polymorphic types at usage sites

### Pattern Matching Semantics

Pattern matching follows these precedence rules:

1. **Variable patterns** (`PVar`) - Always match, bind value
2. **Literal patterns** (`PLit`) - Match exact value
3. **Cons patterns** (`PCons`) - Match non-empty lists, destructure
4. **Fallthrough** - First matching pattern wins

## Examples

See [test.ml](test.ml) for a comprehensive suite of examples including:
- SKI combinator calculus
- Church encodings for arithmetic
- Boolean logic via lambda calculus
- List processing functions
- Recursive algorithms

## Credits

This implementation is based on the educational interpreter from Stephen Diehl's "Write You a Haskell" tutorial series. Extended with additional features including native list support, pattern matching, and enhanced type inference.

## License

MIT License - See [LICENSE](LICENSE) file for details.
