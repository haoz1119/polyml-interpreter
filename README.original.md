# Part 0: Familiarity and a bug fix

Stephen Diehl has put a lot of effort into building an educational interpreter
for a small language minML. ML is a language we didn't look at but will probably
appear quite familiar after working with Haskell.

For example, here are a few definitions (from [test.ml](test.ml)):

```ml
let x = 0;
let compose f g = \x -> f (g x);
let twice f x = f (f x);

let rec fib n = 
  if (n == 0) 
  then 0
  else if (n==1) 
  then 1
  else ((fib (n-1)) + (fib (n-2)));
```

Start by installing (`cabal install`) and running the code (`cabal run`) 
Play around in the interpreter.
What can you do? What can you not do? Are there any noticeable errors?

Read through some of the code (there actually isn't that much).
I would suggest reading the files in this order:

1. [Syntax.hs](src/Syntax.hs) - contains the legal syntax rules
2. [Type.hs](src/Type.hs) - defines the types
3. [Lexer.hs](src/Lexer.hs) - step 1 of running code is reading the tokens into the interpreter
4. [Parser.hs](src/Parser.hs) - step 2, convert tokens into an Abstract Syntax Tree (via Syntax.hs)
5. [Main.hs](src/Main.hs) - Check out the steps for reading a file and parsing it
6. [Eval.hs](src/Eval.hs) - Execute code according to the AST (quite simple!)
7. [Infer.hs](src/Infer.hs) - Perform HMTI on the AST
8. [Pretty.hs](src/Pretty.hs) - Define some pretty printing rules for Syntax and Type

## Fix:

While playing with the interpreter you might have noticed that there is a bug!

```ml
>>> let x = 3 + 2
Cannot unify types: 
        Int
with 
        Int -> a
```

```ml
>>> let x = 3
>>> let f y = x + y
Cannot unify types: 
        Int
with 
        Int -> a
```

Find the location of the error in the source code and make the fix:

```sh
grep -rn "TODO-0" src
```


Jonmountjoy suggestion works<br>
https://github.com/sdiehl/write-you-a-haskell/issues/98
<br>https://github.com/sdiehl/write-you-a-haskell/pull/90/files#diff-6a40138669d68f25e48d55cdbdae8652L94



# Part 1: Adding lists

Unfortunately the project is retired and the author has not completed all the features
that we would like from our language. In particular we cannot create lists! 
(Note: we actually can, using lambda calculus structures, see test.ml for examples.)

You task is to allow for native list creation, e.g.:

```lm
let w = [];                     -- empty lists
let x = [1, 2, 3];              -- lists with values in them
let y = True:[]                 -- Cons'ing 
let z = [1, 2] ++ [3, 4]         -- Concat'ing

let f x = [x, x]                -- lists that are parametric
let g x = [x, x * 2, x * 3]     -- lists that are specified
```

## Edits:

The code is annotated with TODOs to be filled in by you. 
Start by browsing the files to get a bit of familiarity with the project.
Then start tackling the TODOs in this order:
1. Lexer.hs
2. Syntax.hs
3. Parser.hs
4. Types.hs
5. Infer.hs (just add stubs)
6. Eval.hs
7. Infer.hs (finish stubs)

You can get a sense of the edits required by searching for the TODO-1's 
```sh
grep -rn "TODO-1" src
```

## Debugging:

Part of this project will be getting familiar with debugging non-trivial code in Haskell.
One thing to look for is typical print line debugging.
You can achieve this in Main.hs, in the `exec` function.

Add statements such as:

```haskell
  liftIO $ putStrLn "Test" -- Print "Test" to console
  liftIO $ print source    -- Print the source argument
  
  -- Test parsing a custom string
  res <- hoistErr $ parseExpr "3 + 2"
  liftIO $ putStrLn $ ppexpr res

  -- Print the results of the parseMod call
  liftIO $ putStrLn ("Parsed: " ++ concatMap (\(x, y) -> show x ++ " := " ++ ppexpr y ++ "\n") mod)
```

Better debugging suggestions are at the end of this document.

# Part 2: Pattern Matching

Let's add the Concat function. Find the "TODO-CAT" calls and fill those in. 
Notice how much easier this is compared to implementing Cons, now that we already
have many of the working intuitions.

Now let's add the pattern matching functionality.

```ml
let f 0 = 0
let f 1 = 1
let f n = ...

let g (x:xs) = ...
```

Note: We will _not_ require implementing pattern matching for recursive functions.
This is due to the complexity of the task. If you are looking for a challenge,
I encourage you to attempt this.

Similar to task-1, start with the lexer, parser, then move on to infer and eval.

Eval in particular will require some larger edits. This is because we need to allow
for multiple line definitions for a function. We will not be running completeness
checks for coverage of inputs.

i.e. defining a function `f 0 = 0` won't cause an error, until you evaluate it with `f 1`.

The biggest change will be in how we store a closure. Instead of `String Expr TermEnv`, 
we want the closure to store `[(Pattern, Expr)] TermEnv`, or in other words,
the closure contains a mapping between input patterns and body and a global environment map.

When matching against a pattern, you can check:
1. Is the pattern a PVar? -> always match, this is the generic case
2. Is the pattern a PLit? -> match if the argument is equivalent to the literal
3. Is the pattern a (x:xs) structure? -> match if the argument is a non-empty list
4. Otherwise, check another pattern


# Debugging with Trace

Tracing execution is tricky in a language which values purity.
The Trace monad can help.

First import trace
```hs
import Debug.Trace
```

Then, in your `do` block, simply chain the trace call to your existing execution block

```hs
-- before
eval env expr = do
  case expr of
    ...

-- after
eval env expr = do
  trace ("Log: " ++ show expr) do
    case expr of
      ...
```

Read more about [tracing](https://hackage.haskell.org/package/base-4.19.1.0/docs/Debug-Trace.html).
