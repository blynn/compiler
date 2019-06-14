== Parsing ==

A computer program is usually stored as a string intended for human
consumption.In contrast, language specifications usually concentrate on an
abstract representation, such as a syntax tree, where we can ignore details
such as which characters count as whitespace.

A parser is a function taking a string to its corresponding syntax tree, or an
error if the string is not a valid program.

------------------------------------------------------------------------------
parse :: [Char] -> Either Error Language
------------------------------------------------------------------------------

Sometimes parsing is broken into more steps. A lexer tries to turn a string
into a list of tokens, which the parser tries to turn into program.

------------------------------------------------------------------------------
lex :: [Char] -> Either Error [Token]
parse :: [Token] -> Either Error Language
------------------------------------------------------------------------------

Parsing is often viewed as part of the compiler's job, so a better
definition of `compile` may be:

------------------------------------------------------------------------------
compile :: [Char] -> Either Error TargetLanguage
------------------------------------------------------------------------------

In general we may also need to transform a program in `TargetLanguage` to some
binary format. For the ION machine, we'll define a sort of assembly language
that will be our `TargetLanguage`.

\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
import Data.Char (chr, ord)
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Char (ord)
import Text.Megaparsec
import Text.Megaparsec.Char
data VM = VM
  { sp :: Int
  , hp :: Int
  , mem :: Map Int Int
  } deriving Show 

new :: VM
new = VM maxBound 128 mempty

load :: Int -> VM -> Int
load k vm = mem vm ! k

store :: Int -> Int -> VM -> VM
store k v vm = vm{mem = M.insert k v $ mem vm}

app :: Int -> Int -> VM -> (Int, VM)
app x y vm@VM{hp} = (hp, store hp x $ store (hp + 1) y $ vm { hp = hp + 2 })

push :: Int -> VM -> VM
push n vm@VM{sp} = store sp n $ vm{sp = sp - 1}
\end{code}

== ION assembly ==

We define a language that is easy to generate and parse, and that humans can
tolerate in small doses:

------------------------------------------------------------------------------
digit = '0' | ... | '9'
num = digit [num]
con = '#' CHAR | '(' num ')'
idx = '@' CHAR | '[' num ']'
comb = 'S' | 'K' | 'I' | ...
term = comb | '`' term term | con | idx
prog = term ';' [prog]
------------------------------------------------------------------------------

A program is a sequence of terms terminated by semicolons.
The entry point is the last term of the sequence.

The motivation for supporing a sequence instead of insisting on a single term
is that instead of writing programs of the form:

------------------------------------------------------------------------------
norm = (\sq x y -> sq x + sq y) (\x -> x * x)
------------------------------------------------------------------------------

we can write:

------------------------------------------------------------------------------
square x = x * x
norm x y = square x + square y
------------------------------------------------------------------------------

The semicolon terminators are unnecessary, but greatly aid human
comprehension.

The backquote is a prefix binary operator denoting application.
For example, we represent the program `BS(BB)` with:

------------------------------------------------------------------------------
``BS`BB;
------------------------------------------------------------------------------

A term is represented by the last element of a sequence of expressions
terminated by semicolons. The sequence is 0-indexed, and we may refer to an
earlier term by enclosing its index in square brackets.

Thus another way to represent `BS(BB)` is:

------------------------------------------------------------------------------
B;``[0]S`[0][0];
------------------------------------------------------------------------------

An term may refer to an earlier term using the `(@)` prefix unary
operator. Let `n` be the ASCII code of the character following `(@)`. Then
it refers to the term of index `n - 32`. This offset is chosen so that short
programs can be written in printable ASCII without worrying about decimal
conversion.

For example, instead of `[0]` we may write `@ ` and instead of `[10]` we may
write `@*`.

If an earlier term is needed multiple times, then we share it rather than
duplicate it.

Because of indexes that refer to previously defined terms, we define `Expr`
which replaces the role played by `Exp`:

\begin{code}
data Expr = Comb Int | Expr :@ Expr | Nat Int | Idx Int deriving Show 
\end{code}

An 32-bit word constant is represented by a number in parentheses. For example,
42 is represented by `(42)`.

An alternative representation is to use the unary prefix operator `(#)`, in
which case the constant is the ASCII code of the following character.  Again,
this allows us to generate programs without dealing about decimal conversion.

For example, instead of `(42)` we may write `#*`.

Newlines are ignored, so they may be used for layout.

== Parser ==

We use Megaparsec to build a recursive descent parser for our language.

\begin{code}
digit = oneOf ['0'..'9']
num = read <$> some digit 
nat = ord <$> (char '#' *> anySingle)
  <|> char '(' *> num <* char ')'
idx = (+(-32)) . ord <$> (char '@' *> anySingle)
  <|> char '[' *> num <* char ']'
comb = Comb . ord <$> oneOf "SKIBCT"
term = comb
  <|> ((:@) <$> (char '`' *> term) <*> term)
  <|> Nat <$> nat
  <|> Idx <$> idx

prog :: Parsec () String [Expr]
prog = some term
\end{code}

We serialize terms as follows.

\begin{code}
fromExpr' :: [(Int, Int)] -> Expr -> VM -> (Int, VM)
fromExpr' defs t vm = case t of
  Comb n -> (n, vm)
  x :@ y -> let
    (a, vm1) = fromExpr' defs x vm
    (b, vm2) = fromExpr' defs y vm1
    in app a b vm2
  Nat n -> app 35 n vm
  Idx n | Just addr <- lookup n defs -> (addr, vm)

fromProg :: [Expr] -> VM -> VM
fromProg ts vm = push root vm1 where
  (prog, vm1) = foldl addDef ([], vm) ts
  root = snd $ last prog
  addDef (defs, m) t = let (addr, m') = fromExpr' defs t m
    in ((length defs, addr):defs, m')
\end{code}
