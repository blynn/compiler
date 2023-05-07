= ION Assembly =

By "computer program", we often mean a string intended for human
comprehension. In contrast, language specifications usually involve more
abstract objects such as syntax trees, where we can ignore details such as
which characters are treated as whitespace.

A parser maps a string to a syntax tree, or an error if the input string is
not a valid program.

------------------------------------------------------------------------
parse :: [Char] -> Either Error Language
------------------------------------------------------------------------

Parsing is often viewed as part of the compiler's job:

------------------------------------------------------------------------
parseCompile :: [Char] -> Either Error TargetLanguage
parseCompile s = case parse s of
  Left  err  -> Left  err
  Right prog -> Right (compile prog)
------------------------------------------------------------------------

Our `TargetLanguage` is a sort of assembly language for the ION machine, where
we use a string representation of a list of `CL` terms. (Why a list instead
of a single `CL`? See below.)

------------------------------------------------------------------------
type TargetLanguage = [Char]
assemble :: TargetLanguage -> Either Error [CL]
------------------------------------------------------------------------

We also need some way to load an ION machine with a `[CL]`:

------------------------------------------------------------------------
load :: [CL] -> VM
------------------------------------------------------------------------

We build `assemble` and `load` below:

++++++++++
<script>
function hideshow(s) {
  var x = document.getElementById(s);
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}
</script>
<p><a onclick='hideshow("ugh");'>&#9654; Toggle extensions and imports</a></p>
<div id='ugh' style='display:none'>
++++++++++

\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad
import Data.Char (ord)
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified ION
import ION (VM)
\end{code}

++++++++++
</div>
++++++++++

== Grammar ==

We define a language that is easy to generate and parse, and that humans can
tolerate in small doses:

------------------------------------------------------------------------
digit = '0' | ... | '9'
num = digit [num]
con = '#' CHAR | '(' num ')'
idx = '@' CHAR | '[' num ']'
comb = 'S' | 'K' | 'I' | ...
term = comb | '`' term term | con | idx
prog = term ';' [prog]
------------------------------------------------------------------------

A program is a sequence of terms terminated by semicolons.
The entry point is the last term of the sequence.

The motivation for a list instead of a single term is that rather than:

------------------------------------------------------------------------
norm = (\sq x y -> sq x + sq y) (\x -> x * x)
------------------------------------------------------------------------

we'd like to write the sequence of definitions:

------------------------------------------------------------------------
square x = x * x
norm x y = square x + square y
------------------------------------------------------------------------

The semicolon terminators in our grammar are unnecessary, but aid debugging.

The backquote is a prefix binary operator denoting application.
For example, we represent the program `BS(BB)` with:

------------------------------------------------------------------------
``BS`BB;
------------------------------------------------------------------------

A term is represented by the last element of a sequence of expressions
terminated by semicolons. The sequence is 0-indexed, and we may refer to an
earlier term by enclosing its index in square brackets.

Thus another way to represent `BS(BB)` is:

------------------------------------------------------------------------
B;S;``[0][1]`[0][0];
------------------------------------------------------------------------

An term may refer to an earlier term using the `(@)` prefix unary
operator. Let `n` be the ASCII code of the character following `(@)`. Then
it refers to the term of index `n - 32`. This offset is chosen so that short
programs can be written in printable ASCII without worrying about decimal
conversion.

For example, instead of "[0]" we may write "@ "
and instead of "[10]" we may write "@*".

If an earlier term is needed multiple times, then we share rather than
duplicate. Accordingly, we define combinatory logic terms to be combinators,
applications, and indexes of previously defined terms:

\begin{code}
data CL = Com Int | CL :@ CL | Nat Int | Idx Int
\end{code}

An 32-bit word constant is represented by a number in parentheses. For example,
42 is represented by `(42)`.

An alternative representation is to use the unary prefix operator `(#)`, in
which case the constant is the ASCII code of the following character. Again,
this is so we can generate code without worrying about converting to decimal.

For example, instead of `(42)` we may write `#*`.

== Parser ==

We use Megaparsec to build a recursive descent parser for ION assembly.

\begin{code}
digit = digitChar
num = read <$> some digit
nat = ord <$> (char '#' *> anySingle)
  <|> char '(' *> num <* char ')'
idx = (+(-32)) . ord <$> (char '@' *> anySingle)
  <|> char '[' *> num <* char ']'
comb = Com . ord <$> oneOf "SKIBCTRY:L=+-/*%?"
term = comb
  <|> ((:@) <$> (char '`' *> term) <*> term)
  <|> Nat <$> nat
  <|> Idx <$> idx

prog :: Parsec () [Char] [CL]
prog = some (term <* char ';')
\end{code}

Thus our `assemble` function is simply:

\begin{code}
assemble = parse prog ""
\end{code}

We load terms onto a VM as follows.

\begin{code}
fromExpr :: [(Int, Int)] -> CL -> VM -> (Int, VM)
fromExpr defs t vm = case t of
  Com n -> (n, vm)
  x :@ y -> let
    (a, vm1) = fromExpr defs x vm
    (b, vm2) = fromExpr defs y vm1
    in ION.app a b vm2
  Nat n -> ION.app 35 n vm
  Idx n | Just addr <- lookup n defs -> (addr, vm)

load :: [CL] -> VM
load ts = ION.push root vm1 where
  ((_, root):_, vm1) = foldl addDef ([], ION.new) ts
  addDef (defs, m) t = let (addr, m') = fromExpr defs t m
    in ((length defs, addr):defs, m')
\end{code}

A helper turns a given pure CL program into one that runs on standard input and
output by mapping a term `x` to `x(0?)(.)(T1)`:

\begin{code}
wrapIO :: CL -> CL
wrapIO x = x :@ (Com 48 :@ Com 63) :@ Com 46 :@ (Com 84 :@ Com 49)
\end{code}

Lastly, a `main` function which runs the program in the file whose name is
supplied as the first argument:

\begin{code}
main :: IO ()
main = getArgs >>= \as -> case as of
  []  -> putStrLn "Must provide program."
  [f] -> do
    s <- readFile f
    case assemble s of
      Left err -> putStrLn $ "Parse error: " <> show err
      Right cls -> do
        void $ ION.evalIO $ load $ cls ++ [wrapIO $ Idx $ length cls - 1]
  _   -> putStrLn "One program only."
\end{code}

For example:

------------------------------------------------------------------------
$ echo -n 'I;' > id
$ echo "Hello" | ion id
Hello
$ echo -n '``C`T?`KI;' > tail
$ echo "tail" | ion tail
ail
------------------------------------------------------------------------

== Parsing Pitfalls ==

Sadly, we largely ignore the topic of parsing because we wish to focus on
lambda calculus, combinatory logic, and types. We get by with parser
combinators, which we hardly explain.

This is a pity, because my textbooks omitted several fascinating facts, partly
due to their vintage:

  * Many recommend Thompson's construction algorithm for building DFAs from
  regular expressions. In fact, link:../haskell/re.html[regular expression
  derivatives] are superior. (In practice, NFAs may be better to
  because conversion to DFAs may blow up exponentially.)
  See also http://stedolan.net/research/semirings.pdf[_Fun with Semirings_] for
  lesser-known connections between regular languages and other parts of
  computer science.

  * link:../haskell/parse.html[Parsing combinators] take the tedium out of recursive descent parsers, at least in languages like Haskell.

  * link:../haskell/pwd.html[Parsing with derivatives] makes it easy to write an efficient context-free parser generator.

  * https://en.wikipedia.org/wiki/Parsing_expression_grammar[Parsing expression grammars] can be parsed in linear time and can even handle some context-sensitive grammars, though it is unknown whether they can handle all context-free grammars.
