= Theorems for free for the unenlightened =

Want to see a magic trick? Pick a function, any function:

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<p>
<button id='id'>id</button>
<button id='const'>const</button>
<button id='concat'>concat</button>
<button id='sort'>sort</button>
<button id='fold'>fold</button>
</p>
<p>
<textarea id='in' rows='1' style='box-sizing:border-box;width:100%;'></textarea>
</p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Watch closely...

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<p>
<button id='magic'>Abracadabra!</button>
</p>
<p>
<textarea id='out' rows='8' style='box-sizing:border-box;width:100%;'></textarea>
</p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

A good candidate for link:eq.html[equational reasoning]?

== A rose by any other name ==

How do we automatically discover a theorem for any function?

Consider a Haskell function:

------------------------------------------------------------------------
foo :: [a] -> [a]
------------------------------------------------------------------------

Suppose `foo "schooled" == "shoe"`. What is `foo "SCHOOLED"`?

It must be `"SHOE"`.
The function `foo` has black-box access to the elements of its input list, that
is, it can only perform some combination of rearranging or duplicating or
discarding the given letters. If `foo` were able to exploit some property of
characters then it would instead have type `[Char] -> [Char]`. We relabeled
the input, so the output must be exactly what it was before, except relabeled.

We could go beyond letters. If we replace each input letter with its ASCII
code, we obtain "shoe" in ASCII. In other words, given a relabeling function:

------------------------------------------------------------------------
relabel :: Char -> Int
------------------------------------------------------------------------

we have the rewrite rule, or theorem:

------------------------------------------------------------------------
foo (map relabel xs) = map relabel (foo xs)
------------------------------------------------------------------------

There's nothing special about `Char` and `Int`. This holds for any `relabel`
function from any type to any other type.

We can conjure up a theorem from any type signature. (We're really just
specializing the parametricity theorem to a particular case.) The resulting
theorem is nontrivial if a type variable is present.

Let's walk through some examples. Suppose we have a function:

------------------------------------------------------------------------
bar :: a - > b -> a
------------------------------------------------------------------------

There are two type variables, which may have independent relabeling
functions. So for any functions `relabelA, relabelB` we have:

------------------------------------------------------------------------
bar (relabelA x) (relabelB y) = relabelA (bar x y)
------------------------------------------------------------------------

The left-hand side relabels before applying `bar`, while the right-hand
side applies `bar` before relabeling.

It's slightly tougher when an argument is a function:

------------------------------------------------------------------------
baz :: (a -> b -> b) -> b -> [a] -> b
------------------------------------------------------------------------

We can relabel the other arguments easily enough;
for any functions `relabelA, relabelB` we have:

------------------------------------------------------------------------
baz f' (relabelB y) (map relabelA ys) = relabelB (baz f x ys)
------------------------------------------------------------------------

where we still must address the relationship between `f'` and `f`.

Recursion is the answer. We equate the relabeled output of `f` with the output
of `f'` run on relabeled inputs:

------------------------------------------------------------------------
f' (relabelA x) (relabelB y) = relabelB (f x y)
------------------------------------------------------------------------

If this equality holds, then our first equality holds.

More generally, if there are function arguments within function arguments,
then we recurse deeper.

The code below automates the above for a type system limited to type
constants, type variables, functions, and lists. We refer to lists as
functors to stress we can replace any list with any Functor instance.

The `theorize` function returns a list of equalities, the head of which
is true whenever all of the equalities in the tail are true.

Lists of functions are irksome. We expediently introduce lambdas rather
than figuring out how to write the relabeling condition with combinators.

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
<p><a onclick='hideshow("boilerplate");'>&#9654; Toggle boilerplate</a></p>
<div id='boilerplate' style='display:none'>
++++++++++

\begin{code}
{- To compile with GHC, replace boilerplate with:
{-# LANGUAGE FlexibleContexts, LambdaCase #-}
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.State
import Data.List
type Charser = Parsec () String
-}
module Main where
import Base
import System
import Charser
foreign export ccall "main" main
\end{code}

Yuck!

++++++++++
</div>
++++++++++

\begin{code}
infixr 5 :->
data Type = TFunctor Type | TC String | TV String | Type :-> Type deriving Show
data Expr = Var String | Expr :@ Expr | Lam String Expr
data Theorem = Expr := Expr

data Scratch = Scratch  -- Scratch space.
  { varCount :: Int
  , conds :: [Theorem]
  }

theorize :: String -> Type -> [Theorem]
theorize fun t = evalState (theorize' (Var fun) (Var fun) t)
  $ Scratch 0 []

theorize' :: Expr -> Expr -> Type -> State Scratch [Theorem]
theorize' lhs rhs t = case t of
  a :-> b -> do
    v <- genVar
    v' <- relabel id a v
    theorize' (lhs :@ v') (rhs :@ v) b
  _ -> do
    r <- relabel id t rhs
    scr <- get
    pure $ (lhs := r) : conds scr

relabel :: (Expr -> Expr) -> Type -> Expr -> State Scratch Expr
relabel f t v = case t of
  TC _ -> pure v
  TV s -> pure $ f (Var $ s <> "R") :@ v
  TFunctor u -> relabel (f . (Var "fmap" :@)) u v
  _ :-> _ -> do
    v' <- case v of
      Var s -> pure $ Var $ s <> "'"
      _ -> genVar
    let
      ((vL, postL), (vR, postR)) = case f (Var "") of
        Var "" -> ((v', id), (v, id))
        _ -> (functored v', functored v)
      functored vv = (Var "l", \x -> f (Lam "l" x) :@ vv)
    s0 <- get
    let
      ((xL := xR):xt, s1) = runState (theorize' vL vR t)
        $ Scratch (varCount s0) []
    put s0
      { conds = conds s0 <> ((postL xL := postR xR):xt)
      , varCount = varCount s1
      }
    pure v'

genVar :: State Scratch Expr
genVar = do
  s <- get
  let n = varCount s
  put s { varCount = n + 1 }
  pure $ Var $ 'x' : show n
\end{code}

Product and coproduct types are hopefully only a little more work, but may mean
some shortcuts we took are unavailable.

That leaves the boring stuff: parsing types, pretty-printing theorems,
interfacing with this webpage, and so on.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<p><a onclick='hideshow("boring");'>&#9654; Toggle boring stuff</a></p>
<div id='boring' style='display:none'>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

\begin{code}
instance Show Expr where
  show = \case
    Var s -> s
    a :@ b -> show a <> " " <> showR b
    Lam s x -> "(\\" <> s <> " -> " <> show x <> ")"
    where
    showR x = case x of
      _ :@ _ -> "(" <> show x <> ")"
      _ -> show x

instance Show Theorem where
  show (l := r) = show l <> " = " <> show r

decl :: Charser (String, Type)
decl = (,) <$> sp ((:) <$> lowerChar <*> many alphaNumChar)
  <*> (sp (string "::") *> typ)
  where
  typ = foldr1 (:->) <$>
    ((:) <$> typeAtom <*> many (sp (string "->") *> typeAtom))
  typeAtom = typeCon
    <|> typeVar
    <|> between (spch '[') (spch ']') (TFunctor <$> typ)
    <|> between (spch '(') (spch ')') typ
  typeCon = TC <$> sp ((:) <$> upperChar <*> many alphaNumChar)
  typeVar = TV <$> sp ((:) <$> lowerChar <*> many alphaNumChar)
  sp :: Charser a -> Charser a
  sp   = (<* space)
  spch = sp . char

gvs :: Type -> [String]
gvs = \case
  TC _ -> []
  TV v -> [v]
  TFunctor t -> gvs t
  x :-> y -> gvs x `union` gvs y

pretty :: Type -> [Theorem] -> String
pretty t (thm:conds) = "Theorem: For all functions "
  <> unwords ((<> "R") <$> gvs t) <> ",\n" <> case conds of
  [] -> "  " <> show thm
  _ -> concat
    [ "If:\n"
    , unlines $ ("  " <>) . show <$> conds
    , "then:\n  "
    , show thm
    ]

go :: String -> String
go s = case parse (decl <* eof) "" s of
  Left err -> show err
  Right (s, t) -> pretty t $ theorize s t

main :: IO ()
main = interact go
\end{code}

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
</div>
<script>
function setup(s) {
  const name = s.substr(0, s.indexOf(" "));
  function act() {
    document.getElementById("in").value = s;
    document.getElementById("out").value = "";
  }
  document.getElementById(name).addEventListener("click", act);
  if (name == "concat") act();
}

setup("id :: a -> a");
setup("const :: a -> b -> a");
setup("concat :: [[a]] -> a");
setup("sort :: (a -> a -> Bool) -> [a] -> [a]");
setup("fold :: (a -> b -> b) -> b -> [a] -> b");

const ctx = {};

function presto() {
  const s = document.getElementById("in").value;
  ctx.out = [];
  ctx.inp = (new TextEncoder()).encode(s);
  ctx.cursor = 0;
  ctx.instance.exports["main"]();
  document.getElementById("out").value = (new TextDecoder()).decode(Uint8Array.from(ctx.out));
}

async function loadWasm() {
  try {
    ctx.instance = (await WebAssembly.instantiateStreaming(fetch('para.wasm'), {env:
      { putchar: c  => ctx.out.push(c)
      , eof    : () => ctx.cursor == ctx.inp.length
      , getchar: () => ctx.inp[ctx.cursor++]
      }})).instance;

    document.getElementById("in").addEventListener("keydown", (event) => { if (event.key == "Enter") { presto(); event.preventDefault(); }});
    document.getElementById("magic").addEventListener("click", (event) => presto());
  } catch(err) {
    console.log(err);
  }
}
loadWasm();
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

== Already paid for ==

I had difficulty following the paper introducing
https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf[theorems for free]
even after reading
https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/[explanations with pretty pictures].

I'm coping by keeping relabeling in mind, despite this being technically
incorrect. Apparently, we must generalize from functions to relations (only to
specialize back to functions to produce theorems).

I hope to read related papers to truly understand, but I'm also considering
learning more category theory instead, as it all might be
https://reasonablypolymorphic.com/blog/theorems-for-free/[a basic application
of category theory]. (A colleague pointed out
https://ncatlab.org/nlab/show/Adjointness%20in%20Foundations[quantifiers can be
constructed as adjoints], which may fill in a missing piece of the puzzle.)
On the other hand, the wording near the end of section 3 of that
https://maartenfokkinga.github.io/utwente/mmf91m.pdf[bananas, lenses, envelopes
and barbed wire paper] suggests there is a theorem to be proved, and the result
can be viewed as a natural transformation.
