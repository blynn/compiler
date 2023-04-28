= Differential Lambda Calculus =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<p>
<button id='example'>C to F</button>
<button id='lambda'>clover</button>
<button id='implicit'>closed lambda term</button>
<button id='second'>second derivative</button>
</p>
<p>
<textarea id='in' rows='1' style='box-sizing:border-box;width:100%;'></textarea>
</p>
<p>
<button id='go'>Evaluate</button>
</p>
<p>
<textarea id='out' rows='4' style='box-sizing:border-box;width:100%;'></textarea>
</p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

How do we convert Celsius to Farenheit? For the sake of argument, suppose
we know to multiply by a constant `p` then add another constant `q`:

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
module Main where
import Base
import Charser
import System
foreign export ccall "main" main
{- GHC edition:
import Text.Megaparsec
import Text.Megaparsec.Char
type Charser = Parsec () String
-}
\end{code}

++++++++++
</div>
++++++++++

\begin{code}
type R = Double
convert:: R -> R -> R -> R
convert p q x = p * x + q
\end{code}

...but we have forgotten the constants. So we guess their values and hope for
the best:

\begin{code}
stab :: R -> R
stab = convert (1.23) (4.56)
\end{code}

Furthermore, suppose we remember water boils at 100 degrees Celsius or 212
degrees Farenheit. We run `stab 100`. Instead of `212` we see `127.56`. We
guessed wrong.

We can measure for how far off we are for a given input `x` and expected output
`y`.

\begin{code}
oops :: R -> R -> R -> R -> R
oops p q x y = (convert p q x - y)^2
\end{code}

In our case, the error is:

------------------------------------------------------------------------
oops 1.23 4.56 100 212 == 7130.1136
------------------------------------------------------------------------

How can we fix our guesses for `p, q` so that for all test cases `x, y`, the
error `oops p q x y` is zero, or at least close to zero?

Let's focus on `p`. Define:

------------------------------------------------------------------------
focusp p = oops p 4.56 100 212
------------------------------------------------------------------------

We approximate `focusp` around `p = 1.23` with a straight line, using a method
we later describe:

------------------------------------------------------------------------
focusp (1.23 + dp) ~= focusp 1.23 - 16888 * dp
------------------------------------------------------------------------

This minus sign tells us something. Nudging `p` upwards from 1.23 reduces the
error `focusp p` quite a bit, so we decide to increase `p` by a small amount.
If instead the coefficient of `dp` were positive, we would decrease `p`.

We switch focus to `q` and find a linear approximation in terms of a variable
`dq` at `q = 4.56`, and adjust `q` similarly, at which point we repeat the
whole process using our improved guesses for `p` and `q`. With luck, we'll home
in on the correct constants after iterating many times.
This technique is known as 'gradient descent'.

(We could be cleverer and modify our guesses so the approximate error is
exactly zero, which is called
https://en.wikipedia.org/wiki/Newton%27s_method[Newton's method]. Does it work
well for machine learning?
https://stats.stackexchange.com/questions/253632/why-is-newtons-method-not-widely-used-in-machine-learning[Good
question!])

Recall at our particular choices of `p, q, x, y`, the coefficient of `dp` is
`-16888`. It turns out in general, the coefficient of `dp` is:

\begin{code}
oopsp p q x y = 2*x*(convert p q x - y)
\end{code}

and that of `dq` is:

\begin{code}
oopsq p q x y = 2*(convert p q x - y)
\end{code}

Next, we remember another piece of trivia: minus 40 is the same temperature in
Celsius and Farenheit. This gives us a second test case, that is,
`convert p q (-40) == -40` when `p, q` are correct.

We repeatedly nudge our guesses according to our two test cases:

\begin{code}
rate = 0.0001
step x y (p, q) = (p - oopsp p q x y * rate, q - oopsq p q x y * rate)
learn = iterate (step 100 212 . step (-40) (-40)) (1.23, 4.56)
\end{code}

where we've defined the size of a nudge to be 0.0001 times the slope of the
linear approximation. This is because the steeper the slope, the greater the
error, so we are correspondingly bolder about changing our guess.

We find:

------------------------------------------------------------------------
learn!!50000 == (1.8000000000067133,31.99999999798702)
------------------------------------------------------------------------

which is close to the true constants (9/5, 32).

Why choose 0.0001? This fussy parameter is known as the 'learning rate'. The
smaller it is, the longer it takes to reach a good answer. But if it is too
large, those gentle nudges become violent shoves and our guesses are forever
all over the map. Researchers have explored many ideas to tune the learning
rate, but in our case, we picked 0.0001 because it happened to work well for
our toy example!

The `convert` function is simple enough that there are direct ways of figuring
out the right answer. However, we can imagine problems where the function
involves thousands of guessed parameters. As long as we can compute linear
approximations for the error with respect to each parameter, we can iteratively
improve our guesses using the above process.

It remains to explain how we obtained linear approximations.
In short, we used differential calculus.

== With no undue respect ==

We say "differential calculus" and not "derivative calculus", yet we tend to
think in terms of derivatives and not differentials. Faced with an expression,
our first instinct is to single out a variable and take a derivative with
respect to this variable. Why?

The root cause is a myth perpetuated by generation after generation of teachers
and textbooks, that claim the Leibniz notation $dy/dx$ is a mere mnemonic
device. Woe betide those who dare to reason algebraically with it!

This is a lie. Below, we define the differential $d$ as a function from lambda
terms to lambda terms, and $dy/dx$ is an algebraic expression like any other.
As we'd expect, it means $d$ applied to $y$ divided by $d$ applied to $x$.

Knowing the truth lets us forget derivatives and study differentials such as:

\[
d(3 x^2 + 2 y) = 6 x dx + 2 dy
\]

A differential describes the effects of small changes in the inputs.
More precisely, it is the best linear approximation to a given function at a
given point. Unlike derivatives, no variable gets special treatment.
Differentials respect all variables equally.

Above, we see changing $x$ by $dx$ leads to a change of approximately $6x dx$
in the output value, and similarly changing $y$ by $dy$ leads to an change of
approximately $2 dy$. The plane through $(x, y)$ with slope $6x$ in the
$x$-direction and $2$ in the $y$-direction lies tangent to the surface
represented by this function.

If we discover $y$ depends on $x$, say $y = x^3$, then we can compute the
differential $dy = 3x^2 dx$ and substitute into the above to get
$(6x + 6x^2) dx$ using plain algebra.

After years of heavy use of derivatives, taking differentials may seem alien.
Fortunately, a few lines saves us from figuring them out ourselves.

\begin{code}
data V = S String | Dee V deriving Eq
data Expr = Con Int | Var V | Lam V Expr | Expr :@ Expr
  | Inv | Sin | Cos | Exp | Log | Expr :+ Expr | Expr :* Expr | Expr :^ Expr

d :: Expr -> Expr
d expr = case expr of
  Con _ -> Con 0
  Var v -> Var $ Dee v
  x :+ y -> d x :+ d y
  x :* y -> (x :* d y) :+ (d x :* y)
  x :^ y -> (y :* (x :^ (y :+ Con (-1))):* d x)
    :+ ((Log :@ x) :* (x :^ y) :* d y)
  Lam v x -> Lam (Dee v) $ d x
  f :@ x | Lam (Dee v) y <- d f -> sub (Dee v) (d x) $ sub v x y
  Inv -> lzdz $ Con (-1) :* (Inv :@ (z :* z))
  Log -> lzdz $ Inv :@ z
  Exp -> lzdz $ Exp :@ z
  Sin -> lzdz $ Cos :@ z
  Cos -> lzdz $ Con (-1) :* (Sin :@ z)
  where
  z = Var $ S "z"
  lzdz x = Lam (Dee $ S "z") $ x :* Var (Dee $ S "z")

sub :: V -> Expr -> Expr -> Expr
sub v x y = case y of
  Var w | v == w -> x
  Lam w b | v /= w -> Lam w $ rec b
  a :+ b -> rec a :+ rec b
  a :* b -> rec a :* rec b
  a :^ b -> rec a :^ rec b
  a :@ b -> rec a :@ rec b
  _ -> y
  where rec = sub v x
\end{code}

Our variables differ from those of standard lambda calculus. In our world, a
variable can be a run-of-the-mill variable like `x`, but it can also be a
differential of a variable, such as `d x`. We may take differentials
recursively, so if `x` is a variable, then so is `d x, d d x, d d d x, ...`
(perhaps we should call these Peano variables).
Instead of $dddx$, for example, mathematicians customarily write $d^3 x$.

We define functions to pretty-print our expressions, or at least make them less
ugly than the default.

\begin{code}
instance Show V where
  show (S s) = s
  show (Dee v) = "d " ++ show v

instance Show Expr where
  show expr = case expr of
    Con c -> show c
    Var v -> show v
    x :+ y -> concat ["(", show x, " + ", show y, ")"]
    x :* y -> concat ["(", show x, "*", show y, ")"]
    x :^ y -> concat ["(", show x, "^", show y, ")"]
    x :@ y -> concat ["(", show x, " ", show y, ")"]
    Inv -> "(1/)"
    Sin -> "sin"
    Cos -> "cos"
    Exp -> "exp"
    Log -> "log"
    Lam v y -> concat ["\\", show v, " -> ", show y]
\end{code}

We supply a parser so it's less painful to play with our functions.

\begin{code}
chainl1 p op = foldl (\x (f, y) -> f x y) <$> p <*> (many $ (,) <$> op <*> p)
chainr1 p op = go id where
  go d = do
    x <- p
    (op >>= \f -> go (d . (f x:))) <|> pure (foldr ($) x $ d [])

line :: Charser Expr
line = expr <* eof where
  expr = pwr `chainl1` ((spch '+' *> pure (:+))
    <|> (spch '-' *> pure (\x y -> x :+ (Con (-1) :* y))))
  pwr = term `chainr1` (spch '^' *> pure (:^))
  term = apps  `chainl1` ((spch '*' >> pure (:*))
    <|> (spch '/' *> pure (\x y -> x :* (Inv :@ y))))
  apps = dOrApply id <$> some atm
  dOrApply acc [Just one]     = acc one
  dOrApply acc (Nothing:rest) = acc (d $ dOrApply id rest)
  dOrApply acc (Just f:rest)  = dOrApply (acc . (f :@)) rest

  atm = Just <$> (lam <|> num <|> between (spch '(') (spch ')') expr)
    <|> dWord <$> some letterChar <* space

  dWord s = if s == "d" then Nothing else Just $ word s
  word "sin" = Sin
  word "cos" = Cos
  word "exp" = Exp
  word "log" = Log
  word s     = Var $ S s
  lam = spch '\\' *> do
    Var v <- apps
    string "->" *> space *> (Lam v <$> expr)
  num  = Con . fromDecimal <$> (some digitChar <* space)
  spch :: Char -> Charser Char
  spch c = char c <* space
  fromDecimal = foldl (\n d -> 10*n + fromEnum d - fromEnum '0') 0
\end{code}

Our user interface is peculiar. Function application is left-associative (to
match the conventions of lambda calculus and combinatory logic), while `d` is
right-associative (so that `d d d x` means $dddx = d^3 x$).
Unlike Haskell, lambdas bind exactly one variable, so that we can more easily
parse `\d x` as the lambda binding the differentiable variable $dx$. There is
no unary minus, so we write negative integers as, for example, `0 - 42`.

Lastly, we add some code for the interactive demo at the top of this page:

\begin{code}
go :: String -> String
go s = case parse line "" s of
  Left err -> "parse error: " ++ show err
  Right expr -> show expr

main = interact go
\end{code}

== A second opinion ==

We first test our code by computing the second derivative of some variable $y$
with respect to a variable $x$:

------------------------------------------------------------------------
go "d (d y / d x) / d x"
------------------------------------------------------------------------

We get:

------------------------------------------------------------------------
(((d y*((-1*((1/) (d x*d x)))*d d x)) + (d d y*((1/) d x)))*((1/) d x))
------------------------------------------------------------------------

Our program isn't quite ready to take over our calculus homework because it
fails to collect like terms and fold constants and such. However, we can
manually simplify to get:

\[
\frac{d(\frac{dy}{dx})}{dx} = \frac{ddy}{dx^2} - \frac{dy}{dx} \frac{ddx}{dx^2}
\]

This is the correct way to write the second derivative.

The incorrect but widespread $d^2 y / dx^2$ notation for the second derivative
with respect to $x$ is another pernicious consequence of failing to properly
appreciate Leibniz notation.

Some might complain that the truth is too verbose. This is no justification for
propagating falsehoods.
If brevity is paramount, we can always write the unexpanded $d(dy/dx)/dx$
or use Arbogast's notation $D^2_x y$.

It makes no sense to half-heartedly write pseudo-Leibniz terms. It is useless
as a mnemonic because it is wrong, and because it is wrong it reinforces the
myth that differentials cannot be algebraically reasoned with.

== To a certain degree ==

Our code shows that:

\[
d ((p * 100 + q - 212)^2)
\]

evaluates to (after simplification):

\[
\begin{align}
& & 200 * p * (p * 100 + q - 212) * dp \\
&+& 2 * p * (p * 100 + q - 212) * dq
\end{align}
\]

confirming our claims about the coefficients of `dp` and `dq`.

Our code lacks support for symbolic constants, but it's clear how 100 and 212
can be replaced by `x` and `y`. Alternatively, we can compute
`d ((p * x + q - y)^2)` and ignore the `dx` and `dy` terms.

== Think differential ==

In the dark ages, generalizations of our self-taught temperature conversion
program were called 'neural networks'. More recently, we've been saying 'deep
learning'. Even more recently,
https://techburst.io/deep-learning-est-mort-vive-differentiable-programming-5060d3c55074[this
area seems to have been rebranded as 'differentiable programming'].

This reflects a shift in thinking. Although the underlying mathematics remains
the same, the emphasis is no longer on crudely modeling the human brain, nor
stitching together layers of matrix multiplications. The latest fashion is to
program with differentiable functions and improve them with gradient descent,
or perhaps other methods.
See Conal Elliot, https://github.com/conal/talk-2018-deep-learning-rebooted['A
Functional Reboot for Deep Learning'].

In a 2019 paper,
http://online.watsci.org/abstract_pdf/2019v26/v26n3a-pdf/4.pdf[Bartlett and
Khurshudyan] overturn centuries of abuse of Leibniz notation, and bring
differentials back to the realm of algebra.
They show how the correct notation for the second derivative fits perfectly
with the chain rule for the second derivative.

Our live demo shows off the second derivative.
Other examples were taken from Wikipedia's entries on
https://en.wikipedia.org/wiki/Implicit_curve[implicit curves] and
https://en.wikipedia.org/wiki/Quadrifolium[the quadrifolium].

See also https://arxiv.org/pdf/1811.03459.pdf[another paper by Bartlett],
which discusses partial derivatives, and gives a shoutout to the rule:

\[
d(u^v) = v u^{v-1} du + \log(u) u^v dv
\]

which our code supports.

I suspect
http://bcl.hamilton.ie/~barak/papers/ifl2005.ps.gz["perturbation confusion"]
is another casualty of the undeserved status of derivatives over differentials.
An example in the paper goes straight for the derivative, asking
for $\frac{d}{dx} (x + y)$. Similarly, they define a "derivative-taking operator"
that we'll write as `D`.

If we think with differentials instead, we have $d(x + y) = dx + dy$, and
dividing by $dx$ gives $1 + dy/dx$, which doesn't seem to be what they want.
I believe they really meant $\partial_x (x + y) = 1$. Similarly, I believe
they really meant `D` to be a "partial-derivative-taking operator", that is
`D (\x -> e)` means `\x -> partial x (d e)`, where `partial x` sets all
differential variables to zero except for `d x`. With these
definitions, there are no surprises; the example evaluates to what we expect.

https://www.microsoft.com/en-us/research/uploads/prod/2019/07/autodiff-icfp-2019.pdf[A more modern paper] agrees with my interpretation (see Section 4.3).

== Two weird tricks ==

It may seem we could beef up our code to get something like TensorFlow, that
is, a system that automatically performs gradient descent on a given
differentiable function containing unknown parameters.
However, our approach for computing differentials turns out to scale poorly.
Better is
https://en.wikipedia.org/wiki/Automatic_differentiation[automatic
differentiation], a vague-sounding term that encompasses two tricks:

  1. Memoization. For every sub-expression $f$, we keep around $(f, df)$, so we
  avoid recomputing the same sub-expressions over and over again.

  2. Using values instead of symbols. Rather than compute a formula for a
  differential that we later apply to particular values, we just compute with
  values all the time.

An example of the first trick: suppose we wish to compute $d(f g h)$.
The naive method requires us to compute:

\[
(df) g h + f (dg) h + f g (dh)
\]

With memoization, we first compute and remember:

\[
(g h, d(g h)) = (g h, g (dh) + (dg) h)
\]

which we later use to compute:

\[
d(f g h) = (df) g h + f d(g h)
\]

The more functions in our product, the more memoization saves.

An example of the second trick: if we know $(f, df) = (1, 2 dx)$ and $(g, dg) =
(3, 4 dx)$ then the product rule gives $(f g, d(f g)) = (3, 10 dx)$. Algebra
is great, but sticking with values sure is easier than manipulating symbols.

Automatic differentiation has a 'forward mode' and a 'reverse mode'.
The latter is also called 'backpropagation' in some contexts.
These modes relate to the chain rule, which in our implementation is hidden in
lambdas and applications. Working through the details, we find the chain rule
leads to expressions like:

\[
3 \times 4 \times 5 \times (dp + 2 dq + 4 dr)
\]

My understanding is that in forward mode, the multiplications associate to
the right:

\[
\begin{align}
& & 3 \times 4 \times 5 \times (dp + 2 dq + 4 dr) \\
&=& 3 \times 4 \times (5 dp + 10 dq + 20 dr) \\
&=& 3 \times (20 dp + 40 dq + 80 dr) \\
&=& 60 dp + 120 dq + 240 dr
\end{align}
\]

while in reverse mode the multiplications associate to the left, which is more
efficient:

\[
\begin{align}
& & 3 \times 4 \times 5 \times (dp + 2 dq + 4 dr) \\
&=& 12 \times 5 \times (dp + 2 dq + 4 dr) \\
&=& 60 \times (dp + 2 dq + 4 dr) \\
&=& 60 dp + 120 dq + 240 dr
\end{align}
\]

We can view the right-most factor as a list `[1,2,4]`, in which case reverse
mode is the result of applying a fusion law to forward mode:

------------------------------------------------------------------------
map (3*) . map (4*) . map (5*) = map (60*)
------------------------------------------------------------------------

By the way, the second trick shows up elsewhere. In certain areas of
cryptography, we need to compute a function related to
https://en.wikipedia.org/wiki/Weil_pairing[the Weil pairing]. The numbers
involved are so large that it's infeasible to write the function in terms of
two input points given symbolically, say $(x_1, y_1)$ and $(x_2, y_2)$.

Thus in practice, to compute this function on two given points, we roughly act
as if were trying to derive a formula, but always use values instead of
symbols. This is known as https://crypto.stanford.edu/miller/[Miller's
algorithm].

Automated theorem proving profits from turning this trick on its head. Early
theorem provers exhaustively tried every possible value for every variable.
Later provers improved on this by by computing with variables instead.

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script>
function setup(name, t) {
  function act() {
    document.getElementById("in").value = t;
    document.getElementById("out").value = "";
  }
  document.getElementById(name).addEventListener("click", act);
  if (name == "example") act();
}
setup("example", "d((\\z -> z*z)(p*100 + q - 212))");
setup("implicit", "d(\\x -> \\y -> sin(x + y) - cos(x*y) + 1)");
setup("lambda", "d ((\\z -> z^3) (x*x + y^2) - (\\z -> z*z) (x^2 - y*y))");
setup("second", "d (d y / d x) / d x");

const ctx = {};
function run() {
  ctx.inp = (new TextEncoder()).encode(document.getElementById("in").value);
  ctx.out = [], ctx.cursor = 0;
  ctx.instance.exports.main();
  document.getElementById("out").value = (new TextDecoder()).decode(Uint8Array.from(ctx.out));
}
async function loadWasm() {
  try {
    ctx.instance = (await WebAssembly.instantiateStreaming(fetch('differ.wasm'), {env:
      { putchar: c  => ctx.out.push(c)
      , eof    : () => ctx.cursor == ctx.inp.length
      , getchar: () => ctx.inp[ctx.cursor++]
      }})).instance;

    document.getElementById("go").addEventListener("click", (event) => run());
  } catch(err) {
    console.log(err);
  }
}
loadWasm();
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
