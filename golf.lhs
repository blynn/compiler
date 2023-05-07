= Combinator Golf =

Gregory Chaitin's pioneering results on
https://en.wikipedia.org/wiki/Algorithmic_information_theory[algorithmic
information theory] are exciting because he wrote real programs to make theory
concrete. Sadly, Chaitin chose LISP due to a flawed understanding of lambda
calculus, diminishing the beauty of his results.

He's not alone.
https://queue.acm.org/detail.cfm?id=1039523[Alan Kay described LISP as the
"Maxwell's equations of software"] because a "half page of code" described LISP
itself. Surely the equally powerful lambda calculus, which can describe itself
in far less space, is more deserving of the title.
http://www.paulgraham.com/rootsoflisp.html[Paul Graham's _The Roots of Lisp_]
dubs the LISP self-interpreter "The Surprise" because of its supposed brevity.
What, then, should one call a one-line lambda calculus self-interpereter?

(Aside: on the other hand, the analogy with Maxwell's equations is apt, because
if Maxwell had used link:../haskell/ga.html[geometric algebra] instead of
vector algebra, he could have been less verbose and we would be celebrating
Maxwell's _equation_, that is, one equation instead of four:

\[
(\partial_t + \nabla)(\mathbf{e} + \mathbf{B}) = 0
\]

in a vacuum, and more generally:

\[
(\partial_t + \nabla)(\mathbf{e} + \mathbf{B}) = \rho + \mathbf{j}
\]

where bivectors represent the magnetic field $\mathbf{B}$.)

https://tromp.github.io/cl/LC.pdf[John Tromp's reworking of Chaitin's ideas in
lambda calculus and combinatory logic] is a fascinating read. Much of the fun
involves tiny self-interpreters that read binary encodings of themselves.

Tromp's binary CL self-interpreter expects the input in the form of nested
pairs such as:

------------------------------------------------------------------------
(True, (False, (False, ...  (True, const) ... ))
------------------------------------------------------------------------

where the booleans and pairs have their usual Church/Scott encodings.

If we could disable type-checking in Haskell, the interpreter is simply:

------------------------------------------------------------------------
eval c = uncurry (bool (uncurry (c . bool const ap)) (eval (eval . (c .))))
------------------------------------------------------------------------

and satisfies:

------------------------------------------------------------------------
eval c (encode m n) == c m n
------------------------------------------------------------------------

where:

------------------------------------------------------------------------
data CL = App CL CL | S | K
encode m n = case m of
  x :@ y -> (True, (encode x (encode y n)))
  S -> (False, (False, n))
  K -> (False, (True , n))
------------------------------------------------------------------------

Some of our type-challenged compilers will happily run this crazy code.
link:grind.html[Our "Fixity" compiler] accepts the following:

------------------------------------------------------------------------
data Bool = True | False;
id x = x;
const x y = x;
ap x y z = x z(y z);
bool x y b = case b of { True -> y ; False -> x };
uncurry f x = case x of { (,) a b -> f a b };
(.) f g x  = f (g ( x));
encode m n = case m of
  { S -> (False, (False, n))
  ; K -> (False, (True , n))
  ; App x y -> (True, (encode x (encode y n)))
  };
eval c = uncurry (bool (uncurry (c . bool ap const)) (eval (eval . (c .))));
go s = eval id (encode (App (App S K) K) s);
------------------------------------------------------------------------

Since SKK is the identity, the above program just returns the input.

Kiselyov's bracket abstraction algorithm leads us to wonder: why limit
ourselves to S and K? After all, in lambda calculus, we could prohibit terms
with a De Bruijn index greater than 2 and retain the same computing power, but
nobody bothers. Why insist on two particular combinators?

== Hole in one ==

With no restrictions, adding combinators to the definition of CL to obtain a
smaller self-interpreter is too easy. In the extreme, we could take a
https://www.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf["trusting
trust"] approach and define a combinator X which decodes a given binary string
to a CL term and interprets it.

The set of all combinators is X, S, and K. Representing X with 1 bit trivially
results in a 1-bit self-interpreter.

== Rule of three ==

Perhaps it's reasonable to say a combinator is eligible if it is equivalent to
a closed lambda term with at most 3 lambda abstractions and at most 3
applications. That is, every combinator is at most as complex as the S
combinator.

We choose the following 6 combinators:

------------------------------------------------------------------------
Sxyz = xz(yz)
Bxyz = x (yz)
Cxyz = xz(y )
Kxy  = x
Txy  = yx
Vxyz = zxy
------------------------------------------------------------------------

We encode them in binary according to the following table (where the backquote
represents application):

------------------------------------------------------------------------
` 1
B 01
V 0011
T 0010
S 0001
C 00001
K 00000
------------------------------------------------------------------------

Then the following is a binary self-interpreter:

------------------------------------------------------------------------
f c = T(V(T(V(T(T.V(V(T(c.V K C))(c S))(c.V T V)))(c B)))(f(f.(c.))))
------------------------------------------------------------------------

Kiselyov's algorithm yields:

------------------------------------------------------------------------
`Y``B`BT``B`S``BV``BT``S``BV``BT``B`BT``S``BV``S``BV``BT``CB``VKC`TS``CB``VTV`TB``SB``C``BBBB
------------------------------------------------------------------------

We define the Y combinator with:

------------------------------------------------------------------------
Y = ``B``STT``CB``STT
------------------------------------------------------------------------

so our self-interpreter takes 236 bits, beating Tromp's Theorem 4 record of 263
bits.

The following demonstrates this self-interpreter in our "Fixity" compiler:

------------------------------------------------------------------------
data Bool = True | False;
id x = x;
const x y = x;
flip x y z = x z y;
ap x y z = x z(y z);
bool x y b = case b of { True -> y ; False -> x };
uncurry f x = case x of { (,) a b -> f a b };
(.) f g x  = f(g(x));
data CL = K | C | S | T | V | B | App CL CL;
encode m n = case m of
  { K -> (False, (False, (False, (False, (False, n)))))
  ; C -> (False, (False, (False, (False, (True , n)))))
  ; S -> (False, (False, (False, (True , n))))
  ; T -> (False, (False, (True , (False, n))))
  ; V -> (False, (False, (True , (True , n))))
  ; B -> (False, (True , n))
  ; App x y -> (True, (encode x (encode y n)))
  };
t = uncurry;
v = bool;
eval c = t(v(t(v(t(t . v(v(t(c . v const flip))(c ap))(c . v t v)))(c(.)))) (eval(eval . (c .))));
demo _ = eval id (encode (App T (App K (App (App S K) K))) ("one", "two"));
------------------------------------------------------------------------

The term `T(K(SKK))` returns the second element of a Scott-encoded pair, so
the above computes `snd ("one", "two")`.

Is it sporting to consider the Y combinator primitive? If so, we could likely
shrink the interpreter further.

== Instant REPL play ==

Interactive REPLs such as GHCi shine when we want to ask the computer for help
while reducing the number of dreaded edit-compile-run cycles.

We still write code in a file, but once done, we play around in the REPL to
print codes and sizes.

\begin{code}
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.Ord
import Data.Tree

coms = "```B``STT``CB``STT``B`BT``B`S``BV``BT``S``BV``BT``B`BT``S``BV``S``BV``BT``CB``VCK`TS``CB``VTV`TB``SB``C``BBBB"

histo = M.fromListWith (+) $ (\c -> ([c], 1)) <$> coms

huff :: Map String Int -> [(String, [Int])]
huff ps = huff' [] $ huffTree $ (\(k, v) -> Node (k, v) []) <$> M.assocs ps
  where
  huff' s (Node (k, _) []) = [(k, s)]
  huff' s (Node _ [x, y]) = huff' (0:s) x ++ huff' (1:s) y
  huffTree [p] = p
  huffTree ps = huffTree $ Node ("", v0 + v1) [p0, p1]:ps2 where
    p0@(Node (_, v0) _) = getMin ps
    ps1 = delete p0 ps
    p1@(Node (_, v1) _) = getMin ps1
    ps2 = delete p1 ps1
    getMin = minimumBy $ comparing $ \(Node (_, v) _) -> v

total = foldr (\(c, enc) n -> n + length enc * histo!c) 0 $ huff histo
\end{code}

== Infinite regress ==

Is mainstream mathematics mistaken in its handling of the infinite?
https://web.math.princeton.edu/\~nelson/papers/warn.pdf[What does it mean,
Edward Nelson asks, to treat the unfinished as finished]?
We might get a theory that is internally consistent, but so what? Good stories
that have no bearing on reality are also internally consistent. Perhaps
http://sites.math.rutgers.edu/~zeilberg/Opinion125.html[undecidability is
meaningless, as Doron Zeilberger spiritedly opines].

If this turns out to be the case, then I'll be annoyed because of the time I
spent learning a lot of this stuff. But no matter what, it'll always be fun to
seek tinier self-interpreters!

Turing machine golf is also possible, though more painful.
https://www.scottaaronson.com/busybeaver.pdf[Yedidia and Aaronson find small
Turing machines] related to Busy Beaver numbers, the Goldbach conjecture, and
the Riemann hypothesis, which were
https://www.scottaaronson.com/blog/?p=2741[later shrunk further].
Porting combinators to Turing machines may compare well with their approach.
