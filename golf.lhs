= Combinator Golf =

Gregory Chaitin's pioneering results on
https://en.wikipedia.org/wiki/Algorithmic_information_theory[algorithmic
information theory] are exciting because he wrote real programs to make theory
concrete. Sadly, Chaitin chose LISP due to a poor understanding of lambda
calculus, making his results less impressive than they ought to be.

https://tromp.github.io/cl/LC.pdf[John Tromp's reworking of Chaitin's ideas in
lambda calculus and combinatory logic] is a fascinating read. Much of the fun
involves tiny self-interpreters that read binary encodings of themselves.
Tromp attains impressively compact self-interpreters.

Tromp's binary self-interpreter expects the input in the form of nested pairs
such as:

------------------------------------------------------------------------------
(True, (False, (False, ...  (True, const) ... ))
------------------------------------------------------------------------------

and if we could turn off type-checking in Haskell, the interpreter is simply:

------------------------------------------------------------------------------
eval c = uncurry (bool (uncurry (c . bool const ap)) (eval (eval . (c .))))
------------------------------------------------------------------------------

and satisfies:

------------------------------------------------------------------------------
eval c (encode m n) == c m n
------------------------------------------------------------------------------

where:

------------------------------------------------------------------------------
data CL = App CL CL | S | K
encode m n = case m of
  x :@ y -> (True, (encode x (encode y n)))
  S -> (False, (False, n))
  K -> (False, (True , n))
------------------------------------------------------------------------------

The above fails to type-check because Haskell has iso-recursive types.

However, some of our compilers are completely ignorant of types, so will
happily run our crazy code. Try the following with link:grind.html[our "Fixity"
compiler]:

------------------------------------------------------------------------------
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
------------------------------------------------------------------------------

Since SKK is the identity, the above just passes the input through with no
changes.

Kiselyov's bracket abstraction algorithm leads us to wonder: why limit
ourselves to S and K? Just because we can doesn't mean we should. After all, we
could prohibiting lambda terms needing a De Bruijn index greater than 2, but we
don't.

== Hole-in-one ==

With no restrictions, adding combinators to the definition of CL to obtain a
smaller self-interpreter is too easy. In the extreme, we could take a "trusting
trust" approach and define a combinator X which decodes a given binary string
to a CL term then interprets it.

The set of all combinators in this case is X, S, and K, and the representation
of X would be 1 bit long, trivially resulting in a 1-bit self-interpreter.

== Rule-of-three ==

Perhaps it's reasonable to say a combinator is eligible if it is equivalent to
a closed lambda term with at most 3 lambda abstractions and at most 3
applications. That is, every combinator is at most as complex as the S
combinator.

We choose the following 6 combinators:

------------------------------------------------------------------------------
Sxyz = xz(yz)
Bxyz = x (yz)
Cxyz = xz(y )
Kxy  = x
Txy  = yx
Vxyz = zxy
------------------------------------------------------------------------------

We encode them in binary according to the following table (where the backquote
represents application):

------------------------------------------------------------------------------
` 1
B 01
V 0011
T 0010
S 0001
C 00001
K 00000
------------------------------------------------------------------------------

Then the following is a self-interpreter:

------------------------------------------------------------------------------
f c = T(V(T(V(T(T.V(V(T(c.V K C))(c S))(c.V T V)))(c B)))(f(f.(c.))))
------------------------------------------------------------------------------

By Kiselyov's algorithm, this is:

------------------------------------------------------------------------------
`Y``B`BT``B`S``BV``BT``S``BV``BT``B`BT``S``BV``S``BV``BT``CB``VKC`TS``CB``VTV`TB``SB``C``BBBB
------------------------------------------------------------------------------

We can define the Y combinator by:

------------------------------------------------------------------------------
Y = ``B``TT``CB``STT
------------------------------------------------------------------------------

which means our self-interpreter takes 232 bits, beating Tromp's Theorem
4 result of 263 bits.

The following demonstrates this self-interpreter in our "Fixity" compiler,
interpreting `snd ("fst", "snd") = TK(SKK) ("fst", "snd")`.

------------------------------------------------------------------------------
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
demo _ = eval id (encode (App T (App K (App (App S K) K))) ("fst", "snd"));
------------------------------------------------------------------------------

Is it sporting to consider the Y combinator primitive? If so, we could likely
shrink the interpreter further.
