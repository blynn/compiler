= Compiler Quest =

Bootstrapping a compiler is like a role-playing game. From humble beginnings,
we painstakingly cobble together a primitive, hard-to-use compiler whose only
redeeming quality is that it can build itself.

Because the language supported by our compiler is so horrible, we can only
bear to write a few incremental improvements. But once we persevere, we
compile the marginally better compiler to gain a level.

Then we iterate. We add more features to our new compiler, which is easier
thanks to our most recent changes, then compile again, and so on.

== Parenthetically ==

Our first compiler converts parenthesized combinatory logic terms to ION
assembly. For example:

------------------------------------------------------------------------
parse "BS(BB);Y(B(CS)(B(B(C(BB:)))C));"
  ==  "``BS`BB;`Y``B`CS``B`B`C``BB:C;"
------------------------------------------------------------------------

We assume the input is valid and contains no numbers in parentheses or
square brackets (so it only uses `@` to refer to previous terms and `#` for
integer constants).

In Haskell we could write:

------------------------------------------------------------------------
term kon acc s = case s of
  h:t
    | h == ')' || h == ';' -> kon acc t
    | h == '('             -> term comp Nothing t
    | otherwise -> (if h == '#' || h == '@' then esc else id) glom (h:) t
  where
  glom f t = term kon (Just $ maybe id (('`':) .) acc . f) t
  comp m t = maybe undefined (flip glom t) m

esc g f t = case t of th:tt -> g (f . (th:)) tt

parse "" = ""
parse s = term (\p t -> maybe id id p $ ';':parse t) Nothing s
------------------------------------------------------------------------

We employ continuation-passing style. Typically, a function might return a pair
consisting of the tree parsed so far and the remainder of the string to parse,
and another function scrutinizes the pair and processes its contents. We fuse
these steps, obtaining shorter code, though it takes some getting used to.

To translate to combinatory logic, we break our code into more digestible
pieces. We add some glue so our code works in GHC yet still resembles raw
combinators.

------------------------------------------------------------------------
eq a b x y = if a == b then x else y
fix f = f (fix f)
cons = (:)
b = (.)
lst t x y = case t of {[] -> x; h:t -> y h t}
must f s = case s of {[] -> undefined; h:t -> f h t}

orChurch f g x y = f x (g x y)
isPre h = orChurch (eq '#' h) (eq '@' h)
closes h = orChurch (eq ';' h) (eq ')' h)
glom tk acc f t = tk (Just (b (maybe id (b (cons '`')) acc) f)) t
esc g f t = must (\th tt -> g (b f (cons th)) tt) t
atom h gl t = isPre h esc id gl (cons h) t
comp gl a t = maybe undefined (flip gl t) a
sub r gl t = r (comp gl) Nothing t
more r h gl t = eq '(' h (sub r) (atom h) gl t
switch r kon h a t = closes h kon (b (more r h) (glom (r kon))) a t
term = fix (\r kon acc s -> must (\h t -> switch r kon h acc t) s)
parseNonEmpty r s = term (\p t -> maybe id id p (cons ';' (r t))) Nothing s
parse = fix (\r s -> lst s "" (\h t -> parseNonEmpty r s))
------------------------------------------------------------------------

We implement Church booleans. For lists, deleting our `lst` helper makes the
code appear to use Scott-encoded lists. We could have acted similarly for
`Maybe`, but we save a few bytes with `must` and `maybe`: since we may
substitute anything we like for `undefined`, each of `maybe undefined`, `maybe
id`, and `must` become `C(TI)`.

https://crypto.stanford.edu/~blynn/lambda/kiselyov.html[Bracket abstraction]
yields:

----------------------------------------------------------------
must = C(TI)
orChurch = B S(B B)
isPre = S(B orChurch(eq ##))(eq #@)
closes = S(B orChurch(eq #;))(eq #))
glom = R(B(B Just)(B b(must(b(cons #`)))))(B B B)
esc = B(B must)(R(R cons(B B b))(B B B))
atom = S(B C(R id(R esc isPre))) cons
comp = B C(B(B must) flip)
sub = B(R Nothing)(R comp B)
more = R atom(B S(B(C(eq #()) sub))
switch = B(S(B S(C closes)))(S(B B(B C(B(B b) more)))(B glom))
term = Y(B(B(B must))(B(B C) switch))
parseNonEmpty = R Nothing(B term(B(C(B B(must id)))(B(cons #;))))
parse = Y(B(S(T K))(B(B K)(B(B K) parseNonEmpty)))
----------------------------------------------------------------

We order parameters to reduce term sizes (via
https://crypto.stanford.edu/~blynn/lambda/bohm.html[eta-equivalence]).
Then we:

  * Rewrite with prefix notation for application
  * Replace `id const flip cons eq Nothing` with combinators `I K C : =`.
  * Scott-encode `Nothing` and `Just` as `K` and `BKT`.
  * Refer to the 'n'th function with `@` followed by the character with ASCII
code 31 + 'n'.

------------------------------------------------------------------------
`C`TI;
``BS`BB;
``S``B@!`=##`=#@;
``S``B@!`=#;`=#);
``R``B`B``BKT``BB`@ `B`:#```BBB;
``B`B@ ``R``R:``BBB``BBB;
``S``BC``RI``R@%@":;
``BC``B`B@ C;
``B`RK``R@'B;
``R@&``BS``B`C`=#(@(;
``B`S``BS`C@#``S``BB``BC``B`BB@)`B@$;
`Y``B`B`B@ ``B`BC@*;
``RK``B@+``B`C``BB`@ I`B`:#;;
`Y``B`S`TK``B`BK``B`BK@,;
------------------------------------------------------------------------

We added a newline after each semicolon for clarity; these must
be removed before feeding.

== Exponentially ==

Our next compiler rewrites lambda expressions as combinators using the
straightforward bracket abstraction algorithm we described earlier.

The following are friendlier names for the combinators I, K, T, C, Y, and K,
respectively.

------------------------------------------------------------------------
id x = x;
const x _ = x;
(&) x f = f x;
flip f x y = f y x;
fix x = x (fix x);
Nothing x _ = x;
------------------------------------------------------------------------

Our program starts with a few classic definitions. `P` stands for "pair".

------------------------------------------------------------------------
Just x f g = g x;
P x y f = f x y;
(||) f g x y = f x (g x y);
(++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
------------------------------------------------------------------------

As combinators, we have:

------------------------------------------------------------------------
BKT;
BCT;
BS(BB);
Y(B(CS)(B(B(C(BB:)))C));
------------------------------------------------------------------------

Parsing is based on functions of type:

------------------------------------------------------------------------
type Parser x = [Char] -> Maybe (x, [Char])
------------------------------------------------------------------------

A `Parser x` tries to parse the beginning of a given string for a value of
type `x`. If successful, it returns `Just` the value along with the unparsed
remainder of the input string. Otherwise it returns `Nothing`.

Values of type `Parser x` compose in natural ways. See
http://www.cs.uu.nl/research/techreps/repo/CS-2008/2008-044.pdf[Swierstra,
_Combinator Parsing: A Short Tutorial_].

------------------------------------------------------------------------
pure x inp = Just (P x inp);
bind f m = m Nothing (\x -> x f);
(<*>) x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
(<$>) f x = pure f <*> x;
(*>) p q = (\_ x -> x) <$> p <*> q;
(<*) p q = (\x _ -> x) <$> p <*> q;
(<|>) x y = \inp -> (x inp) (y inp) Just;
------------------------------------------------------------------------

These turn into:

------------------------------------------------------------------------
B(B@ )@!;
B(C(TK))T;
C(BB(B@%(C(BB(B@%(B@$))))));
B@&@$;
B@&(@'(KI));
B@&(@'K);
B(B(R@ ))S;
------------------------------------------------------------------------

Our syntax tree goes into the following data type:

------------------------------------------------------------------------
data Ast = R [Char] | V Char | A Ast Ast | L Char Ast
------------------------------------------------------------------------

The `R` stands for "raw", and its field passes through unchanged during
bracket abstraction. Otherwise we have a lambda calculus that supports
one-character variable names. Scott-encoding yields:

------------------------------------------------------------------------
R s   = \a b c d -> a s;
V v   = \a b c d -> b v;
A x y = \a b c d -> c x y;
L x y = \a b c d -> d x y;
------------------------------------------------------------------------

which translates to:

------------------------------------------------------------------------
B(BK)(B(BK)(B(BK)T));
BK(B(BK)(B(BK)T));
B(BK)(B(BK)(B(B(BK))(BCT)));
B(BK)(B(BK)(B(BK)(BCT)));
------------------------------------------------------------------------

The `sat` parser combinator parses a single character that satisfies a given
predicate. The `char` specializes this to parse a given character, and
`var` accepts any character except the semicolon and closing parenthesis.
We use `sat (const const)` to accept any character, and with that, we can
parse a lambda calculus expression such as `\x.(\y.Bx)`:

------------------------------------------------------------------------
sat f inp = inp Nothing (\h t -> f h (pure h t) Nothing);
char c = sat (\x -> x((==)c));
var = sat (\c -> flip (c((==)';') || c((==)')')));
pre = (:) <$> (char '#' <|> char '@') <*> (flip (:) const <$> sat (const const));
atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (R <$> pre) <|> (V <$> var);
apps r = (((&) <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure id;
expr = ((&) <$> atom expr) <*> apps expr;
------------------------------------------------------------------------

As combinators, we have:

------------------------------------------------------------------------
B(C(TK))(B(B(RK))(C(BS(BB))@$));
B@/(BT=);
@/(BC(S(B@"(=#;))(=#))));
@&(@':(@*(@0##)(@0#@)))(@'(C:K)(@/(KK)));
C(B@*(C(B@*(S(B@*(B(@((@0#())(C@)(@0#)))))(B(@&(@((@0#\)(@'@.@1)))(@((@0#.)))))(@'@+@2)))(@'@,@1);
Y(B(R(@$I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@-)))))));
Y(S(B@&(B(@'T)@3))@4);
------------------------------------------------------------------------

The `babs` and `unlam` functions perform simple bracket abstraction, and
`show` writes the resulting lambda-free `Ast` in ION assembly.

------------------------------------------------------------------------
show t = t id (\v -> v:[])(\x y -> '`':(show x ++ show y)) undefined;
unlam v = fix (\r t -> t (\x -> A (V 'K') (R x)) (\x -> x((==)v) (V 'I') (A (V 'K') (V x))) (\x y -> A (A (V 'S') (r x)) (r y)) undefined);
babs t = t R V (\x y -> A (babs x) (babs y)) (\x y -> unlam x (babs y));
------------------------------------------------------------------------

Putting it all together, `main` parses as many semicolon-terminated
expressions as it can and converts them to ION assembly.

------------------------------------------------------------------------
main s = (expr <* char ';') s "" (\p -> p (\x t -> show (babs x) ++ ";" ++ main t)));
------------------------------------------------------------------------

These last few functions are:

------------------------------------------------------------------------
Y(B(R?)(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@#)))I))));
BY(B(B(R?))(C(BB(BC(B(C(T(B(@-(@,#K))@+)))(C(BS(B(R(@,#I))(BT=)))(B(@-(@,#K))@,)))))(S(BC(B(BB)(B(B@-)(B(@-(@,#S))))))I)));
Y(S(BC(B(C(C(T@+)@,))(S(BC(B(BB)(B@-)))I)))(C(BB@7)));
Y(B(C(C(@)@5(@0#;))K))(BT(C(BB(B@#(C(B@#(B@6@8))(:#;K)))))));
------------------------------------------------------------------------

We feed the combinators into our first compiler to produce an ION assembly
program that compiles a primitive lambda calculus to ION assembly.

I should confess that the above combinator terms were produced by a program I
wrote, and I have only checked a few of them. Hopefully, it's plausible all
can be verified by hand.

== Practically ==

We've reached a milestone. Thanks to our previous compiler, never again do
we convert LC to CL by hand.

But there's a catch. For each variable we abstract over, our bracket
abstraction algorithm adds an application of the S combinator to every
application. Hence for N variables, this multiplies the number of applications
by 2^N^, which is intolerable for all but the smallest programs.

Thus our first priority is to optimize bracket abstraction. We stop
recursively adding S combinators as soon as we realize they are unnecessary by
modifying `unlam` and adding a helper function `occurs`:

------------------------------------------------------------------------
occurs v t = t (\x -> False) (\x -> x(v(==))) (\x y -> occurs v x || occurs v y) undefined;
unlam v t = occurs v t
  (t undefined (const (V 'I')) (\x y -> A (A (V 'S') (unlam v x)) (unlam v y)) undefined)
  (A (V 'K') t);
------------------------------------------------------------------------

(I'm unsure if it's worth explaining the optimization in words as I only
understood after fiddling around with combinators. Here goes anyway: I think of
S as unconditionally passing a variable to both children of an application
node. With a little analysis, we can tell when a child has no need for the
variable.)

We rewrite these with one-character variable names and Y combinators, and
use `@`-indexing to refer to other definitions:

------------------------------------------------------------------------
Y\a.\b.\c.c(\d.KI)(\d.d(b=))(\d.\e.@"(abd)(abe))?;
Y\a.\b.\c.@7bc(c?(K(@,#I))(\d.\e.@-(@-(@,#S)(abd))(abe))?)(@-(@,#K)c);
------------------------------------------------------------------------

Our previous compiler turns these into massive but manageable combinatory logic
terms.

== Summary ==

Our three compilers are the following:

------------------------------------------------------------------------
`C`TI;``BS`BB;``S``B@!`=##`=#@;``S``B@!`=#;`=#);``R``B`B``BKT``BB`@ `B`:#```BBB;``B`B@ ``R``R:``BBB``BBB;``S``BC``RI``R@%@":;``BC``B`B@ C;``B`RK``R@'B;``R@&``BS``B`C`=#(@(;``B`S``BS`C@#``S``BB``BC``B`BB@)`B@$;`Y``B`B`B@ ``B`BC@*;``RK``B@+``B`C``BB`@ I`B`:#;;`Y``B`S`TK``B`BK``B`BK@,;

BKT;BCT;BS(BB);Y(B(CS)(B(B(C(BB:)))C));B(B@ )@!;B(C(TK))T;C(BB(B@%(C(BB(B@%(B@$))))));B@&@$;B@&(@'(KI));B@&(@'K);B(B(R@ ))S;B(BK)(B(BK)(B(BK)T));BK(B(BK)(B(BK)T));B(BK)(B(BK)(B(B(BK))(BCT)));B(BK)(B(BK)(B(BK)(BCT)));B(C(TK))(B(B(RK))(C(BS(BB))@$));B@/(BT=);@/(BC(S(B@"(=#;))(=#))));@&(@':(@*(@0##)(@0#@)))(@'(C:K)(@/(KK)));C(B@*(C(B@*(S(B@*(B(@((@0#())(C@)(@0#)))))(B(@&(@((@0#\)(@'@.@1)))(@((@0#.)))))(@'@+@2)))(@'@,@1);Y(B(R(@$I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@-)))))));Y(S(B@&(B(@'T)@3))@4);Y(B(R?)(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@#)))I))));BY(B(B(R?))(C(BB(BC(B(C(T(B(@-(@,#K))@+)))(C(BS(B(R(@,#I))(BT=)))(B(@-(@,#K))@,)))))(S(BC(B(BB)(B(B@-)(B(@-(@,#S))))))I)));Y(S(BC(B(C(C(T@+)@,))(S(BC(B(BB)(B@-)))I)))(C(BB@7)));Y(B(C(C(@)@5(@0#;))K))(BT(C(BB(B@#(C(B@#(B@6@8))(:#;K)))))));

BKT;BCT;BS(BB);Y(B(CS)(B(B(C(BB:)))C));B(B@ )@!;B(C(TK))T;C(BB(B@%(C(BB(B@%(B@$))))));B@&@$;B@&(@'(KI));B@&(@'K);B(B(R@ ))S;B(BK)(B(BK)(B(BK)T));BK(B(BK)(B(BK)T));B(BK)(B(BK)(B(B(BK))(BCT)));B(BK)(B(BK)(B(BK)(BCT)));B(C(TK))(B(B(RK))(C(BS(BB))@$));B@/(BT=);@/(BC(S(B@"(=#;))(=#))));@&(@':(@*(@0##)(@0#@)))(@'(C:K)(@/(KK)));C(B@*(C(B@*(S(B@*(B(@((@0#())(C@)(@0#)))))(B(@&(@((@0#\)(@'@.@1)))(@((@0#.)))))(@'@+@2)))(@'@,@1);Y(B(R(@$I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@-)))))));Y(S(B@&(B(@'T)@3))@4);Y(B(R?)(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@#)))I))));Y\a.\b.\c.c(\d.KI)(\d.d(b=))(\d.\e.@"(abd)(abe))?;Y\a.\b.\c.@7bc(c?(K(@,#I))(\d.\e.@-(@-(@,#S)(abd))(abe))?)(@-(@,#K)c);Y(S(BC(B(C(C(T@+)@,))(S(BC(B(BB)(B@-)))I)))(C(BB@8)));Y(B(C(C(@)@5(@0#;))K))(BT(C(BB(B@#(C(B@#(B@6@9))(:#;K)))))));
------------------------------------------------------------------------

== Term rewriting; bootstrapping ==

https://arxiv.org/pdf/1812.02243.pdf[Corrado Bőhm's PhD thesis (1951)]
discusses self-compilation and presents a high-level language that can compile
itself.

https://tromp.github.io/cl/LC.pdf[John Tromp lists rewrite rules] to further
reduce the size of the output combinatory logic term after bracket abstraction.

Chapter 16 of
https://www.microsoft.com/en-us/research/publication/the-implementation-of-functional-programming-languages/[_The
Implementation of Functional Programming Languages_ by Simon Peyton Jones]
gives a comprehensive overview of this strategy.

http://www.cantab.net/users/antoni.diller/brackets/intro.html[Antoni Diller has interactive demos and more references].

https://en.wikipedia.org/wiki/Chicken_or_the_egg[Ancient philosophers thought
about bootstrapping compilers] but their progress was paltry. Or poultry, one
might say.

https://www.joachim-breitner.de/blog/748-Thoughts_on_bootstrapping_GHC[Joachim
Breitner frets about the difficulty of bootstrapping GHC].

https://cakeml.org/[CakeML] is a formally verified bootstrappable ML compiler.
