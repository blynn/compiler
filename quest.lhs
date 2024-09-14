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

We assume the input is valid and uses `@` to refer to previous terms and `#`
for integer constants.

In Haskell we could write:

------------------------------------------------------------------------
term kon acc s = case s of
  h:t
    | h == ')' || h == ';' -> kon acc t
    | h == '('             -> term comp Nothing t
    | otherwise -> (if h == '#' || h == '@' then esc else id) app (h:) t
  where
  app f t = term kon (Just $ maybe id (('`':) .) acc . f) t
  comp m t = maybe undefined (flip app t) m

esc g f t = case t of th:tt -> g (f . (th:)) tt

parse "" = ""
parse s = term (\p t -> maybe id id p $ ';':parse t) Nothing s
------------------------------------------------------------------------

We employ continuation-passing style. Typically, a function might return a pair
consisting of the tree parsed so far and the remainder of the string to parse,
and another function scrutinizes the pair and processes its contents. We fuse
these steps, obtaining shorter code.

To translate to combinatory logic, we break our code into more digestible
pieces and try to order arguments to reduce term sizes (via
https://crypto.stanford.edu/~blynn/lambda/bohm.html[eta-equivalence]). We add
some glue so our code works in GHC.

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
app rkon acc f t = rkon (Just (b (maybe id (b (cons '`')) acc) f)) t
esc g f t = must (\th tt -> g (b f (cons th)) tt) t
atom h gl t = isPre h esc id gl (cons h) t
comp gl a t = maybe undefined (flip gl t) a
sub r gl t = r (comp gl) Nothing t
more r h gl t = eq '(' h (sub r) (atom h) gl t
switch r kon h a t = closes h kon (b (more r h) (app (r kon))) a t
term = fix (\r kon acc s -> must (\h t -> switch r kon h acc t) s)
parseNonEmpty r s = term (\p t -> maybe id id p (cons ';' (r t))) Nothing s
parse = fix (\r s -> lst s "" (\h t -> parseNonEmpty r s))
------------------------------------------------------------------------

We implement Church booleans. For lists, deleting the `lst` call makes the
code appear to use Scott-encoded lists. We could have acted similarly for
`Maybe`, but we save a few bytes with `must` and `maybe`: since we may
substitute anything we like for `undefined`, we compile each of `maybe
undefined`, `maybe id`, and `must` to `C(TI)`.

https://crypto.stanford.edu/~blynn/lambda/kiselyov.html[Bracket abstraction]
yields:

----------------------------------------------------------------
must = C(T I)
orChurch = B S(B B)
isPre = S(B orChurch(eq ##))(eq #@)
closes = S(B orChurch(eq #;))(eq #))
app = R(B(B Just)(B b(must(b(cons #`)))))(B B B)
esc = B(B must)(R(R cons(B B b))(B B B))
atom = S(B C(R id(R esc isPre))) cons
comp = B C(B(B must) flip)
sub = B(R Nothing)(R comp B)
more = R atom(B S(B(C(eq #()) sub))
switch = B(S(B S(C closes)))(S(B B(B C(B(B b) more)))(B app))
term = Y(B(B(B must))(B(B C) switch))
parseNonEmpty = R Nothing(B term(B(C(B B(must id)))(B(cons #;))))
parse = Y(B(S(T K))(B(B K)(B(B K) parseNonEmpty)))
----------------------------------------------------------------

Next, we:

  * Rewrite with prefix notation for application
  * Replace `id const flip cons eq Nothing` with combinators `I K C : =`.
  * Scott-encode `Nothing` and `Just` as `K` and `BKT`.
  * Refer to the nth function with `@` followed by ASCII code n + 31.

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
pure = B(B(BKT))(BCT)
bind = B(C(TK))T;
ap = C(BB(B bind (C(BB(B bind (B pure))))));
fmap = B ap pure;
(*>) = B ap (fmap (KI));
(<*) = B ap (fmap K);
(<|>) = B(B(R(BKT)))S;
------------------------------------------------------------------------

where we have inlined `Just = BKT` and `(,) = BCT`, and use the aliases
`ap = (<*>)` and `fmap = (<$>)`.

I toyed with replacing `Maybe` and pairs with continuations, for example:

----------------------------------------------------------------
pure x s kon1 kon2 = kon2 x s
(<*>) f x s kon1 kon2 = f s kon1 (\g t -> x t kon1 (\y u -> kon2 (g y) u))
(<|>) f g s kon1 kon2 = f s (g s kon1 kon2) kon2
sat f s kon1 kon2 = lst s kon1 (\h t -> f h (kon2 h t) kon1)
----------------------------------------------------------------

but these seem to produce large combinatory logic terms.

Our syntax tree goes into the following data type:

------------------------------------------------------------------------
data Ast = R ([Char] -> [Char]) | V Char | A Ast Ast | L Char Ast
------------------------------------------------------------------------

The `R` stands for "raw", and its field passes through unchanged during
bracket abstraction. Otherwise we have a lambda calculus that supports
one-character variable names. Scott-encoding yields:

------------------------------------------------------------------------
dR s   = \a b c d -> a s;
dV v   = \a b c d -> b v;
dA x y = \a b c d -> c x y;
dL x y = \a b c d -> d x y;
------------------------------------------------------------------------

which translates to:

------------------------------------------------------------------------
dR = B(BK)(B(BK)(B(BK)T));
dV = BK(B(BK)(B(BK)T));
dA = B(BK)(B(BK)(B(B(BK))(BCT)));
dL = B(BK)(B(BK)(B(BK)(BCT)));
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
pre = (.) . cons <$> (char '#' <|> char '@') <*> (cons <$> sat (const const))
atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (R <$> pre) <|> (V <$> var);
apps r = (((&) <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure id;
expr = ((&) <$> atom expr) <*> apps expr;
------------------------------------------------------------------------

As combinators, we have:

------------------------------------------------------------------------
sat = B(C(TK))(B(B(RK))(C(BS(BB))pure));
char = B sat(BT=);
var = sat(BC(S(B(BS(BB))(=#;))(=#))))
pre = ap(fmap(BB:)((<|>)(char##)(char#@)))(fmap:(sat(KK)));
atom = C(B(<|>)(C(B(<|>)(S(B(<|>)(B((*>)(char#())(C(<*)(char#)))))(B(ap((*>)(char#\)(fmap dL var)))((*>)(char#.)))))(fmap dR pre)))(fmap dV var);
apps = Y(B(R(pure I))(B(B(<|>))(B(S(B ap(B(fmap T)atom)))(B(fmap(C(BBB)(C dA)))))));
expr = Y(S(B ap(B(fmap T)atom))apps);
------------------------------------------------------------------------

where we have inlined the Church-encoded OR function `BS(BB)`.

The `babs` and `unlam` functions perform simple bracket abstraction, and
`shows` writes the resulting lambda-free `Ast` in ION assembly.

------------------------------------------------------------------------
vCheck f x = f x (V 'I') (A (V 'K') (V x))
unlam caseV = fix (\r t -> t (\x -> A (V 'K') (R x)) caseV (\x y -> A (A (V 'S') (r x)) (r y)) undefined)
babs = fix (\r t -> t R V (\x y -> A (r x) (r y)) (\x y -> unlam (vCheck (x ==)) (r y)))
------------------------------------------------------------------------

Putting it all together, `main` parses as many semicolon-terminated
expressions as it can and converts them to ION assembly.

------------------------------------------------------------------------
main s = (expr <* char ';') s id (\p -> p (\x t -> shows (babs x) . (';':) . main t)) "";
------------------------------------------------------------------------

These last few functions are:

------------------------------------------------------------------------
shows = Y(B(R?)(B(C(R:(TI)))(S(BC(B(BB)(B(BB)(B(B(:#`))))))I)));
vCheck = R(B(dA(dV#K))dV)(BS(R(dV#I)))
unlam = BY(B(B(R?))(R(S(BC(B(BB)(B(B dA)(B(dA(dV#S))))))I)(BB(BC(C(T(B(dA(dV#K))dR)))))));
babs = Y(S(BC(B(C(R dV(T dR)))(S(BC(B(BB)(B dA)))I)))(C(BB(B unlam (B vCheck =)))));
main = RK(Y(B(C(RI((<*)expr(char#;))))(BT(C(BB(BB(R(:#;)(BB(B shows babs)))))))));
------------------------------------------------------------------------

We replace the symbols with their `@` addresses and feed into our first
compiler to produce an ION assembly program that compiles a primitive lambda
calculus to ION assembly.

I should confess that some of these terms were produced by a program I wrote,
and I have not checked all of them manually. Hopefully, it's plausible this
could be done. Perhaps an acceptable procedure would be to plug terms into
link:../zh23/bubble.html[the animated reducer from my ZuriHac 2023 talk], and
watch closely to ensure each step is valid and the desired result is obatined.
The computer assists, but ultimately human eyes verify.

== Practically ==

We've reached a milestone. Thanks to our previous compiler, never again do we
convert LC to CL by hand.

But there's a catch. For each variable we abstract over, our bracket
abstraction algorithm applies the S combinator to every application. Hence for
N variables, this multiplies the number of applications by 2^N^, which is
intolerable for all but the smallest programs.

Thus our first priority is to optimize bracket abstraction. We stop recursively
adding S combinators as soon as we realize they are unnecessary by modifying
`vCheck` and `unlam`:

------------------------------------------------------------------------
vCheck f = fix (\r t -> t (\x -> False) f (\x y -> r x || r y) undefined);
unlam occurs = fix (\r t -> occurs t
  (t undefined (const (V 'I')) (\x y -> A (A (V 'S') (r x)) (r y)) undefined)
  (A (V 'K') t));
------------------------------------------------------------------------

(I'm unsure if it's worth explaining the optimization as I only understood
after fiddling around with combinators. Here goes anyway: I think of S as
unconditionally passing a variable to both children of an application node.
With a little analysis, we can tell when a child has no need for the variable.)

We desugar these with Y combinators, and inlining the Church-encoded OR.

------------------------------------------------------------------------
\f.Y(\r.\t.t(\x.KI)f(\x.\y.BS(BB)(rx)(ry))?);
\f.Y(\r.\t.ft(t?(K([dV]#I))(\x.\y.[dA]([dA]([dV]#S)(rx))(ry))?)([dA]([dV]#K)t));
------------------------------------------------------------------------

We replace symbols (this time sensibly denoted within square brackets) with
their `@`-addresses and feed it to our previous compiler, which yields massive
but manageable combinatory logic terms.

== Summary ==

Our three compilers are the following:

------------------------------------------------------------------------
`C`TI;``BS`BB;``S``B@!`=##`=#@;``S``B@!`=#;`=#);``R``B`B``BKT``BB`@ `B`:#```BBB;``B`B@ ``R``R:``BBB``BBB;``S``BC``RI``R@%@":;``BC``B`B@ C;``B`RK``R@'B;``R@&``BS``B`C`=#(@(;``B`S``BS`C@#``S``BB``BC``B`BB@)`B@$;`Y``B`B`B@ ``B`BC@*;``RK``B@+``B`C``BB`@ I`B`:#;;`Y``B`S`TK``B`BK``B`BK@,;

B(B(BKT))(BCT);B(C(TK))T;C(BB(B@!(C(BB(B@!(B@ ))))));B@"@ ;B@"(@#(KI));B@"(@#K);B(B(R(BKT)))S;B(BK)(B(BK)(B(BK)T));BK(B(BK)(B(BK)T));B(BK)(B(BK)(B(B(BK))(BCT)));B(BK)(B(BK)(B(BK)(BCT)));B(C(TK))(B(B(RK))(C(BS(BB))@ ));B@+(BT=);@+(BC(S(B(BS(BB))(=#;))(=#))));@"(@#(BB:)(@&(@,##)(@,#@)))(@#:(@+(KK)));C(B@&(C(B@&(S(B@&(B(@$(@,#())(C@%(@,#)))))(B(@"(@$(@,#\)(@#@*@-)))(@$(@,#.)))))(@#@'@.)))(@#@(@-);Y(B(R(@ I))(B(B@&)(B(S(B@"(B(@#T)@/)))(B(@#(C(BBB)(C@))))))));Y(S(B@"(B(@#T)@/))@0);Y(B(R?)(B(C(R:(TI)))(S(BC(B(BB)(B(BB)(B(B(:#`))))))I)));R(B(@)(@(#K))@()(BS(R(@(#I)));BY(B(B(R?))(R(S(BC(B(BB)(B(B@))(B(@)(@(#S))))))I)(BB(BC(C(T(B(@)(@(#K))@')))))));Y(S(BC(B(C(R@((T@')))(S(BC(B(BB)(B@))))I)))(C(BB(B@4(B@3=)))));RK(Y(B(C(RI(@%@1(@,#;))))(BT(C(BB(BB(R(:#;)(BB(B@2@5)))))))));

B(B(BKT))(BCT);B(C(TK))T;C(BB(B@!(C(BB(B@!(B@ ))))));B@"@ ;B@"(@#(KI));B@"(@#K);B(B(R(BKT)))S;B(BK)(B(BK)(B(BK)T));BK(B(BK)(B(BK)T));B(BK)(B(BK)(B(B(BK))(BCT)));B(BK)(B(BK)(B(BK)(BCT)));B(C(TK))(B(B(RK))(C(BS(BB))@ ));B@+(BT=);@+(BC(S(B(BS(BB))(=#;))(=#))));@"(@#(BB:)(@&(@,##)(@,#@)))(@#:(@+(KK)));C(B@&(C(B@&(S(B@&(B(@$(@,#())(C@%(@,#)))))(B(@"(@$(@,#\)(@#@*@-)))(@$(@,#.)))))(@#@'@.)))(@#@(@-);Y(B(R(@ I))(B(B@&)(B(S(B@"(B(@#T)@/)))(B(@#(C(BBB)(C@))))))));Y(S(B@"(B(@#T)@/))@0);Y(B(R?)(B(C(R:(TI)))(S(BC(B(BB)(B(BB)(B(B(:#`))))))I)));\f.Y(\r.\t.t(\x.KI)f(\x.\y.BS(BB)(rx)(ry))?);\f.Y(\r.\t.ft(t?(K(@(#I))(\x.\y.@)(@)(@(#S)(rx))(ry))?)(@)(@(#K)t));Y(S(BC(B(C(R@((T@')))(S(BC(B(BB)(B@))))I)))(C(BB(B@4(B@3=)))));RK(Y(B(C(RI(@%@1(@,#;))))(BT(C(BB(BB(R(:#;)(BB(B@2@5)))))))));
------------------------------------------------------------------------

== Term rewriting; bootstrapping ==

https://arxiv.org/pdf/1812.02243.pdf[Corrado Böhm's PhD thesis (1951)]
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
