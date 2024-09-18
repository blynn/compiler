= LC to CL, Semantically =

Compilers abandoned bracket abstraction long ago in favour of generating custom
combinators for each particular program, known as _supercombinators_.

We'll buck this trend, partly for notoriety, but also for novelty:
http://okmij.org/ftp/tagless-final/ski.pdf[a bracket abstraction algorithm
by Oleg Kiselyov] breathes new life into the old approach.

Rather than treating lambda terms as a syntax to be rewritten as combinators,
Kiselyov defines the meaning of a lambda term using combinators. The formal
semantics can be viewed as a bracket abstraction algorithm.

Much of our previous compiler remains the same. We have standard definitions:

------------------------------------------------------------------------
or f g x y = f x (g x y);
and f g x y = @C f y (g x y);
lsteq = @Y \r xs ys a b -> xs (ys a (\u u -> b)) (\x xt -> ys b (\y yt -> x(y @=) (r xt yt a b) b));
pair x y f = f x y;
just x f g = g x;
foldr = @Y \r c n l -> l n (\h t -> c h(r c n t));
------------------------------------------------------------------------

Parser combinators library:

------------------------------------------------------------------------
pure x inp = just (pair x inp);
bind f m = m @K (\x -> x f);
ap x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
fmap f x = ap (pure f) x;
alt x y = \inp -> (x inp) (y inp) just;
liftaa f x y = ap (fmap f x) y;
many = @Y \r p -> alt (liftaa @: p (r p)) (pure @K);
some p = liftaa @: p (many p);
liftki = liftaa (@K @I);
liftk = liftaa @K;
sat f inp = inp @K (\h t -> f h (pure h t) @K);
char c = sat (\x -> x(c @=));
------------------------------------------------------------------------

Scott encoding of a data structure we use to hold lambda terms:

------------------------------------------------------------------------
-- data LC = R (String -> String) | V String | A LC LC | L String LC
lcr s   = \a b c d -> a s;
lcv v   = \a b c d -> b v;
lca x y = \a b c d -> c x y;
lcl x y = \a b c d -> d x y;
------------------------------------------------------------------------

Parser:

------------------------------------------------------------------------
com = liftki (char #-) (liftki (char #-) (liftki (many (sat (\c -> @C (c(#
@=))))) (char #
)));
sp = many (alt (sat (\c -> or (c(# @=)) (c(#
(@=))))) com);
spc f = liftk f sp;
spch = @B spc char;
var = spc ( some (sat (\x -> and (#z(x @L)) (x(#a @L)) )));
anyone = fmap (@:) (spc (sat (@K @K)));
pre = ap (alt (fmap (@K @I) (char #@)) (fmap (@B @B @:) (char ##))) anyone;
lam r = liftki (spch #\) (liftaa (@C (foldr lcl)) (some var) (liftki (char #-) (liftki (spch #>) r)));
atom r = alt (fmap lcv var) (alt (liftki (spch #() (liftk r (spch #)))) (alt (fmap lcr pre) (lam r)));
apps = @Y \f r -> alt (liftaa @T (atom r) (fmap (\vs v x -> vs (lca x v)) (f r))) (pure @I);
expr = @Y \r -> liftaa @T (atom r) (apps r);
def = liftaa pair var (liftaa (@C (foldr lcl)) (many var) (liftki (spch #=) expr));
program = liftki sp (some (liftk def (spch #;)));
------------------------------------------------------------------------

Finally, something new; conversion to De Bruijn notation:

------------------------------------------------------------------------
-- data DB = Ze | Su DB | Pass LC | La DB | App DB DB
ze   = \    a b c d e -> a;
su   = \x   a b c d e -> b x;
pass = \x   a b c d e -> c x;
la   = \x   a b c d e -> d x;
app  = \x y a b c d e -> e x y;

debruijn = @Y (\r n e -> e
  (\s -> pass (lcr s))
  (\v -> foldr (\h m -> lsteq h v ze (su m)) (pass (lcv v)) n)
  (\x y -> app (r n x) (r n y))
  (\s t -> la (r (@: s n) t))
  );
------------------------------------------------------------------------

And Kiselyov's bracket abstraction algorithm from Section 4 of the paper:

------------------------------------------------------------------------
closed = \t a b c -> a t;
need   = \x a b c -> b x;
weak   = \x a b c -> c x;

lclo = \r d y -> y
  (\dd -> closed (lca d dd))
  (\e -> need (r (closed (lca (lcr (@:#B@K)) d)) e))
  (\e -> weak (r (closed d) e))
  ;

lnee = \r e y -> y
  (\d -> need (r (closed (lca (lcr (@:#R@K)) d)) e))
  (\ee -> need (r (r (closed (lcr (@:#S@K))) e) ee))
  (\ee -> need (r (r (closed (lcr (@:#C@K))) e) ee))
  ;

lwea = \r e y -> y
  (\d -> weak (r e (closed d)))
  (\ee -> need (r (r (closed (lcr (@:#B@K))) e) ee))
  (\ee -> weak (r e ee))
  ;

babsa = @Y (\r x y -> x
  (\d -> lclo r d y)
  (\e -> lnee r e y)
  (\e -> lwea r e y)
  );

babs = @Y (\r t -> t
  (need (closed (lcr (@:#I@K))))
  (@B weak r)
  closed
  (\t -> r t
    (\d -> closed (lca (lcr (@:#K@K)) d))
    @I
    (babsa (closed (lcr (@:#K@K)))))
  (\x y -> babsa (r x) (r y))
  );
nolam x = babs (debruijn @K x) @I @? @?;
------------------------------------------------------------------------

That leaves the code generator and the main function tying everything
together:

------------------------------------------------------------------------
rank ds v = foldr (\d t -> lsteq v (d @K) (\n -> @B (@:#@) (@:n)) (@B t \n -> #0(#1 @-)(n @+))) @? ds # ;
shows f = @Y \r t -> t @I f (\x y -> @B (@B (@:#`) (r x)) (r y)) @?;
dump tab = foldr (\h t -> shows (rank tab) (nolam (h (@K @I))) (@:#;t)) @K tab;
main s = program s (@:#?@K) (@B dump (@T @K));
------------------------------------------------------------------------

The grammar is identical, but the generated code is far smaller.
