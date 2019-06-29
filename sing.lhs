= The Compiler Singularity =

I have a confession. Behind the scenes, I've been running a computer program
to convert readable code to combinators or lambdas with one-character
variables, and to deal with `@` addresses.

Ideally, to avoid
"https://www.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf[trusting
trust]" issues, all this should have been done manually. Nonetheless, each of
the our terms seem short enough that a sufficiently motivated human could
verify them.

Now that the truth is out, instead of pretending to derive terms by hand, we
show the program-generated source of our next compiler up front:

------------------------------------------------------------------------
\a.\b.\c.\d.ac(bcd);
Y\a.\b.\c.\d.\e.b(cd\f.\g.e)\f.\g.ce\h.\i.f(h=)(agide)e;
Y\a.\b.\c.bc\d.\e.:d(aec);
\a.\b.\c.cab;
\a.\b.\c.ca;
\a.\b.@$(@#ab);
\a.\b.bK\c.\d.ac(@%cd)K;
\a.\b.bK\c.ca;
\a.\b.\c.@'(\d.\e.@'(\f.\g.@%(df)g)(be))(ac);
\a.\b.@((@%a)b;
\a.\b.\c.ac(bc)@$;
Y\a.\b.\c.\d.dc\e.\f.be(abcf);
\a.\b.\c.@((@)ab)c;
Y\a.\b.@*(@,:b(ab))(@%K);
\a.@,:a(@-a);
\a.@&\b.b(a=);
@,(KI);
@,K;
@0(@/#-)(@0(@/#-)(@0(@-(@&\a.C(a(#
=))))(@/#
)));
@-(@*(@&\a.@ (a(# =))(a(#
=)))@2);
\a.@1a@3;
B@4@/;
\a.\b.\c.\d.Cad(bcd);
@4(@.(@&\a.@6(#z(aL))(a(#aL))));
\a.\b.\c.\d.\e.ba;
\a.\b.\c.\d.\e.ca;
\a.\b.\c.\d.\e.\f.eab;
\a.\b.\c.\d.\e.\f.fab;
@)(C:K)(@4(@&(KK)));
@*(@0(@/#@)@<)(@,:(@/##)@<);
\a.@0(@5#\)(@,(C(@+@;))(@.@7)(@0(@/#-)(@0(@5#>)a)));
\a.@*(@*(@*(@0(@5#()(@1a(@5#))))(@>a))(@)@8@=))(@)@9@7);
Y\a.\b.@*(@,T(@?b)(@)(\c.\d.\e.c(@:ed))(ab)))(@%I);
Y\a.@,T(@?a)(@@a);
@,@#@7(@,(C(@+@;))(@-@7)(@0(@5#=)@A));
@0@3(@.(@1@B(@5#;)));
\a.\b.@+(\c.\d.@!a(cK)(\e.:#@(:eK))(Bd\e.# (#!-)(e+)))(Ka)b# ;
Y\a.\b.\c.cI(\d.@Ddb)(\d.\e.:#`(@"(abd)(abe)))?;
Y\a.\b.\c.c(\d.KI)(\d.@!bd)(\d.\e.@ (abd)(abe))\d.\e.C(@ (@!bd)(C(abe)));
Y\a.\b.\c.@Fbc(c?(K(@8(:#IK)))(\d.\e.@:(@:(@9(:#SK))(abd))(abe))?)(@:(@9(:#KK))c);
Y\a.\b.b@8@9(\c.\d.@:(ac)(ad))\c.\d.@Gc(ad);
Y\a.\b.\c.cK\d.\e.@"(@Eb(@H(d(KI))))(:#;(abe));
\a.@Ca(:#?K)(B(\b.@Ibb)(TK));
------------------------------------------------------------------------

(Again, for clarity we have placed a newline after each semicolon.)

Hopefully, it's plausible that the above can be verified by hand to
be equivalent to the following compiler. We have upgraded our previous parser
to support comments, whitespace, and variables consisting of lowercase letters.
We also added code to look up the index of a top-level definition.

Prefixing a character with `@` gives us direct access to the primitive
combinators. (Not to be confused with the `@` operator of ION assembly.)

------------------------------------------------------------------------
or f g x y = f x (g x y);
lsteq = @Y \r xs ys a b -> xs (ys a (\u u -> b)) (\x xt -> ys b (\y yt -> x(y(@=)) (r xt yt a b) b));
append = @Y \r xs ys -> xs ys (\x xt -> @: x (r xt ys));
pair x y f = f x y;
just x f g = g x;

pure x inp = just (pair x inp);
sat f inp = inp @K (\h t -> f h (pure h t) @K);
bind f m = m @K (\x -> x f);
ap x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
fmap f x = ap (pure f) x;
alt x y = \inp -> (x inp) (y inp) just;

foldr = @Y \r c n l -> l n (\h t -> c h(r c n t));
liftaa f x y = ap (fmap f x) y;
many = @Y \r p -> alt (liftaa @: p (r p)) (pure @K);
some p = liftaa @: p (many p);

char c = sat (\x -> x(c(@=)));
liftki = liftaa (@K @I);
liftk = liftaa @K;
com = liftki (char #-) (liftki (char #-) (liftki (many (sat (\c -> @C (c(#
(@=)))))) (char #
)));
sp = many (alt (sat (\c -> or (c(# (@=))) (c(#
(@=))))) com);
spc f = liftk f sp;
spch = @B spc char;
and f g x y = @C f y (g x y);
var = spc ( some (sat (\x -> and (#z(x(@L))) (x(#a(@L))) )));
lcr s   = \a b c d -> a s;
lcv v   = \a b c d -> b v;
lca x y = \a b c d -> c x y;
lcl x y = \a b c d -> d x y;

anyone = fmap (@C @: @K) (spc (sat (@K @K)));
pre = alt (liftki (char #@) anyone) (liftaa @: (char ##) anyone);
lam r = liftki (spch #\) (liftaa (@C (foldr lcl)) (some var) (liftki (char #-) (liftki (spch #>) r)));
atom r = alt (alt (alt (liftki (spch #() (liftk r (spch #)))) (lam r)) (fmap lcr pre)) (fmap lcv var);
apps = @Y \rr r -> alt (liftaa @T (atom r) (fmap (\vs v x -> vs (lca x v)) (rr r))) (pure @I);
expr = @Y \r -> liftaa @T (atom r) (apps r);

def = liftaa pair var (liftaa (@C (foldr lcl)) (many var) (liftki (spch #=) expr));
program = liftki sp (some (liftk def (spch #;)));

rank v ds = foldr (\d t -> lsteq v (d @K) (\n -> @: #@ (@: n @K)) (@B t \n -> # (#!(@-))(n(@+)) )) (@K v) ds # ;
show = @Y \r ds t -> t @I (\v -> rank v ds) (\x y -> @:#`(append (r ds x) (r ds y))) @?;

occurs = @Y(\r v t -> t (\x -> @K @I) (\x -> lsteq v x) (\x y -> or (r v x) (r v y)) @?);
unlam = @Y(\r v t -> occurs v t (t @? (@K (lcr (@:#I@K))) (\x y -> lca (lca (lcv (@:#S@K)) (r v x)) (r v y)) @?) (lca (lcv (@:#K@K)) t));
babs = @Y(\r t -> t lcr lcv (\x y -> lca (r x) (r y)) (\x y -> unlam x (r y)));

dump = @Y \r tab ds -> ds @K \h t -> append (show tab (babs (h (@K @I)))) (@: #; (r tab t));
main s = program s (@:#?@K) (@B (\ds -> dump ds ds) (@T @K));
------------------------------------------------------------------------

Our language is now friendly enough that we are willing to work in it
with our bare hands. No more cheating.

In other words, we have reached a singularity because this compiler is
self-hosting: it can read its 100% organic artisanal handmade source
and produce the corresponding ION assembly.
