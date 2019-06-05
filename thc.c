typedef unsigned u;
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { FORWARD = 27, REDUCING = 9, RECUR = 10 };

void die(char *s) { fprintf(stderr, "error: %s\n", s); exit(1); }

enum { TOP = 1<<27 };
u heap[TOP], np, *sp, *spTop, hp = 128;
char *inp;

void stats() { printf("[HP = %u, stack usage = %ld]\n", hp, spTop - sp); }

u copy(u n) {
  if (n < 128) return n;
  if (np < TOP/2 && n < TOP/2) return n;
  if (np >= TOP/2 && n >= TOP/2) return n;
  u x = heap[n];
  if (x >= 128) {
    while (heap[x] == 'T') {
      heap[n] = heap[n + 1];
      heap[n + 1] = heap[x + 1];
      x = heap[n];
    }
    if (heap[x] == 'K') {
      heap[n + 1] = heap[x + 1];
      x = heap[n] = 'I';
    }
  }
  u y = heap[n + 1];
  switch(x) {
    case FORWARD: return y;
    case REDUCING:
      heap[n] = RECUR;
      heap[n + 1] = np;
      np += 2;
      return heap[n + 1];
    case RECUR: return y;
    case 'I':
      heap[n] = REDUCING;
      y = copy(y);
      if (heap[n] == RECUR) {
        heap[heap[n + 1]] = 'I';
        heap[heap[n + 1] + 1] = y;
      } else {
        heap[n + 1] = y;
      }
      heap[n] = FORWARD;
      return heap[n + 1];
    default: break;
  }
  u z = np;
  np += 2;
  heap[n] = FORWARD;
  heap[n + 1] = z;
  heap[z] = copy(x);
  heap[z + 1] = x == '#' || x == '0' ? y : copy(y);
  return z;
}

void gc() {
  if (sp > spTop) return;
  u *p;

  np = hp < TOP/2 ? TOP/2 : 128;
  u np0 = np;
  for(p = sp; p <= spTop; p++) *p = copy(*p);
  fprintf(stderr, "GC %u %ld\n", np - np0, spTop - sp);
  hp = np;
}

u heapOn(u f, u x) {
  heap[hp] = f;
  heap[hp + 1] = x;
  hp += 2;
  return hp - 2;
}

u buf[65536], bufn, tab[256], tabn;

u bufOn(u f, u x) {
  buf[bufn] = f;
  buf[bufn + 1] = x;
  bufn += 2;
  return bufn - 2;
}

u run();

u parseTerm() {
  u c, n;
  do c = run(); while (c == '\n');
  switch(c) {
    case '`':
      c = parseTerm();
      return bufOn(c, parseTerm());
    case '#': return bufOn('#', run());
    //case '@': return tab[run() - ' '];
    case '@': { n = run(); if (n > 255) die("OVERFLOW!"); return tab[n - ' ']; }
    case '(':
      n = 0;
      while ((c = run()) != ')') n = 10*n + c - '0';
      return bufOn('#', n);
    case '[':
      n = 0;
      while ((c = run()) != ']') n = 10*n + c - '0';
      return tab[n];
    default: return c;
  }
}

void reset(u root) { *(sp = spTop) = root; }

void parse() {
  stats();
  bufn = 128;
  tabn = 0;
  for(;;) {
    u c = parseTerm();
    if (c == ';') {
      c = *inp++;
      c = bufOn(c, bufOn(tab[tabn - 1], bufOn('<', '0')));
      for (hp = 128; hp < bufn; hp++) heap[hp] = buf[hp];
      reset(c);
      return;
    }
    if (tabn == 256) die ("table overflow");
    tab[tabn++] = c;
    if (run() != ';') die("expected ';'");
  }
}

u arg(u n) { return heap[sp [n] + 1]; }
u num(u n) { return heap[arg(n) + 1]; }

void lazy(u height, u f, u x) {
  u *p = heap + sp[height];
  *p = f;
  *++p = x;
  sp += height;
}

u harg(u i, u j) { return heapOn(arg(i), arg(j)); }

int (*ouch)(int);

u run() {
  int ch;
  for(;;) {
    // static int ctr; if (++ctr == (1<<25)) stats(), ctr = 0;
    static int gctr; if ((*sp == 'Y' || *sp == 'S') && ++gctr == (1<<20)) gc(), gctr = 0;
    u x = *sp;
    if (x < 128) switch(x) {
      case FORWARD: stats(); die("stray forwarding pointer");
      case '0': return 0;
      case 'Y': lazy(1, arg(1), sp[1]); break;
      case 'S': lazy(3, harg(1, 3), harg(2, 3)); break;
      case 'B': lazy(3, arg(1), harg(2, 3)); break;
      case 'C': lazy(3, harg(1, 3), arg(2)); break;
      case 'R': lazy(3, harg(2, 3), arg(1)); break;
      case 'I': sp[1] = arg(1); sp++; break;
      case 'T': lazy(2, arg(2), arg(1)); break;
      case 'K': lazy(2, 'I', arg(1)); break;
      case ':': lazy(4, harg(4, 1), arg(2)); break;
      case '<': ch = *inp++; ch <= 0 ? lazy(1, 'I', 'K') : lazy(1, heapOn(':', heapOn('#', ch)), heapOn('<', '?')); break;
      case '#': lazy(2, arg(2), sp[1]); break;
      case 1: ch = num(1); lazy(2, heapOn(arg(2), '?'), heapOn('T', 1)); return ch;
      case 2: ouch(num(1)); lazy(2, heapOn(arg(2), '0'), heapOn('T', 2)); break;
      case ',': lazy(1, heapOn(arg(1), '?'), heapOn('T', 1)); parse(); break;
      case '.': lazy(1, heapOn(arg(1), '0'), heapOn('T', 2)); break;
      case '=': num(1) == num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I'); break;
      case 'L': num(1) <= num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I'); break;
      case '*': lazy(2, '#', num(1) * num(2)); break;
      case '/': lazy(2, '#', num(1) / num(2)); break;
      case '%': lazy(2, '#', num(1) % num(2)); break;
      case '+': lazy(2, '#', num(1) + num(2)); break;
      case '-': lazy(2, '#', num(1) - num(2)); break;
      default: printf("?%u\n", x); die("unknown combinator");
    } else {
      *--sp = heap[x];
    }
  }
  return 0;
}

int runWith(int (*f)(int), char *str) {
  ouch = f;
  inp = str;
  reset(heapOn(',', heapOn('<', '0')));
  //for (u i = 128; i < hp; i++) printf(" %u", heap[i]); puts("");
  return run();
}

char testOut[1024], *testPtr;
int testCh(int c) { return *testPtr++ = c; }

void testCase(char *prog, char *want) {
  testPtr = testOut;
  runWith(testCh, prog);
  *testPtr = 0;
  if (strcmp(testOut, want)) printf("FAIL: got '%s', want '%s'\n", testOut, want);
}

// ASCII 32.. = " !\"#$%&'()*"
char *parenthetically =
/*
uncurry x y = y x;
(&) x y = y x;
(.) x y z = x (y z);

pair x y f = f x y;
(||) f g x y = f x (g x y);
(++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
ifNull xs a b = xs a (\_ _ -> b);
add r acc p = r (ifNull acc p ('`':(acc ++ p)));
isPre h = h('#'(==)) || h('@'(==));
--suffix f h t = isPre h (t undefined (\a b -> f (h:(a:[])) b)) (f (h:[]) t);
suffix f h t = isPre h (t undefined (\a b -> pair (a:[]) b)) (pair [] t) (\x y -> f (h:x) y);
atom r h acc t = suffix (add r acc) h t;
sub r acc = uncurry (add r acc) . r "";
closes h = h(';'(==)) || h(')'(==));
if3 h x y z = closes h x (h('('(==)) y z);
switch r a h t = if3 h pair (sub r) (atom r h) a t;
parseParen acc s = s undefined (\h t -> switch parseParen acc h t);
parse s = parseParen "" s (\p t -> ifNull p ";" (p ++ (';':parse t)));
*/
"``BCT;"
"``BS`BB;"
"`Y``B`CS``B`B`C``BB:C;"
"``B`R``BKK`BB;"
"``C``BBB``S``BS@#``B`B`:#`@\";"
"``S``B@!`T`##=`T`#@=;"
"``B`S``BC``C``BS``C``BB@%``C`T?``B@ ``C:K`@ K``C``BBB:;"
"``BC``B`B@&@$;"
"``S``BC``B`BB``B`BT@$`TK;"
"``S``B@!`T`#;=`T`#)=;"
"``S``BC``B`BB``B`BB@)`T`#(=;"
"``BC``S``BS``B`C``C@*@ @(@';"
"`Y``B`B`C`T?@+;"
"`Y``B`C`@,K``B`S``BB``C@#``:#;K``B`C``BB@\"`B`:#;;"
;

char *exponentially =
/*
flip f x y = f y x;
id x = x;
const x _ = x;
(&) x f = f x;
Nothing x _ = x;
(++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
P x y f = f x y;
Just x f g = g x;
pure x inp = Just (P x inp);
sat f inp = inp Nothing (\h t -> f h (pure h t) Nothing);
bind f m = m Nothing (\x -> x f);
(<*>) x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
(<$>) f x = pure f <*> x;
(*>) p q = (\_ x -> x) <$> p <*> q;
(<*) p q = (\x _ -> x) <$> p <*> q;
(<|>) x y = \inp -> (x inp) (y inp) Just;
char c = sat (\x -> x(c(==)));
R s   = \a b c d -> a s;
V v   = \a b c d -> b v;
A x y = \a b c d -> c x y;
L x y = \a b c d -> d x y;
(||) f g x y = f x (g x y);
var = sat (\c -> flip (c(';'(==)) || c(')'(==))));
pre = (:) <$> (char '#' <|> char '@') <*> (flip (:) const <$> sat (const const));
atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (R <$> pre) <|> (V <$> var);
apps r = (((&) <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure id;
expr = ((&) <$> atom expr) <*> apps expr;
show t = t id (\v -> v:[])(\x y -> '`':(show x ++ show y)) (\x y -> '\\' : (x : ('.' : show y)));
fix x = x (fix x);
unlam v = fix (\r t -> t (\x -> A (V 'K') (R x)) (\x -> x(v(==)) (V 'I') (A (V 'K') (V x))) (\x y -> A (A (V 'S') (r x)) (r y)) undefined);
babs t = t R V (\x y -> A (babs x) (babs y)) (\x y -> unlam x (babs y));
main s = (expr <* char ';') s ";" (\p -> p (\x t -> show (babs x) ++ ";" ++ main t)));
*/
"Y(B(CS)(B(B(C(BB:)))C));"
"BCT;"
"BKT;"
"B(B@\")@!;"
"B(C(TK))(B(B(RK))(C(BS(BB))@#));"
"B(C(TK))T;"
"C(BB(B@%(C(BB(B@%(B@#))))));"
"B@&@#;"
"B@&(@'(KI));"
"B@&(@'K);"
"B(B(R@\"))S;"
"B@$(BT(T=));"
"B(BK)(B(BK)(B(BK)T));"
"BK(B(BK)(B(BK)T));"
"B(BK)(B(BK)(B(B(BK))(BCT)));"
"B(BK)(B(BK)(B(BK)(BCT)));"
"BS(BB);"
"@$(BC(S(B@0(T(#;=)))(T(#)=))));"
"@&(@':(@*(@+##)(@+#@)))(@'(C:K)(@$(KK)));"
"C(B@*(C(B@*(S(B@*(B(@((@+#())(C@)(@+#)))))(B(@&(@((@+#\\)(@'@/@1)))(@((@+#.)))))(@'@,@2)))(@'@-@1);"
"Y(B(R(@#I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@.)))))));"
"Y(S(B@&(B(@'T)@3))@4);"
"Y(S(BC(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@ )))I))))(B(B(B(:#\\)))(B(C(BB:))(B(:#.)))));"
"BY(B(B(R?))(C(BB(BC(B(C(T(B(@.(@-#K))@,)))(C(BS(B(R(@-#I))(BT(T=))))(B(@.(@-#K))@-)))))(S(BC(B(BB)(B(B@.)(B(@.(@-#S))))))I)));"
"Y(S(BC(B(C(C(T@,)@-))(S(BC(B(BB)(B@.)))I)))(C(BB@7)));"
"Y(B(C(C(@)@5(@+#;))(:#;K)))(BT(C(BB(B@ (C(B@ (B@6@8))(:#;K)))))));"
;
char *ski1Compiler =
// Same as above, except:
/*
isFree v t = t (\x -> (\_ y -> y)) (\x -> x(v(==))) (\x y -> isFree v x || isFree v y) (\x y -> flip(x(v(==)) || flip(isFree v y)));
unlam v t = isFree v t (t undefined (const (V 'I')) (\x y -> A (A (V 'S') (unlam v x)) (unlam v y)) undefined) (A (V 'K') t);
*/
"Y(B(CS)(B(B(C(BB:)))C));"
"BCT;"
"BKT;"
"B(B@\")@!;"
"B(C(TK))(B(B(RK))(C(BS(BB))@#));"
"B(C(TK))T;"
"C(BB(B@%(C(BB(B@%(B@#))))));"
"B@&@#;"
"B@&(@'(KI));"
"B@&(@'K);"
"B(B(R@\"))S;"
"B@$(BT(T=));"
"B(BK)(B(BK)(B(BK)T));"
"BK(B(BK)(B(BK)T));"
"B(BK)(B(BK)(B(B(BK))(BCT)));"
"B(BK)(B(BK)(B(BK)(BCT)));"
"BS(BB);"
"@$(BC(S(B@0(T(#;=)))(T(#)=))));"
"@&(@':(@*(@+##)(@+#@)))(@'(C:K)(@$(KK)));"
"C(B@*(C(B@*(S(B@*(B(@((@+#())(C@)(@+#)))))(B(@&(@((@+#\\)(@'@/@1)))(@((@+#.)))))(@'@,@2)))(@'@-@1);"
"Y(B(R(@#I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@.)))))));"
"Y(S(B@&(B(@'T)@3))@4);"
"Y(S(BC(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@ )))I))))(B(B(B(:#\\)))(B(C(BB:))(B(:#.)))));"
"Y\\a.\\b.\\c.c(\\d.KI)(\\d.d(b=))(\\d.\\e.@0(abd)(abe))\\d.\\e.C(@0(d(b=))(C(abe)));"
"Y\\a.\\b.\\c.@7bc(c?(K(@-#I))(\\d.\\e.@.(@.(@-#S)(abd))(abe))?)(@.(@-#K)c);"
"Y(S(BC(B(C(C(T@,)@-))(S(BC(B(BB)(B@.)))I)))(C(BB@8)));"
"Y(B(C(C(@)@5(@+#;))(:#;K)))(BT(C(BB(B@ (C(B@ (B@6@9))(:#;K)))))));"
;

char *semantically =
/*
(++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
P x y f = f x y;
Just x f g = g x;
pure x inp = Just (P x inp);
sat f inp = inp K (\h t -> f h (pure h t) K);
bind f m = m K (\x -> x f);
(<*>) x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
(<$>) f x = pure f <*> x;
(*>) p q = (\_ x -> x) <$> p <*> q;
(<*) p q = (\x _ -> x) <$> p <*> q;
(<|>) x y = \inp -> (x inp) (y inp) Just;
char c = sat (\x -> x(c(==)));
R s   = \a b c d -> a s;
V v   = \a b c d -> b v;
A x y = \a b c d -> c x y;
L x y = \a b c d -> d x y;
(||) f g x y = f x (g x y);
var = sat (\c -> C (c(';'(==)) || c(')'(==))));
pre = (:) <$> (char '#' <|> char '@') <*> (C (:) K <$> sat (K K));
atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (R <$> pre) <|> (V <$> var);
apps r = ((T <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure I;
expr = (T <$> atom expr) <*> apps expr;
show t = t I (\v -> v:[])(\x y -> '`':(show x ++ show y)) (\x y -> '\\' : (x : ('.' : show y)));

Ze   = \a b c d e -> a;
Su   = \x a b c d e -> b x;
Pass = \x a b c d e -> c x;
La = \x a b c d e -> d x;
Ap = \x y a b c d e -> e x y;

toDeb = Y (\r n e -> e
  (\s -> Pass (R s))
  n
  (\x y -> Ap (r n x) (r n y))
  (\s t -> La (r (\v -> s(v(==)) Ze (Su (n v))) t))
  );

Closed = \t a b c -> a t;
Need = \x a b c -> b x;
Weak = \x a b c -> c x;

lClo = \r d y -> y
  (\d2 -> Closed (A d d2))
  (\e -> Need (r (Closed (A (V 'B') d)) e))
  (\e -> Weak (r (Closed d) e))
  ;

lNee = \r e y -> y
  (\d -> Need (r (Closed (A (V 'R') d)) e))
  (\e2 -> Need (r (r (Closed (V 'S')) e) e2))
  (\e2 -> Need (r (r (Closed (V 'C')) e) e2))
  ;

lWea = \r e y -> y
  (\d -> Weak (r e (Closed d)))
  (\e2 -> Need (r (r (Closed (V 'B')) e) e2))
  (\e2 -> Weak (r e e2))
  ;

babsA = Y (\r x y -> x
  (\d -> lClo r d y)
  (\e -> lNee r e y)
  (\e -> lWea r e y)
  );

babs = Y (\r t -> t
  (Need (Closed (V 'I')))
  (B Weak r)
  (Closed)
  (\t -> r t
    (\d -> Closed (A (V 'K') d))
    I
    (babsA (Closed (V 'K'))))
  (\x y -> babsA (r x) (r y))
  );

nolam x = babs (toDeb (B Pass V) x) I undefined undefined;

main s = (expr <* char ';') s ";" (\p -> p (\x t -> show (nolam x) ++ ";" ++ main t));
*/
"Y\\a.\\b.\\c.bc\\d.\\e.:d(aec);"
"\\a.\\b.\\c.cab;"
"\\a.\\b.\\c.ca;"
"\\a.\\b.@\"(@!ab);"
"\\a.\\b.bK\\c.\\d.ac(@#cd)K;"
"\\a.\\b.bK\\c.ca;"
"\\a.\\b.\\c.@%(\\d.\\e.@%(\\f.\\g.@#(df)g)(be))(ac);"
"\\a.\\b.@&(@#a)b;"
"\\a.\\b.@&(@'(\\c.\\d.d)a)b;"
"\\a.\\b.@&(@'(\\c.\\d.c)a)b;"
"\\a.\\b.\\c.ac(bc)@\";"
"\\a.@$\\b.b(a=);"
"\\a.\\b.\\c.\\d.\\e.ba;"
"\\a.\\b.\\c.\\d.\\e.ca;"
"\\a.\\b.\\c.\\d.\\e.\\f.eab;"
"\\a.\\b.\\c.\\d.\\e.\\f.fab;"
"\\a.\\b.\\c.\\d.ac(bcd);"
"@$\\a.C(@0(a(#;=))(a(#)=)));"
"@&(@':(@*(@+##)(@+#@)))(@'(C:K)(@$(KK)));"
"\\a.@*(@*(@*(@((@+#()(@)a(@+#))))(@&(@((@+#\\)(@'@/@1))(@((@+#.)a)))(@'@,@2))(@'@-@1);"
"Y\\a.\\b.@*(@&(@'T(@3b))(@'(\\c.\\d.\\e.c(@.ed))(ab)))(@#I);"
"Y\\a.@&(@'T(@3a))(@4a);"
"Y\\a.\\b.bI(\\c.:cK)(\\c.\\d.:#`(@ (ac)(ad)))\\c.\\d.:#\\(:c(:#.(ad)));"
"\\a.\\b.\\c.\\d.\\e.a;"
"\\a.\\b.\\c.\\d.\\e.\\f.ca;"
"\\a.\\b.\\c.\\d.\\e.\\f.da;"
"\\a.\\b.\\c.\\d.\\e.\\f.ea;"
"\\a.\\b.\\c.\\d.\\e.\\f.\\g.gab;"
"Y\\a.\\b.\\c.c(\\d.@9(@,d))b(\\d.\\e.@;(abd)(abe))\\d.\\e.@:(a(\\f.d(f=)@7(@8(bf)))e);"
"\\a.\\b.\\c.\\d.ba;"
"\\a.\\b.\\c.\\d.ca;"
"\\a.\\b.\\c.\\d.da;"
"\\a.\\b.\\c.c(\\d.@=(@.bd))(\\d.@>(a(@=(@.(@-#B)b))d))\\d.@?(a(@=b)d);"
"\\a.\\b.\\c.c(\\d.@>(a(@=(@.(@-#R)d))b))(\\d.@>(a(a(@=(@-#S))b)d))\\d.@>(a(a(@=(@-#C))b)d);"
"\\a.\\b.\\c.c(\\d.@?(ab(@=d)))(\\d.@>(a(a(@=(@-#B))b)d))\\d.@?(abd);"
"Y\\a.\\b.\\c.b(\\d.@@adc)(\\d.@Aadc)\\d.@Badc;"
"Y\\a.\\b.b(@>(@=(@-#I)))(B@?a)@=(\\c.ac(\\d.@=(@.(@-#K)d))I(@C(@=(@-#K))))\\c.\\d.@C(ac)(ad);"
"\\a.@D(@<(B@9@-)a)I??;"
"Y\\a.\\b.@)@5(@+#;)b(:#;K)\\c.c\\d.\\e.@ (@ (@6(@Ed))(:#;K))(ae);"
;

/*
(||) f g x y = f x (g x y);
lstEq = \xs ys a b -> xs (ys a (\_ _ -> b)) (\x xt -> ys b (\y yt -> x(y(==)) (lstEq xt yt a b) b));
(++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
P x y f = f x y;
Just x f g = g x;
pure x inp = Just (P x inp);
sat f inp = inp K (\h t -> f h (pure h t) K);
bind f m = m K (\x -> x f);
(<*>) x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
(<$>) f x = pure f <*> x;
(*>) p q = (\_ x -> x) <$> p <*> q;
(<*) p q = (\x _ -> x) <$> p <*> q;
(<|>) x y = \inp -> (x inp) (y inp) Just;

foldr = \c n l -> l n (\h t -> c h(foldr c n t));
many p = ((:) <$> p <*> many p) <|> pure [];
some p = (:) <$> p <*> many p;

char c = sat (\x -> x(c(==)));
com = char '-' *> (char '-' *> (many (sat (\c -> C (c('\n'(==))))) *> char '\n'));
sp = many (sat (\c -> c(' '(==)) || c('\n'(==))) <|> com);
spc f = f <* sp;
spch = B spc char;
(&&) f g x y = C f y (g x y);
var = spc ( some (sat (\x -> ('z'(x(<=)) && x('a'(<=))))));
R s   = \a b c d -> a s;
V v   = \a b c d -> b v;
A x y = \a b c d -> c x y;
L x y = \a b c d -> d x y;

anyOne = C (:) K <$> spc (sat (K K));
pre = (char '@' *> anyOne) <|> ((:) <$> char '#' <*> anyOne);
atom r = (spch '(' *> (r <* spch ')')) <|> (spch '\\' *> (C (foldr L) <$> some var) <*> (char '-' *> (spch '>' *> r))) <|> (R <$> pre) <|> (V <$> var);
apps r = ((T <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure I;
expr = T <$> atom expr <*> apps expr;
def = P <$> var <*> (C (foldr L) <$> many var <*> (spch '=' *> expr));
program = sp *> some (def <* spch ';');

rank v ds = foldr (\d t -> lstEq v (d K) (\n -> '@':(n:[])) (B t \n -> 32(33(-))(n(+)) )) (K v) ds 32;
show ds t = t I (\v -> rank v ds) (\x y -> '`':(show ds x ++ show ds y)) (\x y -> '\\' : (x ++ (" -> " ++ show ds y)));

Ze   = \a b c d e -> a;
Su   = \x a b c d e -> b x;
Pass = \x a b c d e -> c x;
La = \x a b c d e -> d x;
Ap = \x y a b c d e -> e x y;

toDeb = Y (\r n e -> e
  (\s -> Pass (R s))
  (\v -> foldr (\h m -> lstEq h v Ze (Su m)) (Pass (V v)) n)
  (\x y -> Ap (r n x) (r n y))
  (\s t -> La (r ((:) s n) t))
  );

Closed = \t a b c -> a t;
Need = \x a b c -> b x;
Weak = \x a b c -> c x;

lClo = \r d y -> y
  (\d2 -> Closed (A d d2))
  (\e -> Need (r (Closed (A (R "B") d)) e))
  (\e -> Weak (r (Closed d) e))
  ;

lNee = \r e y -> y
  (\d -> Need (r (Closed (A (R "R") d)) e))
  (\e2 -> Need (r (r (Closed (R "S")) e) e2))
  (\e2 -> Need (r (r (Closed (R "C")) e) e2))
  ;

lWea = \r e y -> y
  (\d -> Weak (r e (Closed d)))
  (\e2 -> Need (r (r (Closed (R "B")) e) e2))
  (\e2 -> Weak (r e e2))
  ;

babsA = Y (\r x y -> x
  (\d -> lClo r d y)
  (\e -> lNee r e y)
  (\e -> lWea r e y)
  );

babs = Y (\r t -> t
  (Need (Closed (R "I")))
  (B Weak r)
  (Closed)
  (\t -> r t
    (\d -> Closed (A (R "K") d))
    I
    (babsA (Closed (R "K"))))
  (\x y -> babsA (r x) (r y))
  );

nolam x = babs (toDeb [] x) I undefined undefined;

dump tab ds = ds ";" (\h t -> show tab (nolam (h(K I))) ++ (';':dump tab t));
main s = program s (@:#?@K) (@B (\ds -> dump ds ds) (@T @K));
*/
char *singularity =
"\\a.\\b.\\c.\\d.ac(bcd);"
"Y\\a.\\b.\\c.\\d.\\e.b(cd\\f.\\g.e)\\f.\\g.ce\\h.\\i.f(h=)(agide)e;"
"Y\\a.\\b.\\c.bc\\d.\\e.:d(aec);"
"\\a.\\b.\\c.cab;"
"\\a.\\b.\\c.ca;"
"\\a.\\b.@$(@#ab);"
"\\a.\\b.bK\\c.\\d.ac(@%cd)K;"
"\\a.\\b.bK\\c.ca;"
"\\a.\\b.\\c.@'(\\d.\\e.@'(\\f.\\g.@%(df)g)(be))(ac);"
"\\a.\\b.@((@%a)b;"
"\\a.\\b.@((@)(\\c.\\d.d)a)b;"
"\\a.\\b.@((@)(\\c.\\d.c)a)b;"
"\\a.\\b.\\c.ac(bc)@$;"
"Y\\a.\\b.\\c.\\d.dc\\e.\\f.be(abcf);"
"Y\\a.\\b.@,(@((@):b)(ab))(@%K);"
"\\a.@((@):a)(@.a);"
"\\a.@&\\b.b(a=);"
"@*(@0#-)(@*(@0#-)(@*(@.(@&\\a.C(a(#\n=))))(@0#\n)));"
"@.(@,(@&\\a.@ (a(# =))(a(#\n=)))@1);"
"\\a.@+a@2;"
"B@3@0;"
"\\a.\\b.\\c.\\d.Cad(bcd);"
"@3(@/(@&\\a.@5(#z(aL))(a(#aL))));"
"\\a.\\b.\\c.\\d.\\e.ba;"
"\\a.\\b.\\c.\\d.\\e.ca;"
"\\a.\\b.\\c.\\d.\\e.\\f.eab;"
"\\a.\\b.\\c.\\d.\\e.\\f.fab;"
"@)(C:K)(@3(@&(KK)));"
"@,(@*(@0#@)@;)(@((@):(@0##))@;);"
"\\a.@,(@,(@,(@*(@4#()(@+a(@4#))))(@((@*(@4#\\)(@)(C(@-@:))(@/@6)))(@*(@0#-)(@*(@4#>)a))))(@)@7@<))(@)@8@6);"
"Y\\a.\\b.@,(@((@)T(@=b))(@)(\\c.\\d.\\e.c(@9ed))(ab)))(@%I);"
"Y\\a.@((@)T(@=a))(@>a);"
"@((@)@#@6)(@((@)(C(@-@:))(@.@6))(@*(@4#=)@?));"
"@*@2(@/(@+@@(@4#;)));"
"\\a.\\b.@-(\\c.\\d.@!a(cK)(\\e.:#@(:eK))(Bd\\e.# (#!-)(e+)))(Ka)b# ;"
"Y\\a.\\b.\\c.cI(\\d.@Bdb)(\\d.\\e.:#`(@\"(abd)(abe)))?;"
"\\a.\\b.\\c.\\d.\\e.a;"
"\\a.\\b.\\c.\\d.\\e.\\f.ca;"
"\\a.\\b.\\c.\\d.\\e.\\f.da;"
"\\a.\\b.\\c.\\d.\\e.\\f.ea;"
"\\a.\\b.\\c.\\d.\\e.\\f.\\g.gab;"
"Y\\a.\\b.\\c.c(\\d.@F(@7d))(\\d.@-(\\e.\\f.@!ed@D(@Ef))(@F(@8d))b)(\\d.\\e.@H(abd)(abe))\\d.\\e.@G(a(:db)e);"
"\\a.\\b.\\c.\\d.ba;"
"\\a.\\b.\\c.\\d.ca;"
"\\a.\\b.\\c.\\d.da;"
"\\a.\\b.\\c.c(\\d.@J(@9bd))(\\d.@K(a(@J(@9(@7(:#BK))b))d))\\d.@L(a(@Jb)d);"
"\\a.\\b.\\c.c(\\d.@K(a(@J(@9(@7(:#RK))d))b))(\\d.@K(a(a(@J(@7(:#SK)))b)d))\\d.@K(a(a(@J(@7(:#CK)))b)d);"
"\\a.\\b.\\c.c(\\d.@L(ab(@Jd)))(\\d.@K(a(a(@J(@7(:#BK)))b)d))\\d.@L(abd);"
"Y\\a.\\b.\\c.b(\\d.@Madc)(\\d.@Nadc)\\d.@Oadc;"
"Y\\a.\\b.b(@K(@J(@7(:#IK))))(B@La)@J(\\c.ac(\\d.@J(@9(@7(:#KK))d))I(@P(@J(@7(:#KK)))))\\c.\\d.@P(ac)(ad);"
"\\a.@Q(@IKa)I??;"
"Y\\a.\\b.\\c.c(:#;K)\\d.\\e.@\"(@Cb(@R(d(KI))))(:#;(abe));"
"\\a.@Aa(:#?K)(B(\\b.@Sbb)(TK));"
;

char *cat3(char *a, char *b, char *c) {
  static char buf[16384];
  strcpy(buf, a);
  strcat(buf, b);
  strcat(buf, c);
  return buf;
}

void runTests() {
  testCase("I;;.Hello, World!\n", "Hello, World!\n");
  testCase("`K``:#O``:#KK;;.", "OK");
  testCase("I;;,I;;.compile then run", "compile then run");
  testCase(
// xs ++ ys = case xs of { [] -> ys ; (x:xt) -> x : xt ++ ys }
// f = fix \r xs ys -> xs ys (\x xt -> (:) x (r xt ys))
//   = Y(B(CS)(B(B(C(BB(:))))C))
// Sub-term B(B(C(BB(:))))C = \r ys -> \x xt -> (:) x (r xt ys)
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "`K``@ " "``:#B``:#eK" "``:#nK;"
    ";.",
    "Ben");
  testCase(
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "``SS``B`BK``B`BK``B`B`:#`@ ;"  // \acc p = acc p (\_ _ -> '`':acc ++ p)
    "`@!``:#xK;"
    ";."
    "y",
    "`xy");
  testCase(
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "``SS``B`BK``B`BK``B`B`:#`[0];"  // \acc p = acc p (\_ _ -> '`':acc ++ p)
    "`[1]``:#xK;"
    ";."
    "y",
    "`xy");
  testCase(cat3(parenthetically, "`K``:#f``:#xK;;", "."),  // atom id 'x' "f"
    "fx");
  testCase(cat3(parenthetically, "`K```@'I#x``:#fK;;", "."),  // atom id 'x' "f"
    "`fx");
  testCase(cat3(parenthetically, "`K````@*#)``:#1K``:#2K``:#3K;;", "."),  // if3 ')' "1" "2" "3"
    "1");
  testCase(cat3(parenthetically, "``B`TK`@,K;;", ".just(one);not(two);"),  // fst . parseParen
    "````just``one");
  testCase(cat3(parenthetically, ";", ".par(en);(t(he)(ses));K;;.extra"),
    "```par`en;``t`he``ses;K;;");
}

int pc(int c) { int r = putchar(c); fflush(stdout); return r; }

void catfile(char *s, char *f) {
  char *p = s + strlen(s);
  FILE *fp = fopen(f, "r");
  p += fread(p, 1, 65536, fp);
  fclose(fp);
  *p = 0;
}

int main(int argc, char **argv) {
  spTop = heap + TOP - 1;
  char program[1<<20];
  strcpy(program, "");
  strcat(program, parenthetically); strcat(program, ";,");
  strcat(program, exponentially); strcat(program, ";,");
  strcat(program, ski1Compiler); strcat(program, ";,");
  strcat(program, semantically); strcat(program, ";,");
  strcat(program, singularity); strcat(program, ";,");

  catfile(program, "singularity"); strcat(program, ";,");
  catfile(program, "stringy"); strcat(program, ";,");
  catfile(program, "binary"); strcat(program, ";,");
  catfile(program, "algebraically"); strcat(program, ";,");
  catfile(program, "parity.hs"); strcat(program, ";,");
  catfile(program, "fixity.hs"); strcat(program, ";,");
  catfile(program, "typically.hs"); strcat(program, ";,");
  catfile(program, "classy.hs"); strcat(program, ";,");
  catfile(program, "classy.hs"); strcat(program, ";.");

  strcat(program,
"infixl 6 + , -;"
"infixl 7 *;"
"infixr 5 : , ++;"
"infix 4 == , <=;"
"infixr 3 &&;"
"infixr 2 ||;"
"infixr 0 $;"
"($) f x = f x;"
"data Bool = True | False;"
"ife a b c = case a of { True -> b ; False -> c };"
"flst xs n c = case xs of { [] -> n; (:) h t -> c h t };"
"foldr c n l = flst l n (\\h t -> c h(foldr c n t));"
"elem k xs = foldr (\\x t -> ife (x == k) True t) False xs;"
"go s = ife (1+2*3 == 7) ('s':'u':'c':\"cess\n\") $ (\\x -> x) \"FAIL\n\";;."
);
  if (argc > 1) runTests(); else runWith(pc, program);
  return 0;
}
