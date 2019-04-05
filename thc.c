typedef unsigned u;
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void die(char *s) { fprintf(stderr, "error: %s\n", s); exit(1); }

enum { TOP = 1<<26 };
u heap[TOP], *sp, hp, tab[256], tabn;
char *inp;

u heapOn(u f, u x) {
  heap[hp] = f;
  heap[hp + 1] = x;
  hp += 2;
  return hp - 2;
}

u run();

u parseTerm() {
  u c;
  do c = run(); while (c == '\n');
  switch(c) {
    case '0': return 0;
    case '#': return heapOn('#', run());
    case '@': return tab[run() - ' '];
    case '`':
      c = hp;
      hp += 2;
      heap[c] = parseTerm();
      heap[c + 1] = parseTerm();
      return c;
    default: return c;
  }
}

u parse() {
  tabn = 0;
  for(;;) {
    u c = parseTerm();
    if (c == ';') {
      c = *inp++;
      return heapOn(c, heapOn(tab[tabn - 1], heapOn('<', 0)));
    }
    tab[tabn++] = c;
    //if (run() != ';') die("expected ';'");
    c = run(); if (c != ';') printf("!%u(%c)\n",c,c), die("expected ';'");
  }
}

void reset(u root) {
  sp = heap + TOP - 3;
  *sp = root;
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
    u x = *sp;
    if (!x) break;
    if (x < 128) switch(x) {
      case 'Y': lazy(1, arg(1), sp[1]); break;
      case 'S': lazy(3, harg(1, 3), harg(2, 3)); break;
      case 'B': lazy(3, arg(1), harg(2, 3)); break;
      case 'C': lazy(3, harg(1, 3), arg(2)); break;
      case 'R': lazy(3, harg(2, 3), arg(1)); break;
      case 'I': lazy(2, arg(1), arg(2)); break;
      case 'T': lazy(2, arg(2), arg(1)); break;
      case 'K': lazy(2, 'I', arg(1)); break;
      case ':': lazy(4, harg(4, 1), arg(2)); break;
      case '<': ch = *inp++; ch <= 0 ? lazy(1, 'I', 'K') : lazy(1, heapOn(':', heapOn('#', ch)), heapOn('<', 0)); break;
      case '#': lazy(2, arg(2), sp[1]); break;
      case 1: ch = num(1); lazy(2, heapOn(arg(2), 0), heapOn('T', 1)); return ch;
      case 2: ouch(num(1)); lazy(2, heapOn(arg(2), 0), heapOn('T', 2)); break;
      case ',': lazy(1, heapOn(arg(1), 0), heapOn('T', 1)); reset(parse()); break;
      case '.': lazy(1, heapOn(arg(1), 0), heapOn('T', 2)); break;
      case '=': num(1) == num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I'); break;
      default: printf("?%u\n", x); die("unknown combinator");
    } else {
      *--sp = heap[x];
    }
  }
  return 0;
}

int runWith(int (*f)(int), char *str) {
  hp = 128;
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
char *parenCompiler =
/*
pair x y f = f x y;
uncurry x y = y x;
(&) x y = y x;
(.) x y z = x (y z);
(||) f g x y = f x (g x y);
(++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
add acc p = acc p (\_ _ -> ('`':(acc ++ p)));
isPre h = h('#'(==)) || h('@'(==));
suffix f h t = isPre h (t undefined (\a b -> f (h:(a:[])) b)) (f (h:[]) t);
atom r h acc t = suffix (r . (add acc)) h t;
sub r acc = uncurry ((r .) (add acc)) . r "";
closes h = h(';'(==)) || h(')'(==));
if3 h x y z = closes h x (h('('(==)) y z);
switch r a h t = if3 h pair (sub r) (atom r h) a t;
parseParen acc s = s undefined (\h t -> switch parseParen acc h t);
parse s = parseParen "" s (\p t -> p ";" (\_ _ -> p ++ (';':parse t)));
*/
"``BCT;"
"``BS`BB;"
"`Y``B`CS``B`B`C``BB:C;"
"``SS``B`BK``B`BK``B`B`:#`@\";"
"``S``B@!`T`##=`T`#@=;"
"``S``BS``B`BS``B`S``BB@$``B`B`C`T?``C``BBB``C``BB:``C:K``CB``C:K;"
"``BC``B`B@%``C``BBB@#;"
"``S``BC``B`BB``B`BT``C``BBB@#`TK;"
"``S``B@!`T`#;=`T`#)=;"
"``S``BC``B`BB``B`BB@(`T`#(=;"
"``BC``S``BS``B`C``C@)@ @'@&;"
"`Y``B`B`C`T?@*;"
"`Y``B`C`@+K``B`S``BB`T``:#;K``B`B`BK``B`B`BK``B`C``BB@\"`B`:#;;"
;

char *skiCompiler =
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
unlam v t = isFree v t (t undefined (const (V 'I')) (\x y -> A (A (V 'S') (unlam v x)) (unlam v y)) (\x y -> unlam v y)) (A (V 'K') t);
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
"Y(S(BS(B(BC)(B(S(BC(B(C(T(K(KI))))(BT(T=)))))(S(BS(B(BC)(B(B(BB))(B(B@0)))))I))))(B(B(B(BC)))(B(S(BC(B(BB)(B(B@0)(BT(T=))))))(B(BC)))));"
"Y(B(R(@.(@-#K)))(B(BS)(B(S(BS@7))(S(BS(B(BC)(B(B(C(C(T?)(K(@-#I)))))(S(BS(B(BC)(B(B(BB))(B(B(B@.))(B(B(@.(@-#S))))))))I))))(BK)))));"
"Y(S(BC(B(C(C(T@,)@-))(S(BC(B(BB)(B@.)))I)))(C(BB@8)));"
"Y(B(C(C(@)@5(@+#;))(:#;K)))(BT(C(BB(B@ (C(B@ (B@6@9))(:#;K)))))));"
;

char *semantically =
/*
(.) f g x = f (g x);
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

Ze   = \a b c d e -> a;
Su   = \x a b c d e -> b x;
Pass = \x a b c d e -> c x;
La = \x a b c d e -> d x;
Ap = \x y a b c d e -> e x y;

toDeb = fix (\r n e -> e
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

babsA = fix (\r x y -> x
  (\d -> lClo r d y)
  (\e -> lNee r e y)
  (\e -> lWea r e y)
  );

babs = fix (\r t -> t
  (Need (Closed (V 'I')))
  (Weak . r)
  (Closed)
  (\t -> r t
    (\d -> Closed (A (V 'K') d))
    id
    (babsA (Closed (V 'K'))))
  (\x y -> babsA (r x) (r y))
  );

nolam x = babs (toDeb (Pass . V) x) id undefined undefined;

showCNW t = t (show) (\e -> "[N]" ++ showCNW e) (\e -> "[W]" ++ showCNW e);

showDeb t = t "Z" (\v -> 'S':showDeb v)(\s -> show s)(\x -> '\\':showDeb x) (\x y -> '`':(showDeb x ++ showDeb y));

main s = (expr <* char ';') s ";" (\p -> p (\x t -> show (nolam x) ++ ";" ++ main t));
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
"BK(BK(BKK));"
"BK(B(BK)(B(BK)(B(BK)T)));"
"BK(BK(B(BK)(B(BK)T)));"
"BK(BK(BK(B(BK)T)));"
"B(BK)(B(BK)(B(BK)(B(BK)(BCT))));"
"Y(S(BS(B(BC)(B(S(BC(C(T(B@9@,)))))(S(BS(B(BC)(B(B(BB))(B(B@;)))))I))))(B(B(B(B@:)))(C(BBB)(B(C(BS(B(R@7)(CB(T=)))))(B@8)))));"
"B(BK)(B(BK)T);"
"BK(B(BK)T);"
"BK(BKT);"
"S(BS(B(BC)(B(S(BC(BT(B(B@=)@.))))(B(B(B@>))(CB(B@=(@.(@-#B))))))))(B(B(B@?))(CB@=));"
"S(BS(B(BC)(S(BS(B(BC)(B(BT)(B(B(B@>))(BC(CB(B@=(@.(@-#R)))))))))(B(B(B@>))(SB(T(@=(@-#S))))))))(B(B(B@>))(SB(T(@=(@-#C)))));"
"S(BS(B(BC)(S(BS(B(BC)(B(BT)(B(B(B@?))(B(R@=)(BB))))))(B(B(B@>))(SB(T(@=(@-#B))))))))(B(B@?));"
"Y(S(BC(B(BS)(S(BC(B(BS)(B(CB)(BC@@))))(BC@A))))(BC@B));"
"Y(S(BC(S(BC(B(R@=)(B(C(T(@>(@=(@-#I)))))(B@?))))(B(R(@C(@=(@-#K))))(B(RI)(R(B@=(@.(@-#K))))))))(S(BC(B(BB)(B@C)))I));"
"C(C(C(B@D(@<(B@9@-)))I)?)?;"
"Y(S(BC(B(C(T@6))(B(@ (:#[(:#N(:#]K)))))))(B(@ (:#[(:#W(:#]K))))));"
"Y(S(BC(S(BC(B(R@6)(B(C(T(:#ZK)))(B(:#S)))))(B(:#\\))))(B(B(B(:#`)))(S(BC(B(BB)(B@ )))I)));"
"Y(B(C(C(@)@5(@+#;))(:#;K)))(BT(C(BB(B@ (C(B@ (B@6@E))(:#;K)))))));"
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
  testCase(cat3(parenCompiler, "`K``:#f``:#xK;;", "."),  // atom id 'x' "f"
    "fx");
  testCase(cat3(parenCompiler, "`K```@&I#x``:#fK;;", "."),  // atom id 'x' "f"
    "`fx");
  testCase(cat3(parenCompiler, "`K````@)#)``:#1K``:#2K``:#3K;;", "."),  // if3 ')' "1" "2" "3"
    "1");
  testCase(cat3(parenCompiler, "``B`TK`@+K;;", ".just(one);not(two);"),  // fst . parseParen
    "````just``one");
  testCase(cat3(parenCompiler, ";", ".par(en);(t(he)(ses));K;;.extra"),
    "```par`en;``t`he``ses;K;;");
}

int pc(int c) { int r = putchar(c); fflush(stdout); return r; }
int main(int argc, char **argv) {
  char program[16384];
  strcpy(program, "");
  strcat(program, parenCompiler); strcat(program, ";,");
  strcat(program, skiCompiler); strcat(program, ";,");
  strcat(program, ski1Compiler); strcat(program, ";,");
  strcat(program, semantically); strcat(program, ";.");
  //strcat(program, "\\x.\\y.x;\\x.x;\\x.\\y.\\z.xz(yz);;");
  //strcat(program, "Y\\r.\\c.\\n.\\l.ln(\\h.\\t.ch(rcnt));");
  //strcat(program, "\\h.\\t.\\c.\\n.ch(tcn);");
  strcat(program, "\\x.\\y.\\z.yz;");
  strcat(program, ".");
  if (argc > 1) runTests(); else runWith(pc, program);
  return 0;
}
