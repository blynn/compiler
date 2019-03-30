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
  u c = run();
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
  u outer = run();
  tabn = 0;
  for(;;) {
    u c = parseTerm();
    if (c == ';') return heapOn(outer, heapOn(tab[tabn - 1], heapOn('<', 0)));
    tab[tabn++] = c;
    //if (run() != ';') printf("!%u(%c)",x,x), die("expected ';'");
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
      case 1: ch = num(1); lazy(2, heapOn(arg(2), 0), heapOn('T', 1)); if (ch == '\n') break; else return ch;
      case 2: ouch(num(1)); lazy(2, heapOn(arg(2), 0), heapOn('T', 2)); break;
      case '$': lazy(1, heapOn(arg(1), 0), heapOn('T', 1)); reset(parse()); break;
      case '>': lazy(1, heapOn(arg(1), 0), heapOn('T', 2)); break;
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
  reset(heapOn('$', heapOn('<', '0')));
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
term h = h(';'(==)) || h(')'(==));
if3 h x y z = term h x (h('('(==)) y z);
switch r a h t = if3 h pair (sub r) (atom r h) a t;
parseParen acc s = s undefined (\h t -> switch parseParen acc h t);
parse pre s = parseParen "" s (\p t -> p pre (\_ _ -> parse (pre ++ (p ++ ";")) t));
main = (++ ";") . parse ">";
*/
"``BCT;"
"``BS`BB;"
"`Y``B`CS``B`B`C``BB:C;"
"``SS``B`BK``B`BK``B`B`:#`@\";"
"``S``B@!`T`##=`T`#@=;"
"``S``BS``B`BS``B`S``BB@$``B`B`C`T?``R``R``RK:``BB:``BBB``R``RK:B;"
"``BC``B`B@%``R@#``BBB;"
"``S``BC``B`BB``B`BT``R@#``BBB`TK;"
"``S``B@!`T`#;=`T`#)=;"
"``S``BC``B`BB``B`BB@(`T`#(=;"
"``BC``S``BS``B`C``R@ @)@'@&;"
"`Y``B`B`C`T?@*;"
"`Y``B`B`C`@+K``B`S``BS``B`BBT``B`B`B`BK``B`B`B`BK``R``R``R``:#;K@\"``BB@\"``BBB;"
"``R``:#;K``B@\"`@,``:#$K;"
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
pre = (:) <$> (char '#' <|> char '@') <*> (sat (const const));
atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (R <$> pre) <|> (V <$> var);
apps r = (((&) <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure id;
expr = ((&) <$> atom expr) <*> apps expr;
show t = t id (\v -> v:[])(\x y -> '`':(show x ++ show y)) (\x y -> '\\' : (x : ('.' : show y)));
fix x = x (fix x);
unlam v = fix (\r t -> t (\x -> A (V 'K') (R x)) (\x -> x(v(==)) (V 'I') (A (V 'K') (V x))) (\x y -> A (A (V 'S') (r x)) (r y)) undefined);
babs t = t R V (\x y -> A (babs x) (babs y)) (\x y -> unlam x (babs y));
prog pre s = (expr <* char ';') s pre (\p -> p(\x t -> prog (pre ++ (show (babs x) ++ ";")) t));
main s = prog ">" s ++ ";";
*/
"Y(B(CS)(B(B(C(BB:)))C));"
"BCT;"
"BKT;"
"B(B@\")@!;"
"B(C(TK))(B(B(RK))(R@#(BS(BB))));"
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
"R(@'@-@1)(B@*(R(@'@,@2)(B@*(S(B@*(B(@((@+#())(R(@+#))@))))(B(@&(@((@+#\\)(@'@/@1)))(@((@+#.)))))));"
"Y(B(R(@#I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(R(C@.)(BBB)))))));"
"Y(S(B@&(B(@'T)@3))@4);"
"Y(S(BC(B(C(R(RK:)(TI)))(B(B(B(:#`)))(S(BC(B(BB)(B@ )))I))))(B(B(B(:#\\)))(B(C(BB:))(B(:#.)))));"
"BY(B(B(R?))(R(S(BC(B(BB)(B(B@.)(B(@.(@-#S))))))I)(BB(BC(B(C(T(B(@.(@-#K))@,)))(R(B(@.(@-#K))@-)(BS(B(R(@-#I))(BT(T=))))))))));"
"Y(S(BC(B(C(R@-(T@,)))(S(BC(B(BB)(B@.)))I)))(C(BB@7)));"
"Y(B(S(BC(C(@)@5(@+#;)))))(B(BT)(R(R(R(:#;K)(B@ (B@6@8)))(BB@ ))(BBB))));"
"R(:#;K)(B@ (@9(:#>K)));"
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
"B(C(TK))(B(B(RK))(R@#(BS(BB))));"
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
"R(@'@-@1)(B@*(R(@'@,@2)(B@*(S(B@*(B(@((@+#())(R(@+#))@))))(B(@&(@((@+#\\)(@'@/@1)))(@((@+#.)))))));"
"Y(B(R(@#I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(R(C@.)(BBB)))))));"
"Y(S(B@&(B(@'T)@3))@4);"
"Y(S(BC(B(C(R(RK:)(TI)))(B(B(B(:#`)))(S(BC(B(BB)(B@ )))I))))(B(B(B(:#\\)))(B(C(BB:))(B(:#.)))));"
//"Y(S(BS(B(BC)(B(S(BC(B(C(T(K(KI))))(BT(T=)))))(S(BS(B(BC)(B(B(BB))(B(B@0)))))I))))(B(B(B(BC)))(B(S(BC(B(BB)(B(B@0)(BT(T=))))))(B(BC)))));"
"Y\\r.\\v.\\t.t(\\x.KI)(\\x.x(v=))(\\x.\\y.@0(rvx)(rvy))(\\x.\\y.@0(C(x(v=)))(C(rvy)));"
//"Y(B(R(@.(@-#K)))(B(BS)(B(S(BS@7))(S(BS(B(BC)(B(B(C(R(K(@-#I))(T?))))(S(BS(B(BC)(B(B(BB))(B(B(B@.))(B(B(@.(@-#S))))))))I))))(BK)))));"
"Y\\r.\\v.\\t.@7vt(t?(K(@-#I))(\\x.\\y.@.(@.(@-#S)(rvx))(rvy))(\\x.\\y.rvy))(@.(@-#K)t);"
"Y(S(BC(B(C(R@-(T@,)))(S(BC(B(BB)(B@.)))I)))(C(BB@8)));"
"Y(B(S(BC(C(@)@5(@+#;)))))(B(BT)(R(R(R(:#;K)(B@ (B@6@9)))(BB@ ))(BBB))));"
"R(:#;K)(B@ (@:(:#>K)));"
;

char *cat3(char *a, char *b, char *c) {
  static char buf[16384];
  strcpy(buf, a);
  strcat(buf, b);
  strcat(buf, c);
  return buf;
}

void runTests() {
  testCase(">I;;Hello, World!\n", "Hello, World!\n");
  testCase(">" "`K``:#O``:#KK;;", "OK");
  testCase("$I;;>I;;compile then run", "compile then run");
  testCase(">"
// xs ++ ys = case xs of { [] -> ys ; (x:xt) -> x : xt ++ ys }
// f = fix \r xs ys -> xs ys (\x xt -> (:) x (r xt ys))
//   = Y(B(CS)(B(B(C(BB(:))))C))
// Sub-term B(B(C(BB(:))))C = \r ys -> \x xt -> (:) x (r xt ys)
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "`K``@ " "``:#B``:#eK" "``:#nK;"
    ";",
    "Ben");
  testCase(">"
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "``SS``B`BK``B`BK``B`B`:#`@ ;"  // \acc p = acc p (\_ _ -> '`':acc ++ p)
    "`@!``:#xK;"
    ";y",
    "`xy");
  testCase(cat3(">", parenCompiler, "`K``:#f``:#xK;;"),  // atom id 'x' "f"
    "fx");
  testCase(cat3(">", parenCompiler, "`K```@&I#x``:#fK;;"),  // atom id 'x' "f"
    "`fx");
  testCase(cat3(">", parenCompiler, "`K````@)#)``:#1K``:#2K``:#3K;;"),  // if3 ')' "1" "2" "3"
    "1");
  testCase(cat3(">", parenCompiler, "``B`TK`@+K;;just(one);not(two);"),  // fst . parseParen
    "````just``one");
  testCase(cat3(">", parenCompiler, ";par(en);(t(he)(ses));K;;extra"),
    "$```par`en;``t`he``ses;K;;");
}

int pc(int c) { int r = putchar(c); fflush(stdout); return r; }
int main(int argc, char **argv) {
  char program[16384];
  strcpy(program, "$");
  strcat(program, parenCompiler); strcat(program, ";");
  strcat(program, skiCompiler); strcat(program, ";");
  strcat(program, ski1Compiler); strcat(program, ";");
  //strcat(program, "\\x.\\y.x;\\x.x;\\x.\\y.\\z.xz(yz);;");
  strcat(program, "Y\\r.\\c.\\n.\\l.ln(\\h.\\t.ch(rcnt));");
  //strcat(program, "\\h.\\t.\\c.\\n.ch(tcn);");
  if (argc > 1) runTests(); else runWith(pc, program);
  return 0;
}
