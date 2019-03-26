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
    tab[tabn++] = parseTerm();
    u c = run();
    switch (c) {
      case '\n': return heapOn(outer, heapOn(tab[tabn - 1], heapOn('<', 0)));
      case ';': break;
      default: die("expected term terminator");
    }
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
  "`Y``B`CS``B`B`C``BB:C;"  // (++)
  "``SS``B`BK``B`BK``B`B`:#`@ ;"  // [add] = \acc p -> acc p ('`':acc ++ p)
  "``S``BS``B`BB`T`##=`T`#@=;"  // [isPre] = \h -> ('#' == h) || ('@' == h)
  "``S``BS``B`BS``B`S``BB@\"``B`B`C`T?``C``BBB``C``BB:``C:K``CB``C:K;"  // [suffix] = \f h t -> isPre h (t undefined (\a b -> f (h:a:K) b)) (f (h:K) t)
  "``BC``B`B@#``C``BBB@!;"  // [atom] = \r h acc t -> suffix (r . (add acc)) h t
  "``S``BC``B`BB``B`BT``C``BBB@!`TK;"  // [sub] = \r acc -> uncurry ((r .) ([add] acc)) . r ""
  "``S``BS``B`BB`T`#;=`T`#)=;"  // [term] = \h -> (';' == h) || (')' == h)
  // Because of how we handle primitve functions, this is: \hxy.=#;hx(=#)xy)
  "``S``BC``B`BB``B`BB@&`T`#(=;"  // [if3] = \hxyz.[term]hx(['(' ==]hyz)
  "``BC``S``BS``B`C``C@'``BCT@%@$;"  // [switch] = \raht.[if3]h[pair]([sub]r)([atom]rh)at
  "`Y``B`B`C`T?@(;"  // [parseParen] = fix \r acc s -> s undefined $ \h t -> [switch] r acc h t
  "`Y``B`B`C`@)K``B`S``BS``B`BBT``B`B`B`BK``B`B`B`BK``C``BBB``C``BB@ `:#;;" // [parse] = fix \r pre s -> [parseParen] "" s (\p t -> p pre (\_ _ -> r (pre ++ ';':p) t))
  "``B``C@ ``:#\nK`@*``:#>``:#IK"  // (++ "\n") . parse ">I"
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
atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (V <$> var);
apps r = (((&) <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure id;
expr = ((&) <$> atom expr) <*> apps expr;
undefined x = x;
-- Last field should be undefined. We only print lambdas when debugging.
show t = t id (\v -> v:[])(\x y -> '`':(show x ++ show y)) (\x y -> '\\' : (x : ('.' : show y)));
fix x = x (fix x);
unlam v = fix (\r t -> t (\x -> A (V 'K') (R x)) (\x -> x(v(==)) (V 'I') (A (V 'K') (V x))) (\x y -> A (A (V 'S') (r x)) (r y)) undefined);
babs t = t R V (\x y -> A (babs x) (babs y)) (\x y -> unlam x (babs y));
prog pre s = (expr <* char ';') s pre (\p -> p(\x t -> prog (pre ++ (show (babs x) ++ ";")) t));
main s = prog "" s;
*/
"Y(B(CS)(B(B(C(BB:)))C));"
"BCT;"
"BKT;"
"B(B@#)@\";"
"B(C(TK))(B(B(RK))(R@$(BS(BB))));"
"B(C(TK))T;"
"C(BB(B@&(C(BB(B@&(B@$))))));"
"B@'@$;"
"B@'(@((KI));"
"B@'(@(K);"
"B(B(R@#))S;"
"B@%(BT(T=));"
"B(BK)(B(BK)(B(BK)T));"
"BK(B(BK)(B(BK)T));"
"B(BK)(B(BK)(B(B(BK))(BCT)));"
"B(BK)(B(BK)(B(BK)(BCT)));"
"BS(BB);"
"@%(BC(S(B@1(T(#;=)))(T(#)=))));"
"R(@(@.@2)(B@+(S(B@+(B(@)(@,#())(R(@,#))@*)))(B(@'(@)(@,#\\)(@(@0@2)))(@)(@,#.)))));"
"Y(B(R(@$I))(B(B@+)(B(S(B@'(B(@(T)@3)))(B(@((R(C@/)(BBB)))))));"
"Y(S(B@'(B(@(T)@3))@4);"
"Y(S(BC(B(C(R(RK:)(TI)))(B(B(B(:#`)))(S(BC(B(BB)(B@!)))I))))(B(B(B(:#\\)))(B(C(BB:))(B(:#.)))));"
"BY(B(B(RI))(R(S(BC(B(BB)(B(B@/)(B(@/(@.#S))))))I)(BB(BC(B(C(T(B(@/(@.#K))@-)))(R(B(@/(@.#K))@.)(BS(B(R(@.#I))(BT(T=))))))))));"
"Y(S(BC(B(C(R@.(T@-)))(S(BC(B(BB)(B@/)))I)))(C(BB@7)));"
"Y(B(S(BC(C(@*@5(@,#;)))))(B(BT)(R(R(R(:#;K)(B@!(B@6@8)))(BB@!))(BBB))));"
"@9K;"
;
char *cat3(char *a, char *b, char *c) {
  static char buf[16384];
  strcpy(buf, a);
  strcat(buf, b);
  strcat(buf, c);
  return buf;
}

void runTests() {
  testCase(">I\nHello, World!\n", "Hello, World!\n");
  testCase(">" "`K``:#O``:#KK\n", "OK");
  testCase("$I\n>I\ncompile then run", "compile then run");
  testCase(">"
// xs ++ ys = case xs of { [] -> ys ; (x:xt) -> x : xt ++ ys }
// f = fix \r xs ys -> xs ys (\x xt -> (:) x (r xt ys))
//   = Y(B(CS)(B(B(C(BB(:))))C))
// Sub-term B(B(C(BB(:))))C = \r ys -> \x xt -> (:) x (r xt ys)
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "`K``@ " "``:#B``:#eK" "``:#nK"
    "\n",
    "Ben");
  testCase(">"
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "``SS``B`BK``B`BK``B`B`:#`@ ;"  // \acc p = acc p (\_ _ -> '`':acc ++ p)
    "`@!``:#xK"
    "\ny",
    "`xy");
  testCase(cat3(">", parenCompiler, ";`K```@$I#x``:#fK\n"),  // atom id 'x' "f"
    "`fx");
  testCase(cat3(">", parenCompiler, ";`K````@'#)``:#1K``:#2K``:#3K\n"),  // if3 ')' "1" "2" "3"
    "1");
  testCase(cat3(">", parenCompiler, ";``B`TK`@)K\njust(one);not(two);"),  // fst . parseParen
    "````just``one");
  testCase(cat3(">", parenCompiler, "\npar(en);(t(he)(ses));K;;extra"),
    ">I;```par`en;``t`he``ses;K\n");
}

int pc(int c) { int r = putchar(c); fflush(stdout); return r; }
int main(int argc, char **argv) {
  char program[16384];
  strcpy(program, cat3("$", parenCompiler, "\n"));
  strcat(program, skiCompiler);
  strcat(program, ";\\x.\\y.x;\\x.x;\\x.\\y.\\z.xz(yz);\necho");
  if (argc > 1) runTests(); else runWith(pc, program);
  return 0;
}
