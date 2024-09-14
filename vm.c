#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
typedef unsigned u;

enum { FORWARD = 27, REDUCING = 9 };

void die(char *s) { fprintf(stderr, "error: %s\n", s); exit(1); }

enum { TOP = 1<<23, TABMAX = 1<<10, BUFMAX = 1<<20 };
//enum { TOP = 1000000, TABMAX = 1<<10, BUFMAX = 1<<20 };
u *mem, *altmem, *sp, *spTop, hp, tab[TABMAX], tabn;

void stats() { printf("[HP = %u, stack usage = %td]\n", hp, spTop - sp); }

static inline u isAddr(u n) { return n>=128; }

u evac(u n) {
  if (!isAddr(n)) return n;
  u x = mem[n];
  while (isAddr(x) && mem[x] == 'T') {
    mem[n] = mem[n + 1];
    mem[n + 1] = mem[x + 1];
    x = mem[n];
  }
  if (isAddr(x) && mem[x] == 'K') {
    mem[n + 1] = mem[x + 1];
    x = mem[n] = 'I';
  }
  u y = mem[n + 1];
  switch(x) {
    case FORWARD: return y;
    case REDUCING:
      mem[n] = FORWARD;
      mem[n + 1] = hp;
      hp += 2;
      return mem[n + 1];
    case 'I':
      mem[n] = REDUCING;
      y = evac(y);
      if (mem[n] == FORWARD) {
        altmem[mem[n + 1]] = 'I';
        altmem[mem[n + 1] + 1] = y;
      } else {
        mem[n] = FORWARD;
        mem[n + 1] = y;
      }
      return mem[n + 1];
    default: break;
  }
  u z = hp;
  hp += 2;
  mem[n] = FORWARD;
  mem[n + 1] = z;
  altmem[z] = x;
  altmem[z + 1] = y;
  return z;
}

u gccount;
void gc() {
  gccount++;
  hp = 128;
  u di = hp;
  sp = altmem + TOP - 1;
  *sp = evac(*spTop);
  //fprintf(stderr, "GC %u\n", hp - 128);
  while (di < hp) {
    u x = altmem[di] = evac(altmem[di]);
    di++;
    if (x != 'a' && x != '#') altmem[di] = evac(altmem[di]);
    di++;
  }
  spTop = sp;
  u *tmp = mem;
  mem = altmem;
  altmem = tmp;
}

static inline u app(u f, u x) {
  mem[hp] = f;
  mem[hp + 1] = x;
  hp += 2;
  return hp - 2;
}

static u root_memo;
void reset(u root) {
  root_memo = root;
  *(sp = spTop) = app(
    app(app(root, app('0', '?')), '.'), app('T', '1'));
}

void loadRaw(u (*get)()) {
  hp = 128 - 1;
  for (;;) {
    u c;
    do c = get(); while (c && (c < '0' || c > '9'));
    if (!c) break;
    u n = 0;
    for (;;) {
      if (c < '0' || c > '9') break;
      n = 10*n + c - '0';
      c = get();
    }
    mem[hp++] = n;
  }
  reset(mem[128 - 1]);
}

u parseTerm(u (*get)()) {
  u n, c;
  do c = get(); while (c == '\n');
  switch(c) {
    case '`':
      c = parseTerm(get);
      return app(c, parseTerm(get));
    case '#': return app('#', get());
    case '@': return tab[get() - ' '];
    case '(':
      n = 0;
      while ((c = get()) != ')') n = 10*n + c - '0';
      return app('#', n);
    case '[':
      n = 0;
      while ((c = get()) != ']') n = 10*n + c - '0';
      return tab[n];
    default: return c;
  }
}

void parseMore(u (*get)()) {
  for(;;) {
    u c = parseTerm(get);
    if (!c) {
      reset(tab[tabn - 1]);
      return;
    }
    if (tabn == TABMAX) die ("table overflow");
    tab[tabn++] = c;
    if (get() != ';') die("expected ';'");
  }
}

char *str;
u str_get() { return *(unsigned char*)str++; }

void parseWith(u (*get)()) {
  hp = 128;
  tabn = 0;
  parseMore(get);
}

void parse(char *s) {
  str = s;
  parseWith(str_get);
}

void parseRaw(char *s) {
  str = s;
  loadRaw(str_get);
}

static inline u arg(u n) { return mem[sp [n] + 1]; }
static inline u num(u n) { return mem[arg(n) + 1]; }

static inline void lazy(u height, u f, u x) {
  u *p = mem + sp[height];
  *p = f;
  *++p = x;
  sp += height - 1;
  *sp = f;
}

static inline u apparg(u i, u j) { return app(arg(i), arg(j)); }

static void foreign(u n) {
  switch(n) {
    case 1: putchar(num(2)); lazy(4, app(arg(4), 'K'), arg(3)); break;
  };
}

void run(u (*get)(), void (*put)(u)) {
  gccount = 0;
  u c;
  clock_t start = clock();
  for(;;) {
    // static int ctr; if (++ctr == (1<<25)) stats(), ctr = 0;
    // static int gctr; if ((*sp == 'Y' || *sp == 'S') && ++gctr == (1<<20)) gc(), gctr = 0;
    if (mem + hp > sp - 8) gc();
    u x = *sp;
    if (isAddr(x)) *--sp = mem[x]; else switch(x) {
      case FORWARD: stats(); die("stray forwarding pointer");
      case '.': {
        clock_t end = clock();
        fprintf(stderr, "gcs = %u, time = %lfms, HP = %u\n", gccount, (end - start) * 1000.0 / (double) CLOCKS_PER_SEC, hp);
        return;
      }
      case 'Y': lazy(1, arg(1), sp[1]); break;
      case 'S': lazy(3, apparg(1, 3), apparg(2, 3)); break;
      case 'B': lazy(3, arg(1), apparg(2, 3)); break;
      case 'C': lazy(3, apparg(1, 3), arg(2)); break;
      case 'R': lazy(3, apparg(2, 3), arg(1)); break;
      case 'I': sp[1] = arg(1); sp++; break;
      case 'T': lazy(2, arg(2), arg(1)); break;
      case 'K': lazy(2, 'I', arg(1)); break;
      case ':': lazy(4, apparg(4, 1), arg(2)); break;
      case '0': c = get(); !c ? lazy(1, 'I', 'K') : lazy(1, app(':', app('#', c)), app('0', '?')); break;
      case '#': lazy(2, arg(2), sp[1]); break;
      case '1': put(num(1)); lazy(2, app(arg(2), '.'), app('T', '1')); break;
      case '=': num(1) == num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I'); break;
      case 'L': num(1) <= num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I'); break;
      case '*': lazy(2, '#', num(1) * num(2)); break;
      case '/': lazy(2, '#', num(1) / num(2)); break;
      case '%': lazy(2, '#', num(1) % num(2)); break;
      case '+': lazy(2, '#', num(1) + num(2)); break;
      case '-': lazy(2, '#', num(1) - num(2)); break;
      case 'a': {
        u mnt = arg(1);
        u m = mnt>>16;
        u n = (mnt>>8)&255;
        u t = mnt&255;
        sp += 2;
        u f = arg(m);
        for (; n; n--) f = app(f, mem[*sp++ + 1]);
        sp += t;
        mem[*sp] = 'I';
        mem[*sp + 1] = f;
        break;
      }
      case 'F': foreign(arg(1)); break;
      default: printf("?%u\n", x); die("unknown combinator");
    }
  }
}

char buf[BUFMAX];
char *bufptr, *buf_end;
void buf_reset() { bufptr = buf; }
void buf_put(u c) {
  if (bufptr == buf_end) die("buffer overflow");
  *bufptr++ = c;
}

void testCmp(char *inp, char *want) {
  str = inp;
  buf_reset();
  run(str_get, buf_put);
  *bufptr = 0;
  if (strcmp(buf, want)) printf("FAIL: got '%s', want '%s'\n", buf, want);
}

void testCase(char *prog, char *inp, char *want) {
  parse(prog);
  testCmp(inp, want);
}

void testCaseMore(char *prog, char *more, char *inp, char *want) {
  parse(prog);
  str = more;
  parseMore(str_get);
  testCmp(inp, want);
}

char *parenthetically =
/*
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
*/
"`C`TI;"
"``BS`BB;"
"``S``B@!`=##`=#@;"
"``S``B@!`=#;`=#);"
"``R``B`B``BKT``BB`@ `B`:#```BBB;"
"``B`B@ ``R``R:``BBB``BBB;"
"``S``BC``RI``R@%@\":;"
"``BC``B`B@ C;"
"``B`RK``R@'B;"
"``R@&``BS``B`C`=#(@(;"
"``B`S``BS`C@#``S``BB``BC``B`BB@)`B@$;"
"`Y``B`B`B@ ``B`BC@*;"
"``RK``B@+``B`C``BB`@ I`B`:#;;"
"`Y``B`S`TK``B`BK``B`BK@,;"
;

char *exponentially =
/*
id x = x;
const x _ = x;
(&) x f = f x;
flip f x y = f y x;
fix x = x (fix x);
Nothing x _ = x;
Just x f g = g x;
P x y f = f x y;
(||) f g x y = f x (g x y);
pure x inp = Just (P x inp);
bind f m = m Nothing (\x -> x f);
(<*>) x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
(<$>) f x = pure f <*> x;
(*>) p q = (\_ x -> x) <$> p <*> q;
(<*) p q = (\x _ -> x) <$> p <*> q;
(<|>) x y = \inp -> (x inp) (y inp) Just;
R s   = \a b c d -> a s;
V v   = \a b c d -> b v;
A x y = \a b c d -> c x y;
L x y = \a b c d -> d x y;
sat f inp = inp Nothing (\h t -> f h (pure h t) Nothing);
char c = sat (\x -> x((==)c));
var = sat (\c -> flip (c((==)';') || c((==)')')));
pre = (.) . cons <$> (char '#' <|> char '@') <*> (cons <$> sat (const const))
atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (R <$> pre) <|> (V <$> var);
apps r = (((&) <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure id;
expr = ((&) <$> atom expr) <*> apps expr;
shows = fix(\r t -> t id cons (\x y -> b (b (cons '`') (r x)) (r y)) undefined);
vCheck v x = eq v x (V 'I') (A (V 'K') (V x))
unlam caseV = fix (\r t -> t (\x -> A (V 'K') (R x)) caseV (\x y -> A (A (V 'S') (r x)) (r y)) undefined)
babs = fix (\r t -> t R V (\x y -> A (r x) (r y)) (\x y -> unlam (vCheck x) (r y)))
main s = (fix (\r s -> (expr <* char ';') s id (\p -> p (\x t -> b (b (shows (babs x)) (cons ';')) (r t))))) s "";
*/
"B(B(BKT))(BCT);"
"B(C(TK))T;"
"C(BB(B@!(C(BB(B@!(B@ ))))));"
"B@\"@ ;"
"B@\"(@#(KI));"
"B@\"(@#K);"
"B(B(R(BKT)))S;"
"B(BK)(B(BK)(B(BK)T));"
"BK(B(BK)(B(BK)T));"
"B(BK)(B(BK)(B(B(BK))(BCT)));"
"B(BK)(B(BK)(B(BK)(BCT)));"
"B(C(TK))(B(B(RK))(C(BS(BB))@ ));"
"B@+(BT=);"
"@+(BC(S(B(BS(BB))(=#;))(=#))));"
"@\"(@#(BB:)(@&(@,##)(@,#@)))(@#:(@+(KK)));"
"C(B@&(C(B@&(S(B@&(B(@$(@,#())(C@%(@,#)))))(B(@\"(@$(@,#\\)(@#@*@-)))(@$(@,#.)))))(@#@'@.)))(@#@(@-);"
"Y(B(R(@ I))(B(B@&)(B(S(B@\"(B(@#T)@/)))(B(@#(C(BBB)(C@))))))));"
"Y(S(B@\"(B(@#T)@/))@0);"
"Y(B(R?)(B(C(R:(TI)))(S(BC(B(BB)(B(BB)(B(B(:#`))))))I)));"
"R(B(@)(@(#K))@()(BS(R(@(#I)));"
"BY(B(B(R?))(R(S(BC(B(BB)(B(B@))(B(@)(@(#S))))))I)(BB(BC(C(T(B(@)(@(#K))@')))))));"
"Y(S(BC(B(C(R@((T@')))(S(BC(B(BB)(B@))))I)))(C(BB(B@4(B@3=)))));"
"RK(Y(B(C(RI(@%@1(@,#;))))(BT(C(BB(BB(R(:#;)(BB(B@2@5)))))))));"
;
char *practically =
// Same as above, except:
/*
vCheck v = fix (\r t -> t (\x -> False) (\x -> v == x) (\x y -> r x || r y) undefined);
unlam occurs = fix (\r t -> occurs t
  (t undefined (const (V 'I')) (\x y -> A (A (V 'S') (r x)) (r y)) undefined)
  (A (V 'K') t));
*/
"B(B(BKT))(BCT);"
"B(C(TK))T;"
"C(BB(B@!(C(BB(B@!(B@ ))))));"
"B@\"@ ;"
"B@\"(@#(KI));"
"B@\"(@#K);"
"B(B(R(BKT)))S;"
"B(BK)(B(BK)(B(BK)T));"
"BK(B(BK)(B(BK)T));"
"B(BK)(B(BK)(B(B(BK))(BCT)));"
"B(BK)(B(BK)(B(BK)(BCT)));"
"B(C(TK))(B(B(RK))(C(BS(BB))@ ));"
"B@+(BT=);"
"@+(BC(S(B(BS(BB))(=#;))(=#))));"
"@\"(@#(BB:)(@&(@,##)(@,#@)))(@#:(@+(KK)));"
"C(B@&(C(B@&(S(B@&(B(@$(@,#())(C@%(@,#)))))(B(@\"(@$(@,#\\)(@#@*@-)))(@$(@,#.)))))(@#@'@.)))(@#@(@-);"
"Y(B(R(@ I))(B(B@&)(B(S(B@\"(B(@#T)@/)))(B(@#(C(BBB)(C@))))))));"
"Y(S(B@\"(B(@#T)@/))@0);"
"Y(B(R?)(B(C(R:(TI)))(S(BC(B(BB)(B(BB)(B(B(:#`))))))I)));"
"\\f.Y(\\r.\\t.t(\\x.KI)f(\\x.\\y.BS(BB)(rx)(ry))?);"
"\\f.Y(\\r.\\t.ft(t?(K(@(#I))(\\x.\\y.@)(@)(@(#S)(rx))(ry))?)(@)(@(#K)t));"
"Y(S(BC(B(C(R@((T@')))(S(BC(B(BB)(B@))))I)))(C(BB(B@4(B@3=)))));"
"RK(Y(B(C(RI(@%@1(@,#;))))(BT(C(BB(BB(R(:#;)(BB(B@2@5)))))))));"
;

void runTests() {
  testCase("`KK;", "ignoreme", "");
  testCase("I;", "Hello, World!\n", "Hello, World!\n");
  testCase("``C`T?`KI;", "tail", "ail");
  testCase("`K``:#O``:#KK;", "", "OK");
  // xs ++ ys = case xs of { [] -> ys ; (x:xt) -> x : xt ++ ys }
  // f = fix \r xs ys -> xs ys (\x xt -> (:) x (r xt ys))
  //   = Y(B(CS)(B(B(C(BB(:))))C))
  // because B(B(C(BB(:))))C = \r ys -> \x xt -> (:) x (r xt ys)
  testCase(
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "`K``@ " "``:#B``:#eK" "``:#nK;",
    "",
    "Ben");
  testCase(
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "``SS``B`BK``B`BK``B`B`:#`@ ;"  // \acc p = acc p (\_ _ -> '`':acc ++ p)
    "`@!``:#xK;",
    "y",
    "`xy");
  testCase(
    "`Y``B`CS``B`B`C``BB:C;"  // (++)
    "``SS``B`BK``B`BK``B`B`:#`[0];"  // \acc p = acc p (\_ _ -> '`':acc ++ p)
    "`[1]``:#xK;",
    "y",
    "`xy");
  testCase("`K``:#f``:#xK;", "", "fx");
  // atom id 'x' "f"
  testCaseMore(parenthetically, "`K```@'I#x``:#fK;", "", "`fx");
  // if3 ')' "1" "2" "3"
  testCaseMore(parenthetically, "`K````@*#)``:#1K``:#2K``:#3K;", "", "1");
  // fst . term
  testCaseMore(parenthetically, "``B`TK`@,K;",
    "just(one);not(two);", "````just``one");
  testCase(parenthetically, "par(en);(t(he)(ses));K;(I);", "```par`en;``t`he``ses;K;I;");
}

FILE *fp;
void fp_reset(char *f) {
  fp = fopen(f, "r");
  if (!fp) die("fopen failed");
}
u fp_get() {
  u c = fgetc(fp);
  return c == EOF ? fclose(fp), 0 : c;
}

const char iocccshim[] = "infixr 5 ++;(<=) = intLE;";
const char *iocccp;
void ioccc_reset(char *f) {
  fp_reset(f);
  iocccp = iocccshim;
}
u ioccc_get() {
  return *iocccp ? *iocccp++ : fp_get();
}

void pc(u c) { putchar(c); fflush(stdout); }
u ioget() {
  int c = getchar();
  if (c == EOF) return 0;
  return c;
}

void lvlup(char *prog) {
  parse(buf);
  str = prog;
  buf_reset();
  run(str_get, buf_put);
  *bufptr = 0;
}

void lvlup_file(char *filename) {
  fprintf(stderr, "loading %s...\n", filename);
  parse(buf);
  fp_reset(filename);
  buf_reset();
  run(fp_get, buf_put);
  *bufptr = 0;
}

void lvlup_file_raw(char *filename) {
  fprintf(stderr, "loading %s...\n", filename);
  parseRaw(buf);
  fp_reset(filename);
  buf_reset();
  run(fp_get, buf_put);
  *bufptr = 0;
}

void singout() {
  strcpy(buf, "I;");
  bufptr = buf + 2;
  lvlup(parenthetically);
  lvlup(exponentially);
  lvlup(practically);
  lvlup_file("singularity.boot");
  lvlup_file("singularity");
  char *p = buf;
  do putchar(*p); while (*++p);
}

void rpg() {
  strcpy(buf, "I;");
  bufptr = buf + 2;
  lvlup(parenthetically);
  lvlup(exponentially);
  lvlup(practically);
  lvlup_file("singularity.boot");
  lvlup_file("singularity");
  lvlup_file("semantically");
  lvlup_file("stringy");
  lvlup_file("binary");
  lvlup_file("algebraically");
  lvlup_file("parity.hs");
  lvlup_file("fixity.hs");
  lvlup_file("typically.hs");
  lvlup_file("classy.hs");
  lvlup_file("barely.hs");
  lvlup_file("barely.hs");
  lvlup_file_raw("barely.hs");
}

void untyped(char *filename) {
  strcpy(buf, "I;");
  bufptr = buf + 2;
  lvlup(parenthetically);
  lvlup(exponentially);
  lvlup(practically);
  lvlup_file("singularity.boot");
  lvlup_file("singularity");
  lvlup_file("semantically");
  lvlup_file("stringy");
  lvlup_file("binary");
  lvlup_file("algebraically");
  lvlup_file("parity.hs");
  lvlup_file("fixity.hs");
  parse(buf);
  buf_reset();
  fp_reset(filename);
  run(fp_get, buf_put);
  *bufptr = 0;
  parse(buf);
  run(ioget, pc);
}

void dis(char *file) {
  fp_reset("raw");
  loadRaw(fp_get);
  fp_reset("disassembly.hs");
  buf_reset();
  run(fp_get, buf_put);
  parseRaw(buf);
  fp_reset(file);
  fprintf(stderr, "disassembling %s\n", file);
  run(fp_get, pc);
}

void runFile(char *f) {
  fp_reset("raw");
  loadRaw(fp_get);
  fp_reset(f);
  buf_reset();
  run(fp_get, buf_put);
  *bufptr = 0;
  parseRaw(buf);
  run(ioget, pc);
}

void ioccc(char *f) {
  fp_reset("raw");
  loadRaw(fp_get);
  ioccc_reset(f);
  buf_reset();
  run(ioccc_get, buf_put);
  *bufptr = 0;
  parseRaw(buf);
  run(ioget, pc);
}

void iotest() {
  fp_reset("raw");
  loadRaw(fp_get);
  str =
    "ioBind2 m k = ioBind m (\\_ -> k);"
    "flst xs n c = case xs of { [] -> n; (:) h t -> c h t };"
    "foldr c n l = flst l n (\\h t -> c h(foldr c n t));"
    "(.) f g x = f (g x);"
    "data Unit = Unit;"
    "flip f x y = f y x;"
    "map = flip (foldr . ((:) .)) [];"
    "mapM_ f = foldr (ioBind2 . f) (ioPure Unit);"
    "main = mapM_ putChar \"Hello, World!\\n\""
    ;
  buf_reset();
  run(str_get, buf_put);
  *bufptr = 0;
  parseRaw(buf);
  *sp = app(app(root_memo, '?'), '.');
  str = "";
  run(str_get, pc);
}

int main(int argc, char **argv) {
  mem = malloc(TOP * sizeof(u)); altmem = malloc(TOP * sizeof(u));
  buf_end = buf + BUFMAX;
  spTop = mem + TOP - 1;
  if (argc > 1) {
    if (!strcmp(argv[1], "untyped")) return untyped(argv[2]), 0;
    if (!strcmp(argv[1], "test")) return runTests(), 0;
    if (!strcmp(argv[1], "iotest")) return iotest(), 0;
    if (!strcmp(argv[1], "rawck")) {
      rpg();
      fp_reset("raw");
      str = buf;
      u c;
      while ((c = str_get())) if (c != fp_get()) die("raw check failed!");
      puts("OK");
      return 0;
    }
    if (!strcmp(argv[1], "run")) return runFile(argv[2]), 0;
    if (!strcmp(argv[1], "ioccc")) return ioccc(argv[2]), 0;
    if (!strcmp(argv[1], "testdis")) return dis("disassembly.hs"), 0;
    if (!strcmp(argv[1], "dis")) return dis(argv[2]), 0;
    if (!strcmp(argv[1], "asm")) {
      parseWith(ioget);
      printf("%u", root_memo);
      for(u i=128; i<hp; i++) printf(",%u", mem[i]);
      return 0;
    }
    if (!strcmp(argv[1], "runion")) {
      fp_reset(argv[2]);
      parseWith(fp_get);
      run(ioget, pc);
      return 0;
    }
    if (!strcmp(argv[1], "runheap")) {
      fp_reset(argv[2]);
      loadRaw(fp_get);
      run(ioget, pc);
      return 0;
    }
    if (!strcmp(argv[1], "sing")) return singout(), 0;
    return puts("bad command"), 0;
  }
  rpg();
  puts(buf);
  return 0;
}
