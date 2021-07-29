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

void parse(char *s) {
  hp = 128;
  tabn = 0;
  str = s;
  parseMore(str_get);
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
uncurry x y = y x;
(.) x y z = x (y z);

pair x y f = f x y;
(||) f g x y = f x (g x y);
(++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
ifNull xs a b = xs a (\_ _ -> b);
add r acc p = r (ifNull acc p ('`':(acc ++ p)));
isPre h = h('#'(==)) || h('@'(==));
suffix f h t = isPre h (t undefined (\a b -> pair (a:[]) b)) (pair [] t) (\x y -> f (h:x) y);
atom r h acc t = suffix (add r acc) h t;
sub r acc = uncurry (add r acc) . r "";
closes h = h(';'(==)) || h(')'(==));
if3 h x y z = closes h x (h('('(==)) y z);
switch r a h t = if3 h pair (sub r) (atom r h) a t;
term acc s = s undefined (\h t -> switch term acc h t);
parse s = s "" (\_ _ -> term "" s (\p t -> p ++ (';':parse t)));
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
"`Y``B`S`TK``B`BK``B`BK``B`C`@,K``B`C``BB@\"`B`:#;;"
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
(++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
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
char c = sat (\x -> x(c(==)));
var = sat (\c -> flip (c(';'(==)) || c(')'(==))));
pre = (:) <$> (char '#' <|> char '@') <*> (flip (:) const <$> sat (const const));
atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (R <$> pre) <|> (V <$> var);
apps r = (((&) <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure id;
expr = ((&) <$> atom expr) <*> apps expr;
show t = t id (\v -> v:[])(\x y -> '`':(show x ++ show y)) undefined;
unlam v = fix (\r t -> t (\x -> A (V 'K') (R x)) (\x -> x(v(==)) (V 'I') (A (V 'K') (V x))) (\x y -> A (A (V 'S') (r x)) (r y)) undefined);
babs t = t R V (\x y -> A (babs x) (babs y)) (\x y -> unlam x (babs y));
main s = (expr <* char ';') s "" (\p -> p (\x t -> show (babs x) ++ ";" ++ main t)));
*/
"BKT;"
"BCT;"
"BS(BB);"
"Y(B(CS)(B(B(C(BB:)))C));"
"B(B@ )@!;"
"B(C(TK))T;"
"C(BB(B@%(C(BB(B@%(B@$))))));"
"B@&@$;"
"B@&(@'(KI));"
"B@&(@'K);"
"B(B(R@ ))S;"
"B(BK)(B(BK)(B(BK)T));"
"BK(B(BK)(B(BK)T));"
"B(BK)(B(BK)(B(B(BK))(BCT)));"
"B(BK)(B(BK)(B(BK)(BCT)));"
"B(C(TK))(B(B(RK))(C(BS(BB))@$));"
"B@/(BT(T=));"
"@/(BC(S(B@\"(T(#;=)))(T(#)=))));"
"@&(@':(@*(@0##)(@0#@)))(@'(C:K)(@/(KK)));"
"C(B@*(C(B@*(S(B@*(B(@((@0#())(C@)(@0#)))))(B(@&(@((@0#\\)(@'@.@1)))(@((@0#.)))))(@'@+@2)))(@'@,@1);"
"Y(B(R(@$I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@-)))))));"
"Y(S(B@&(B(@'T)@3))@4);"
"Y(B(R?)(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@#)))I))));"
"BY(B(B(R?))(C(BB(BC(B(C(T(B(@-(@,#K))@+)))(C(BS(B(R(@,#I))(BT(T=))))(B(@-(@,#K))@,)))))(S(BC(B(BB)(B(B@-)(B(@-(@,#S))))))I)));"
"Y(S(BC(B(C(C(T@+)@,))(S(BC(B(BB)(B@-)))I)))(C(BB@7)));"
"Y(B(C(C(@)@5(@0#;))K))(BT(C(BB(B@#(C(B@#(B@6@8))(:#;K)))))));"
;
char *practically =
// Same as above, except:
/*
occurs v t = t (\x -> (\_ y -> y)) (\x -> x(v(==))) (\x y -> occurs v x || occurs v y) undefined;
unlam v t = occurs v t (t undefined (const (V 'I')) (\x y -> A (A (V 'S') (unlam v x)) (unlam v y)) undefined) (A (V 'K') t);
*/
"BKT;"
"BCT;"
"BS(BB);"
"Y(B(CS)(B(B(C(BB:)))C));"
"B(B@ )@!;"
"B(C(TK))T;"
"C(BB(B@%(C(BB(B@%(B@$))))));"
"B@&@$;"
"B@&(@'(KI));"
"B@&(@'K);"
"B(B(R@ ))S;"
"B(BK)(B(BK)(B(BK)T));"
"BK(B(BK)(B(BK)T));"
"B(BK)(B(BK)(B(B(BK))(BCT)));"
"B(BK)(B(BK)(B(BK)(BCT)));"
"B(C(TK))(B(B(RK))(C(BS(BB))@$));"
"B@/(BT(T=));"
"@/(BC(S(B@\"(T(#;=)))(T(#)=))));"
"@&(@':(@*(@0##)(@0#@)))(@'(C:K)(@/(KK)));"
"C(B@*(C(B@*(S(B@*(B(@((@0#())(C@)(@0#)))))(B(@&(@((@0#\\)(@'@.@1)))(@((@0#.)))))(@'@+@2)))(@'@,@1);"
"Y(B(R(@$I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@-)))))));"
"Y(S(B@&(B(@'T)@3))@4);"
"Y(B(R?)(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@#)))I))));"
"Y\\a.\\b.\\c.c(\\d.KI)(\\d.d(b=))(\\d.\\e.@\"(abd)(abe))?;"
"Y\\a.\\b.\\c.@7bc(c?(K(@,#I))(\\d.\\e.@-(@-(@,#S)(abd))(abe))?)(@-(@,#K)c);"
"Y(S(BC(B(C(C(T@+)@,))(S(BC(B(BB)(B@-)))I)))(C(BB@8)));"
"Y(B(C(C(@)@5(@0#;))K))(BT(C(BB(B@#(C(B@#(B@6@9))(:#;K)))))));"
;
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
"\\a.\\b.\\c.ac(bc)@$;"
"Y\\a.\\b.\\c.\\d.dc\\e.\\f.be(abcf);"
"\\a.\\b.\\c.@((@)ab)c;"
"Y\\a.\\b.@*(@,:b(ab))(@%K);"
"\\a.@,:a(@-a);"
"\\a.@&\\b.b(a=);"
"@,(KI);"
"@,K;"
"@0(@/#-)(@0(@/#-)(@0(@-(@&\\a.C(a(#\n=))))(@/#\n)));"
"@-(@*(@&\\a.@ (a(# =))(a(#\n=)))@2);"
"\\a.@1a@3;"
"B@4@/;"
"\\a.\\b.\\c.\\d.Cad(bcd);"
"@4(@.(@&\\a.@6(#z(aL))(a(#aL))));"
"\\a.\\b.\\c.\\d.\\e.ba;"
"\\a.\\b.\\c.\\d.\\e.ca;"
"\\a.\\b.\\c.\\d.\\e.\\f.eab;"
"\\a.\\b.\\c.\\d.\\e.\\f.fab;"
"@)(C:K)(@4(@&(KK)));"
"@*(@0(@/#@)@<)(@,:(@/##)@<);"
"\\a.@0(@5#\\)(@,(C(@+@;))(@.@7)(@0(@/#-)(@0(@5#>)a)));"
"\\a.@*(@*(@*(@0(@5#()(@1a(@5#))))(@>a))(@)@8@=))(@)@9@7);"
"Y\\a.\\b.@*(@,T(@?b)(@)(\\c.\\d.\\e.c(@:ed))(ab)))(@%I);"
"Y\\a.@,T(@?a)(@@a);"
"@,@#@7(@,(C(@+@;))(@-@7)(@0(@5#=)@A));"
"@0@3(@.(@1@B(@5#;)));"
"\\a.\\b.@+(\\c.\\d.@!a(cK)(\\e.:#@(:eK))(Bd\\e.# (#!-)(e+)))(Ka)b# ;"
"Y\\a.\\b.\\c.cI(\\d.@Ddb)(\\d.\\e.:#`(@\"(abd)(abe)))?;"
"Y\\a.\\b.\\c.c(\\d.KI)(\\d.@!bd)(\\d.\\e.@ (abd)(abe))\\d.\\e.C(@ (@!bd)(C(abe)));"
"Y\\a.\\b.\\c.@Fbc(c?(K(@8(:#IK)))(\\d.\\e.@:(@:(@9(:#SK))(abd))(abe))?)(@:(@9(:#KK))c);"
"Y\\a.\\b.b@8@9(\\c.\\d.@:(ac)(ad))\\c.\\d.@Gc(ad);"
"Y\\a.\\b.\\c.cK\\d.\\e.@\"(@Eb(@H(d(KI))))(:#;(abe));"
"\\a.@Ca(:#?K)(B(\\b.@Ibb)(TK));"
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

void rpg() {
  strcpy(buf, "I;");
  bufptr = buf + 2;
  lvlup(parenthetically);
  lvlup(exponentially);
  lvlup(practically);
  lvlup(singularity);
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
      fp_reset("raw");
      loadRaw(fp_get);
      run(ioget, pc);
      return 0;
    }
    if (!strcmp(argv[1], "asmWith")) {
      fp_reset(argv[2]);
      loadRaw(fp_get);
      run(ioget, pc);
      return 0;
    }
    return puts("bad command"), 0;
  }
  rpg();
  puts(buf);
  return 0;
}
