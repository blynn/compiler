-- Separate fixity phase.
-- Export lists.
-- Requires `Const` values to be already rewritten.
module RTS where

import Base
import Ast
import Kiselyov
import Map
import Parser

import_qq_here = import_qq_here

libcHost = [r|#include<stdio.h>
static int env_argc;
int getargcount() { return env_argc; }
static char **env_argv;
int getargchar(int n, int k) { return env_argv[n][k]; }
static int nextCh, isAhead;
int eof_shim() {
  if (!isAhead) {
    isAhead = 1;
    nextCh = getchar();
  }
  return nextCh == -1;
}
void exit(int);
void putchar_shim(int c) { putchar(c); }
int getchar_shim() {
  if (!isAhead) nextCh = getchar();
  if (nextCh == -1) exit(1);
  isAhead = 0;
  return nextCh;
}
void errchar(int c) { fputc(c, stderr); }
void errexit() { fputc('\n', stderr); }
|]

libcWasm = [r|
extern u __heap_base;
void* malloc(unsigned long n) {
  static u bump = (u) &__heap_base;
  return (void *) ((bump += n) - n);
}
void errchar(int c) {}
void errexit() {}
|]

preamble = [r|#define EXPORT(f, sym) void f() asm(sym) __attribute__((visibility("default")));
void *malloc(unsigned long);
enum { FORWARD = 127, REDUCING = 126 };
static u *mem, *altmem, *sp, *spTop, hp;
static inline u isAddr(u n) { return n>=128; }
static u evac(u n) {
  if (!isAddr(n)) return n;
  u x = mem[n];
  while (isAddr(x) && mem[x] == _T) {
    mem[n] = mem[n + 1];
    mem[n + 1] = mem[x + 1];
    x = mem[n];
  }
  if (isAddr(x) && mem[x] == _K) {
    mem[n + 1] = mem[x + 1];
    x = mem[n] = _I;
  }
  u y = mem[n + 1];
  switch(x) {
    case FORWARD: return y;
    case REDUCING:
      mem[n] = FORWARD;
      mem[n + 1] = hp;
      hp += 2;
      return mem[n + 1];
    case _I:
      mem[n] = REDUCING;
      y = evac(y);
      if (mem[n] == FORWARD) {
        altmem[mem[n + 1]] = _I;
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

static void gc() {
  hp = 128;
  u di = hp;
  sp = altmem + TOP - 1;
  for(u *r = root; *r; r++) *r = evac(*r);
  *sp = evac(*spTop);
  while (di < hp) {
    u x = altmem[di] = evac(altmem[di]);
    di++;
    if (x != _F && x != _NUM) altmem[di] = evac(altmem[di]);
    di++;
  }
  spTop = sp;
  u *tmp = mem;
  mem = altmem;
  altmem = tmp;
}

static inline u app(u f, u x) { mem[hp] = f; mem[hp + 1] = x; return (hp += 2) - 2; }
static inline u arg(u n) { return mem[sp [n] + 1]; }
static inline int num(u n) { return mem[arg(n) + 1]; }
static inline void lazy2(u height, u f, u x) {
  u *p = mem + sp[height];
  *p = f;
  *++p = x;
  sp += height - 1;
  *sp = f;
}
static void lazy3(u height,u x1,u x2,u x3){u*p=mem+sp[height];sp[height-1]=*p=app(x1,x2);*++p=x3;*(sp+=height-2)=x1;}
typedef unsigned long long uu;
static inline void lazyDub(uu n) { lazy3(4, _V, app(_NUM, n), app(_NUM, n >> 32)); }
static inline uu dub(u lo, u hi) { return ((uu)num(hi) << 32) + (u)num(lo); }
|]

-- Main VM loop.
comdefsrc = [r|
F x = "foreign(arg(1));"
Y x = x "sp[1]"
Q x y z = z(y x)
QQ f a b c d = d(c(b(a(f))))
S x y z = x z(y z)
B x y z = x (y z)
C x y z = x z y
R x y z = y z x
V x y z = z x y
T x y = y x
K x y = "_I" x
I x = "sp[1] = arg(1); sp++;"
CONS x y z w = w x y
NUM x y = y "sp[1]"
DADD x y = "lazyDub(dub(1,2) + dub(3,4));"
DSUB x y = "lazyDub(dub(1,2) - dub(3,4));"
DMUL x y = "lazyDub(dub(1,2) * dub(3,4));"
DDIV x y = "lazyDub(dub(1,2) / dub(3,4));"
DMOD x y = "lazyDub(dub(1,2) % dub(3,4));"
ADD x y = "_NUM" "num(1) + num(2)"
SUB x y = "_NUM" "num(1) - num(2)"
MUL x y = "_NUM" "num(1) * num(2)"
QUOT x y = "_NUM" "num(1) / num(2)"
REM x y = "_NUM" "num(1) % num(2)"
DIV x y = "_NUM" "div(num(1), num(2))"
MOD x y = "_NUM" "mod(num(1), num(2))"
EQ x y = "num(1) == num(2) ? lazy2(2, _I, _K) : lazy2(2, _K, _I);"
LE x y = "num(1) <= num(2) ? lazy2(2, _I, _K) : lazy2(2, _K, _I);"
U_DIV x y = "_NUM" "(u) num(1) / (u) num(2)"
U_MOD x y = "_NUM" "(u) num(1) % (u) num(2)"
U_LE x y = "(u) num(1) <= (u) num(2) ? lazy2(2, _I, _K) : lazy2(2, _K, _I);"
REF x y = y "sp[1]"
READREF x y z = z "num(1)" y
WRITEREF x y z w = w "((mem[arg(2) + 1] = arg(1)), _K)" z
END = "return;"
ERR = "sp[1]=app(app(arg(1),_ERREND),_ERR2);sp++;"
ERR2 = "lazy3(2, arg(1), _ERROUT, arg(2));"
ERROUT = "errchar(num(1)); lazy2(2, _ERR, arg(2));"
ERREND = "errexit(); return;"
|]

argList t = case t of
  TC s -> [TC s]
  TV s -> [TV s]
  TAp (TC "IO") (TC u) -> [TC u]
  TAp (TAp (TC "->") x) y -> x : argList y
  _ -> [t]

cTypeName (TC "()") = "void"
cTypeName (TC "Int") = "int"
cTypeName (TC "Char") = "int"
cTypeName _ = "int"

ffiDeclare (name, t) = let tys = argList t in (concat
  [cTypeName $ last tys, " ", name, "(", intercalate "," $ cTypeName <$> init tys, ");\n"]++)

ffiArgs n t = case t of
  TAp (TC "IO") u -> ("", ((False, u), n))
  TAp (TAp (TC "->") _) y -> first (((if 3 <= n then ", " else "") ++ "num(" ++ shows n ")") ++) $ ffiArgs (n + 1) y
  _ -> ("", ((True, t), n))

needsNum t = case t of
  TC "Int" -> True
  TC "Char" -> True
  _ -> False

ffiDefine n (name, t) = ("case " ++) . shows n . (": " ++) . if ret == TC "()"
  then longDistanceCall . cont ("_K"++) . ("); break;"++)
  else ("{u r = "++) . longDistanceCall . cont ((if needsNum ret then "app(_NUM, r)" else "r") ++) . ("); break;}\n"++)
  where
  (args, ((isPure, ret), count)) = ffiArgs 2 t
  lazyn = ("lazy2(" ++) . shows (if isPure then count - 1 else count + 1) . (", " ++)
  cont tgt = if isPure then ("I, "++) . tgt else  ("app(arg("++) . shows (count + 1) . ("), "++) . tgt . ("), arg("++) . shows count . (")"++)
  longDistanceCall = (name++) . ("("++) . (args++) . ("); "++) . lazyn

arrCount = \case
  TAp (TAp (TC "->") _) y -> 1 + arrCount y
  _ -> 0

genExport m n = ("void f"++) . shows n . ("("++)
  . foldr (.) id (intersperse (',':) $ map (("u "++) .) xs)
  . ("){rts_reduce("++)
  . foldl (\s x -> ("app("++) . s . (",app(_NUM,"++) . x . ("))"++)) rt xs
  . (");}\n"++)
  where
  xs = map ((('x':) .) . shows) [0..m - 1]
  rt = ("root["++) . shows n . ("]"++)

genArg m a = case a of
  V s -> ("arg("++) . (maybe undefined shows $ lookup s m) . (')':)
  E (StrCon s) -> (s++)
  A x y -> ("app("++) . genArg m x . (',':) . genArg m y . (')':)
genArgs m as = foldl1 (.) $ map (\a -> (","++) . genArg m a) as
genComb (s, (args, body)) = let
  argc = ('(':) . shows (length args)
  m = zip args [1..]
  in ("case _"++) . (s++) . (':':) . (case body of
    A (A x y) z -> ("lazy3"++) . argc . genArgs m [x, y, z] . (");"++)
    A x y -> ("lazy2"++) . argc . genArgs m [x, y] . (");"++)
    E (StrCon s) -> (s++)
  ) . ("break;\n"++)

comb = (,) <$> wantConId <*> ((,) <$> many wantVarId <*> (res "=" *> combExpr))
combExpr = foldl1 A <$> some
  (V <$> wantVarId <|> E . StrCon <$> wantString <|> paren combExpr)
comdefs = case lexer posLexemes $ LexState comdefsrc (1, 1) of
  Left e -> error e
  Right (xs, _) -> case parse (braceSep comb) $ offside xs of
    Left e -> error e
    Right (cs, _) -> cs
comEnum s = maybe (error s) id $ lookup s $ zip (fst <$> comdefs) [1..]
comName i = maybe undefined id $ lookup i $ zip [1..] (fst <$> comdefs)

runFun = ([r|
static int div(int a, int b) { int q = a/b; return q - (((u)(a^b)) >> 31)*(q*b!=a); }
static int mod(int a, int b) { int r = a%b; return r + (((u)(a^b)) >> 31)*(!!r)*b; }

static void run() {
  for(;;) {
    if (mem + hp > sp - 8) gc();
    u x = *sp;
    if (isAddr(x)) *--sp = mem[x]; else switch(x) {
|]++)
  . foldr (.) id (genComb <$> comdefs)
  . ([r|
    }
  }
}
|]++)

rtsAPI opts = ([r|
void rts_init() {
  mem = malloc(TOP * sizeof(u)); altmem = malloc(TOP * sizeof(u));
  hp = 128;
  for (u i = 0; i < sizeof(prog)/sizeof(*prog); i++) mem[hp++] = prog[i];
  spTop = mem + TOP - 1;
}
|]++)
  . rtsReduce opts

-- Hash consing.
data Obj = Local String | Global String String | Code Int deriving Eq

instance Ord Obj where
  x <= y = case x of
    Local a -> case y of
      Local b -> a <= b
      _ -> True
    Global m a -> case y of
      Local _ -> False
      Global n b -> if m == n then a <= b else m <= n
      _ -> True
    Code a -> case y of
      Code b -> a <= b
      _ -> False

memget k@(a, b) = get >>= \(tab, (hp, f)) -> case mlookup k tab of
  Nothing -> put (insert k hp tab, (hp + 2, f . (a:) . (b:))) >> pure hp
  Just v -> pure v

enc t = case t of
  Lf n -> case n of
    Basic c -> pure $ Code $ comEnum c
    ChrCon c -> Code <$> memget (Code $ comEnum "NUM", Code $ ord c)
    StrCon s -> enc $ foldr (\h t -> Nd (Nd (lf "CONS") (Lf $ ChrCon h)) t) (lf "K") s
    Link m s _ -> pure $ Global m s
    _ -> error $ "BUG! " ++ show t
  LfVar s -> pure $ Local s
  Nd x y -> enc x >>= \hx -> enc y >>= \hy -> Code <$> memget (hx, hy)

asm combs = foldM
  (\symtab (s, t) -> (flip (insert s) symtab) <$> enc t)
  Tip combs

hashcons hp combs = (symtab', (hp', (mem++)))
  where
  (symtab, (_, (hp', memF))) = runState (asm combs) (Tip, (hp, id))
  symtab' = resolveLocal <$> symtab
  mem = resolveLocal <$> memF []
  resolveLocal = \case
    Code n -> Right n
    Local s -> resolveLocal $ symtab ! s
    Global m s -> Left (m, s)

codegenLocal (name, neat) (bigmap, (hp, f)) =
  (insert name localmap bigmap, (hp', f . f'))
  where
  (localmap, (hp', f')) = hashcons hp $ optiComb $ toAscList $ snd <$> typedAsts neat

codegen ffis mods = (bigmap', mem) where
  (bigmap, (_, memF)) = foldr codegenLocal (Tip, (128, id)) $ toAscList mods
  bigmap' = (resolveGlobal <$>) <$> bigmap
  mem = resolveGlobal <$> memF []
  ffiIndex = fromList $ zip (keys ffis) [0..]
  resolveGlobal = \case
    Left (m, s) -> if m == "{foreign}"
      then ffiIndex ! s
      else resolveGlobal $ (bigmap ! m) ! s
    Right n -> n

getIOType (Qual [] (TAp (TC "IO") t)) = Right t
getIOType q = Left $ "main : " ++ show q

compileWith topSize libc opts mods = do
  let
    ffis = foldr (\(k, v) m -> insertWith (error $ "duplicate import: " ++ k) k v m) Tip $ concatMap (toAscList . ffiImports) $ elems mods
    (bigmap, mem) = codegen ffis mods
    ffes = foldr (\(expName, v) m -> insertWith (error $ "duplicate export: " ++ expName) expName v m) Tip
      [ (expName, (addr, argcount))
      | (modName, neat) <- toAscList mods
      , (expName, ourName) <- toAscList $ ffiExports neat
      , let addr = maybe (error $ "missing: " ++ ourName) id $ mlookup ourName $ bigmap ! modName
      , let argcount = arrCount $ mustType modName ourName
      ]
    mustType modName s = case mlookup s $ typedAsts $ mods ! modName of
      Just (Qual [] t, _) -> t
      _ -> error "TODO: report bad exports"
    mayMain = do
      mainAddr <- mlookup "main" =<< mlookup "Main" bigmap
      mainType <- fst <$> mlookup "main" (typedAsts $ mods ! "Main")
      pure (mainAddr, mainType)
  mainStr <- case mayMain of
    Nothing -> pure ""
    Just (a, q) -> do
      getIOType q
      pure $ if "no-main" `elem` opts then "" else "int main(int argc,char**argv){env_argc=argc;env_argv=argv;rts_reduce(" ++ shows a ");return 0;}\n"

  pure
    $ ("typedef unsigned u;\n"++)
    . ("enum{TOP="++)
    . (topSize++)
    . (",_UNDEFINED=0,"++)
    . foldr (.) id (map (\(s, _) -> ('_':) . (s++) . (',':)) comdefs)
    . ("};\n"++)
    . ("static const u prog[]={" ++)
    . foldr (.) id (map (\n -> shows n . (',':)) mem)
    . ("};\nstatic u root[]={" ++)
    . foldr (.) id (map (\(addr, _) -> shows addr . (',':)) $ elems ffes)
    . ("0};\n" ++)
    . (preamble++)
    . (libc++)
    . foldr (.) id (ffiDeclare <$> toAscList ffis)
    . ("static void foreign(u n) {\n  switch(n) {\n" ++)
    . foldr (.) id (zipWith ffiDefine [0..] $ toAscList ffis)
    . ("\n  }\n}\n" ++)
    . runFun
    . rtsAPI opts
    . foldr (.) id (zipWith (\(expName, (_, argcount)) n -> ("EXPORT(f"++) . shows n . (", \""++) . (expName++) . ("\")\n"++) . genExport argcount n) (toAscList ffes) [0..])
    $ mainStr

compile = compileWith "1<<24" libcHost []

declWarts = ([r|#define IMPORT(m,n) __attribute__((import_module(m))) __attribute__((import_name(n)));
enum {
  ROOT_BASE = 1<<9,  // 0-terminated array of exported functions
  // HEAP_BASE - 4: program size
  HEAP_BASE = (1<<20) - 128 * sizeof(u),  // program
  TOP = 1<<22
};
static u *root = (u*) ROOT_BASE;
void errchar(int c) {}
void errexit() {}
|]++)

rtsAPIWarts opts = ([r|
static inline void rts_init() {
  mem = (u*) HEAP_BASE; altmem = (u*) (HEAP_BASE + (TOP - 128) * sizeof(u));
  hp = 128 + mem[127];
  spTop = mem + TOP - 1;
}

// Export so we can later find it in the wasm binary.
void rts_reduce(u) asm("reduce") __attribute__((visibility("default")));
|]++) . rtsReduce opts

rtsReduce opts =
  (if "pre-post-run" `elem` opts then ("void pre_run(void); void post_run(void);\n"++) else id)
  . ([r|
void rts_reduce(u n) {
  static u ready;if (!ready){ready=1;rts_init();}
  *(sp = spTop) = app(app(n, _UNDEFINED), _END);
|]++)
  . (if "pre-post-run" `elem` opts then ("pre_run();run();post_run();"++) else ("run();"++))
  . ("\n}\n"++)

ffiDeclareWarts (name, t) = let tys = argList t in (concat
  [cTypeName $ last tys, " ", name, "(", intercalate "," $ cTypeName <$> init tys, ") IMPORT(\"env\", \"", name, "\");\n"]++)

warts opts mods =
  ("typedef unsigned u;\n"++)
  . ("enum{_UNDEFINED=0,"++)
  . foldr (.) id (map (\(s, _) -> ('_':) . (s++) . (',':)) comdefs)
  . ("};\n"++)
  . declWarts
  . (preamble++)
  . (if "no-import" `elem` opts then ("#undef IMPORT\n#define IMPORT(m,n)\n"++) else id)
  . foldr (.) id (ffiDeclareWarts <$> toAscList ffis)
  . ([r|void foreign(u n) asm("foreign") __attribute__((visibility("default")));|]++)
  . ("void foreign(u n) {\n  switch(n) {\n" ++)
  . foldr (.) id (zipWith ffiDefine [0..] $ toAscList ffis)
  . ("\n  }\n}\n" ++)
  . runFun
  . rtsAPIWarts opts
  $ ""
  where
  ffis = foldr (\(k, v) m -> insertWith (error $ "duplicate import: " ++ k) k v m) Tip $ concatMap (toAscList . ffiImports) $ elems mods
