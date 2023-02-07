-- Separate fixity phase.
-- Export lists.
-- Requires `Const` values to be already rewritten.
module RTS where

import Base
import Ast
import Kiselyov
import Map
import Parser

import Typer

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
void putchar_shim(int c) { putchar(c); fflush(stdout); }
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

-- Main VM loop.
comdefsrc = [r|
F x = "foreign(num(1));"
Y x = x "sp[1]"
Q x y z = z(y x)
QQ f a b c d = d(c(b(a(f))))
S x y z = x z(y z)
B x y z = x (y z)
BK x y z = x y
C x y z = x z y
R x y z = y z x
V x y z = z x y
T x y = y x
K x y = "_I" x
KI x y = "_I" y
I x = "sp[1] = arg(1); sp++;"
LEFT x y z = y x
CONS x y z w = w x y
NUM x y = y "sp[1]"
NUM64 x y = y "sp[1]"
FLO x = "lazy2(1, _I, app64d((double) num(1)));"
OLF x = "_NUM" "((int) flo(1))"
FADD x y = "lazy2(2, _I, app64d(flo(1) + flo(2)));"
FSUB x y = "lazy2(2, _I, app64d(flo(1) - flo(2)));"
FMUL x y = "lazy2(2, _I, app64d(flo(1) * flo(2)));"
FDIV x y = "lazy2(2, _I, app64d(flo(1) / flo(2)));"
FLE x y = "lazy2(2, _I, flo(1) <= flo(2) ? _K : _KI);"
FEQ x y = "lazy2(2, _I, flo(1) == flo(2) ? _K : _KI);"
PAIR64 x = "{uu n = (*((uu*) (mem + arg(1) + 2)));lazy2(1, app(_V, app(_NUM, n)), app(_NUM, n >> 32));}"
DADD x y = "lazyDub(dub(1,2) + dub(3,4));"
DSUB x y = "lazyDub(dub(1,2) - dub(3,4));"
DMUL x y = "lazyDub(dub(1,2) * dub(3,4));"
DDIV x y = "lazyDub(dub(1,2) / dub(3,4));"
DMOD x y = "lazyDub(dub(1,2) % dub(3,4));"
DSHL x y = "lazyDub(dub(1,2) << dub(3,4));"
DSHR x y = "lazyDub(dub(1,2) >> dub(3,4));"
ADD x y = "_NUM" "num(1) + num(2)"
SUB x y = "_NUM" "num(1) - num(2)"
MUL x y = "_NUM" "num(1) * num(2)"
QUOT x y = "_NUM" "num(1) / num(2)"
REM x y = "_NUM" "num(1) % num(2)"
DIV x y = "_NUM" "div(num(1), num(2))"
MOD x y = "_NUM" "mod(num(1), num(2))"
XOR x y = "_NUM" "num(1) ^ num(2)"
AND x y = "_NUM" "num(1) & num(2)"
OR x y = "_NUM" "num(1) | num(2)"
SHL x y = "_NUM" "num(1) << num(2)"
SHR x y = "_NUM" "num(1) < 0 ? ~(~num(1) >> num(2)) : num(1) >> num(2)"
U_SHR x y = "_NUM" "(u) num(1) >> (u) num(2)"
EQ x y = "lazy2(2, _I, num(1) == num(2) ? _K : _KI);"
LE x y = "lazy2(2, _I, num(1) <= num(2) ? _K : _KI);"
U_DIV x y = "_NUM" "(u) num(1) / (u) num(2)"
U_MOD x y = "_NUM" "(u) num(1) % (u) num(2)"
U_LE x y = "lazy2(2, _I, (u) num(1) <= (u) num(2) ? _K : _KI);"
REF x y = y "sp[1]"
NEWREF x y z = z ("_REF" x) y
READREF x y z = z "num(1)" y
WRITEREF x y z w = w "((mem[arg(2) + 1] = arg(1)), _K)" z
END = "return;"
ERR = "sp[1]=app(app(arg(1),_ERREND),_ERR2);sp++;"
ERR2 = "lazy3(2, arg(1), _ERROUT, arg(2));"
ERROUT = "errchar(num(1)); lazy2(2, _ERR, arg(2));"
ERREND = "errexit(); return;"
VMSCRATCH = "*scratchpadend++ = num(1); lazy2(3, app(arg(3), _K), arg(2));"
VMSCRATCHROOT = "*scratchpadend++ = _UNDEFINED; *scratchpadend++ = num(1); lazy2(3, app(arg(3), _K), arg(2));"
VMRUN = "vmrun();"
VMGCROOT = "vmgcroot();"
|]

argList t = case t of
  TC s -> [TC s]
  TV s -> [TV s]
  TAp (TC "IO") (TC u) -> [TC u]
  TAp (TAp (TC "->") x) y -> x : argList y
  _ -> [t]

cTypeName = \case
  TC "()" -> "void"
  TC "Word64" -> "uu"
  _ -> "int"

ffiDeclare opts (name, t) = (concat
  [cTypeName $ last tys, " ", name, "(", intercalate "," $ cTypeName <$> init tys, ")"]++) . attr name . (";\n"++)
  where
  tys = argList t
  attr
    | "warts" `elem` opts && not ("no-import" `elem` opts) =
      \name -> (" IMPORT(\"env\", \""++) . (name++) . ("\");\n"++)
    | otherwise = const id

ffiArgs n t = case t of
  TAp (TC "IO") u -> ("", ((False, u), n))
  TAp (TAp (TC "->") _) y -> first (((if 3 <= n then ", " else "") ++ "num(" ++ shows n ")") ++) $ ffiArgs (n + 1) y
  _ -> ("", ((True, t), n))

ffiDefine n (name, t) = ("case " ++) . shows n . (": " ++) . case ret of
  TC "()" -> longDistanceCall . cont ("_K"++) . ("); break;"++)
  _ -> ("{u r = "++) . longDistanceCall . cont (heapify ret ('r':)) . ("); break;}\n"++)
  where
  (args, ((isPure, ret), count)) = ffiArgs 2 t
  lazyn = ("lazy2(" ++) . shows (if isPure then count - 1 else count + 1) . (", " ++)
  cont tgt = if isPure then ("I, "++) . tgt else  ("app(arg("++) . shows (count + 1) . ("), "++) . tgt . ("), arg("++) . shows count . (")"++)
  longDistanceCall = (name++) . ("("++) . (args++) . ("); "++) . lazyn

genExport ourType n = ("void f"++) . shows n . ("("++)
  . foldr (.) id (intersperse (',':) $ map declare txs)
  . ("){rts_reduce("++)
  . foldl (\s tx -> ("app("++) . s . (',':) . uncurry heapify tx . (')':)) rt txs
  . (");}\n"++)
  where
  txs = go 0 ourType
  go n = \case
    TAp (TAp (TC "->") t) rest -> (t, ('x':) . shows n) : go (n + 1) rest
    _ -> []
  rt = ("root["++) . shows n . ("]"++)
  declare (t, x) = (cTypeName t ++) . (' ':) . x

heapify t x = case t of
  TC "Word64" -> ("app(app(_V, app(_NUM,"++) . x . (")),app(_NUM,"++) . x . (" >> 32))"++)
  TC "Char" -> num
  TC "Int" -> num
  TC "Word" -> num
  _ -> x
  where
  num = ("app(_NUM,"++) . x . (')':)

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

comb = (,) <$> conId <*> ((,) <$> many varId <*> (res "=" *> combExpr))
combExpr = foldl1 A <$> some
  (V <$> varId <|> E . StrCon <$> lexeme tokStr <|> paren combExpr)
comdefs = case parse (lexemePrelude *> braceSep comb <* eof) comdefsrc of
  Left e -> error e
  Right (cs, _) -> cs
comEnum s = maybe (error s) id $ lookup s $ zip (fst <$> comdefs) [1..]
comName i = maybe undefined id $ lookup i $ zip [1..] (fst <$> comdefs)

runFun opts ffis = ("enum{_UNDEFINED=0,"++)
  . foldr (.) id (map (\(s, _) -> ('_':) . (s++) . (',':)) comdefs)
  . ("};\n"++)
  . ([r|#define EXPORT(f, sym) void f() asm(sym) __attribute__((export_name(sym)));
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
  if (x == _NUM64) {
    hp += 2;
    altmem[z + 2] = mem[n + 2];
    altmem[z + 3] = mem[n + 3];
  }
  return z;
}

static u gc() {
  hp = 128;
  u di = hp;
  sp = altmem + TOP - 1;
  for(u *r = root; r != rootend; r++) *r = evac(*r);
  *sp = evac(*spTop);
  while (di < hp) {
    u x = altmem[di] = evac(altmem[di]);
    di++;
    if (x == _NUM64) di += 2;
    else if (x != _NUM) altmem[di] = evac(altmem[di]);
    di++;
  }
  spTop = sp;
  u *tmp = mem;
  mem = altmem;
  altmem = tmp;
  for(u usage = 0;; usage++) {
    u x = *sp;
    if (isAddr(x)) *--sp = mem[x]; else return usage + hp + 8 >= TOP;
  }
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
static inline u app64d(double d) {
  mem[hp] = _NUM64;
  mem[hp+1] = 0;
  *((uu*) (mem + hp + 2)) = *((uu*) &d);
  return (hp += 4) - 4;
}
static inline double flo(u n) { return *((double*) (mem + arg(n) + 2)); }
static inline void lazyDub(uu n) { lazy3(4, _V, app(_NUM, n), app(_NUM, n >> 32)); }
static inline uu dub(u lo, u hi) { return ((uu)num(hi) << 32) + (u)num(lo); }
static int div(int a, int b) { int q = a/b; return q - (((u)(a^b)) >> 31)*(q*b!=a); }
static int mod(int a, int b) { int r = a%b; return r + (((u)(a^b)) >> 31)*(!!r)*b; }
static inline u tagcheck(u x) { return isAddr(x) ? x - 128 + hp : x; }
void vmheap(u *start) {
  // TODO: What if there is insufficient heap?
  u *heapptr = mem + hp;
  u *p = start;
  while (p != scratchpadend) {
    u x = *p++;
    *heapptr++ = x == _UNDEFINED ? vmroot[*p++] : tagcheck(x);
    u y = *p++;
    if (x == _NUM) {
      *heapptr++ = y;
    } else {
      *heapptr++ = y == _UNDEFINED ? vmroot[*p++] : tagcheck(y);
    }
  }
  hp = heapptr - mem;
}
void vmrun() {
  u x = tagcheck(num(1));
  vmheap(scratchpad);
  scratchpadend = scratchpad;
  lazy2(2, x, arg(2));
}
void vmgcroot() {
  u lim = num(1);
  u *p = scratchpad;
  while (lim--) {
    u x = *p++;
    *rootend++ = x == _UNDEFINED ? vmroot[*p++] : tagcheck(x);
  }
  vmheap(p);
  scratchpadend = scratchpad;
  lazy2(3, app(arg(3), _K), arg(2));
}
|]++)
    . foldr (.) id (ffiDeclare opts <$> ffis)
    . ("static void foreign(u n) {\n  switch(n) {\n" ++)
    . foldr (.) id (zipWith ffiDefine [0..] ffis)
    . ("\n  }\n}\n" ++)
    . ([r|static void run() {
  for(;;) {
    if (mem + hp > sp - 8 && gc()) return;
    u x = *sp;
    if (isAddr(x)) *--sp = mem[x]; else switch(x) {
|]++)
  . foldr (.) id (genComb <$> comdefs)
  . ([r|
    }
  }
}
|]++)
  . rtsInit opts
  . rtsReduce opts

rtsInit opts
  | "warts" `elem` opts = ([r|void rts_init() {
  mem = (u*) HEAP_BASE;
  altmem = mem + TOP - 128;
  hp = 128 + mem[127];
  spTop = mem + TOP - 1;
  for (rootend = root; *rootend; rootend++);
  vmroot = rootend;
  scratchpadend = scratchpad = altmem + TOP;
}
|]++)
  | otherwise = ([r|void rts_init() {
  mem = malloc(TOP * sizeof(u)); altmem = malloc(TOP * sizeof(u));
  hp = 128;
  for (u i = 0; i < sizeof(prog)/sizeof(*prog); i++) mem[hp++] = prog[i];
  spTop = mem + TOP - 1;
  vmroot = rootend;
}
|]++)

rtsReduce opts =
  (if "pre-post-run" `elem` opts then ("void pre_run(void); void post_run(void);\n"++) else id)
  . (if "warts" `elem` opts then ([r|void rts_reduce(u) __attribute__((export_name("reduce")));
|]++) else id)
  . ([r|
void rts_reduce(u n) {
  static u ready;if (!ready){ready=1;rts_init();}
  *(sp = spTop) = app(app(n, _UNDEFINED), _END);
|]++)
  . (if "pre-post-run" `elem` opts then ("pre_run();run();post_run();"++) else ("run();"++))
  . ("\n}\n"++)

-- Hash consing.
instance (Ord a, Ord b) => Ord (Either a b) where
  x <= y = case x of
    Left a -> case y of
      Left b -> a <= b
      Right _ -> True
    Right a -> case y of
      Left _ -> False
      Right b -> a <= b
  compare x y = case x of
    Left a -> case y of
      Left b -> compare a b
      Right _ -> LT
    Right a -> case y of
      Left _ -> GT
      Right b -> compare a b

memget k@(a, b) = get >>= \(tab, (hp, f)) -> case mlookup k tab of
  Nothing -> put (insert k hp tab, (hp + 2, f . (a:) . (b:))) >> pure hp
  Just v -> pure v

-- Parser only supports nonnegative integer literals, hence sign is always `True`.
integerify x = integerSignList x \True xs ->
  Nd (Nd (Lf $ Link "Base" "Integer") (Lf $ Link "#" "True")) $
    foldr (\h t -> Nd (Nd (lf "CONS") (Lf $ ChrCon $ chr $ intFromWord h)) t) (lf "K") xs

enc t = case t of
  Lf n -> case n of
    Basic c -> pure $ Right $ comEnum c
    Const x -> enc $ integerify x
    ChrCon c -> Right <$> memget (Right $ comEnum "NUM", Right $ ord c)
    StrCon s -> enc $ foldr (\h t -> Nd (Nd (lf "CONS") (Lf $ ChrCon h)) t) (lf "K") s
    Link m s -> pure $ Left (m, s)
    _ -> error $ "BUG! " ++ show t
  LfVar s -> pure $ Left ("", s)
  Nd x y -> enc x >>= \hx -> enc y >>= \hy -> Right <$> memget (hx, hy)

asm combs = foldM
  (\symtab (s, t) -> (flip (insert s) symtab) <$> enc t)
  Tip combs

rewriteCombs tab = optim . go where
  go = \case
    LfVar v -> let t = follow [v] v in case t of
      Lf (Basic _) -> t
      LfVar w -> if v == w then Nd (lf "Y") (lf "I") else t
      _ -> LfVar v
    Nd a b -> Nd (go a) (go b)
    t -> t
  follow seen v = case tab ! v of
    LfVar w | w `elem` seen -> LfVar $ last seen
            | True -> follow (w:seen) w
    t -> t

freeCount v expr = case expr of
  E _ -> 0
  V s -> if s == v then 1 else 0
  A x y -> freeCount v x + freeCount v y
  L w t -> if v == w then 0 else freeCount v t
app01 s x = case freeCount s x of
  0 -> const x
  1 -> flip (fill s) x
  _ -> A $ L s x
optiApp t = case t of
  A x y -> case optiApp x of
    L s x -> app01 s x (optiApp y)
    x -> A x (optiApp y)
  L s x -> L s (optiApp x)
  _ -> t
slideY name orig = stripArgs id orig where
  stripArgs f = \case
    L s x | s /= name -> stripArgs (f . (s:)) x
    t -> measure (f []) t
  measure args t = case args of
    [] -> orig
    _ -> case seek [] t of
      Just n | n > 0 -> let
        (eaten, rest) = splitAt n args
        in foldr L (A (E $ Basic "Y") $ L name $ foldr L (either (error "BUG!") id $ snip n t) rest) eaten
      _ -> orig
    where
    seek spine t = case t of
      V v | v == name -> Just $ length $ takeWhile match $ zip args spine
      A x y -> seek (t:spine) x `fight` seek [] y
      L s x | s /= name -> seek [] x
      _ -> Nothing
    snip n t = case t of
      V v | v == name -> Left 1
      A x y -> case snip n x of
        Left k | k == n -> Right $ V name
               | True -> Left (k + 1)
        Right x' -> A x' <$> snip n y
      L s x | s /= name -> L s <$> snip n x
      _ -> Right t

  match = \case
    (s, A _ (V v)) -> s == v
    _ -> False
  fight x y = maybe x (\n -> Just $ maybe n (min n) x) y

inlineLone objs expr = go expr where
  go = \case
    E (Link m s) | m /= "{foreign}", Right i <- _syms (objs ! m) ! s, i < 128 -> E (Basic $ comName i)
    A x y -> A (go x) (go y)
    L w t -> L w (go t)
    t -> t

getIOType (Qual [] (TAp (TC "IO") t)) = Right t
getIOType q = Left $ "main : " ++ show q

libcWarts = ([r|#define IMPORT(m,n) __attribute__((import_module(m))) __attribute__((import_name(n)));
enum {
  ROOT_BASE = 1<<9,  // 0-terminated array of exported functions
  // HEAP_BASE - 4: program size
  HEAP_BASE = (1<<20) - 128 * sizeof(u),  // program
  TOP = 1<<22
};
static u *root = (u*) ROOT_BASE, *vmroot, *rootend, *scratchpad, *scratchpadend;
void errchar(int c) {}
void errexit() {}
|]++)

allFFIs s =
  fromList . concatMap (toAscList . ffiImports) . elems <$> singleFile s

warts opts ffis =
  ("typedef unsigned u;\n"++)
  . libcWarts
  . runFun opts (toAscList ffis)
  $ ""

topoModules tab = reverse <$> go [] [] (keys tab) where
  go todo done = \case
    [] -> Right done
    x:xt
      | x `elem` map fst done -> go todo done xt
      | x `elem` todo -> Left $ "cycle: " ++ x
      | True -> do
        neat <- maybe (Left $ "missing module: " ++ x) pure $ mlookup x tab
        done <- go (x:todo) done $ dependentModules neat
        go todo ((x, neat):done) xt

data Module = Module
  { _neat :: Neat
  , _syms :: Map String (Either (String, String) Int)
  , _mem :: [Either (String, String) Int]
  }

data Layout = Layout
  { _offsets :: Map String Int
  , _memFun :: [Int] -> [Int]
  , _hp :: Int
  , _ffes :: Map String (Int, Type)
  }
layoutNew = Layout Tip id 0 Tip

agglomerate ffiMap objs lout name = lout
  { _offsets = insert name off $ _offsets lout
  , _ffes = ffes
  , _memFun = _memFun lout . resolve (_mem ob)
  , _hp = off + length (_mem ob)
  }
  where
  ffes = foldr (\(expName, v) m -> insertWith (error $ "duplicate export: " ++ expName) expName v m) (_ffes lout)
    [ (expName, (addr, mustType ourName))
    | (expName, ourName) <- toAscList $ ffiExports neat
    , let addr = adj $ _syms ob ! ourName
    ]
  mustType s = case mlookup s $ typedAsts neat of
    Just (Qual [] t, _) -> t
    _ -> error $ "TODO: bad export: " ++ s
  off = _hp lout
  ob = objs ! name
  neat = _neat ob
  adj = \case
    Right n -> if n < 128 then n else n + off
    Left global -> follow global
  follow (m, s) = case _syms (objs ! m) ! s of
    Right n -> if n < 128 then n else n + (_offsets lout ! m)
    Left global -> follow global
  resolve (l:r:rest) = case l of
    Right c | c == comEnum "NUM" -> (c:) . (either ((ffiMap !) . snd) id r:) . resolve rest
    _ -> (adj l:) . (adj r:) . resolve rest
  resolve [] = id

compile topSize libc opts s = do
  tab <- insert "#" neatPrim <$> singleFile s
  ms <- topoModules tab
  objs <- foldM compileModule Tip ms
  let
    ffis = foldr (\(k, v) m -> insertWith (error $ "duplicate import: " ++ k) k v m) Tip $ concatMap (toAscList . ffiImports) $ elems tab
    ffiMap = fromList $ zip (keys ffis) [0..]
    lout = foldl (agglomerate ffiMap objs) layoutNew $ fst <$> ms
    mem = _memFun lout []
    ffes = _ffes lout
    mayMain = do
      mainOff <- mlookup "Main" $ _offsets lout
      Right n <- mlookup "main" $ _syms $ objs ! "Main"
      mainType <- fst <$> mlookup "main" (typedAsts $ _neat $ objs ! "Main")
      pure (mainOff + n, mainType)
  mainStr <- case mayMain of
    Nothing -> pure ""
    Just (a, q) -> do
      getIOType q
      pure $ if "no-main" `elem` opts then "" else "int main(int argc,char**argv){env_argc=argc;env_argv=argv;rts_reduce(" ++ shows a ");return 0;}\n"
  pure
    $ ("typedef unsigned u;\n"++)
    . ("enum{TOP="++) . (topSize++) . ("};\n"++)
    . ("static const u prog[]={" ++)
    . foldr (.) id (map (\n -> shows n . (',':)) mem)
    . ("};\nstatic u root[1<<16]={" ++)
    . foldr (.) id (map (\(addr, _) -> shows addr . (',':)) $ elems ffes)
    . ("}, *rootend = root + " ++) . shows (size ffes) . (", *vmroot;\n" ++)
    . ("u scratchpad[1<<20], *scratchpadend = scratchpad;\n" ++)
    . (libc++)
    . runFun opts (toAscList ffis)
    . foldr (.) id (zipWith (\(expName, (_, ourType)) n -> ("EXPORT(f"++) . shows n . (", \""++) . (expName++) . ("\")\n"++) . genExport ourType n) (toAscList ffes) [0..])
    $ mainStr

combTyped objs typed = rewriteCombs rawCombs <$> rawCombs where
  slid = mapWithKey (\k (_, t) -> slideY k $ optiApp t) typed
  rawCombs = optim . nolam . inlineLone objs <$> slid

compileModule objs (name, neat) = do
  let
    searcher = searcherNew name (_neat <$> objs) neat
    typed = typedAsts neat
  depdefs <- mapM (\(s, t) -> (s,) <$> patternCompile searcher t) $ topDefs neat
  typed <- inferDefs searcher depdefs (topDecls neat) typed
  typed <- inferTypeclasses searcher (instances neat) typed
  let
    combs = combTyped objs typed
    (symtab, (_, (_, memF))) = runState (asm $ toAscList combs) (Tip, (128, id))
    localmap = resolveLocal <$> symtab
    mem = resolveLocal <$> memF []
    resolveLocal = \case
      Left ("", s) -> resolveLocal $ symtab ! s
      x -> x
  Right $ insert name (Module (neat { typedAsts = typed }) localmap mem) objs

data MatExpr = Mat [[Bool]] | MatFree IntTree | MatExpr :@ MatExpr
instance Show MatExpr where
  showsPrec p = \case
    Mat xs -> case xs of
      [] -> ('I':)
      _ -> ('[':) . foldr (.) (']':) (intersperse (',':) $ foldr (.) id . map (shows . fromEnum) <$> xs)
    MatFree s -> (if p > 0 then (' ':) else id) . shows s
    t :@ u -> showParen (p > 0) $ showsPrec 0 t . showsPrec 1 u

matrixComb = snd . matrixOpt . debruijn []

matrixOpt = \case
  Ze -> ([True], Mat [])
  Su e -> first (False:) $ matrixOpt e
  La e -> sledL 1 e
  App e1 e2 -> matrixOpt e1 ## matrixOpt e2
  Pass s -> ([], MatFree s)
  where
  sledL n = \case
    La e -> sledL (n + 1) e
    e -> let
      (g, d) = matrixOpt e
      present = reverse $ take n (g ++ repeat False)
      in (if and present then id else (([], Mat [present]) ##)) (drop n g, d)

  ([], d1) ## ([], d2) = ([], d1 :@ d2)
  (g1, d1) ## (g2, d2)
    | Mat [] <- d1, Mat [] <- d2 = \cases
      | not $ or $ last p1 : init p2 -> go $ Mat []
      | True:False:t1 <- p1, False:True:t2 <- p2 -> go $ Mat [t1, t2]
      | otherwise -> common
    | Mat [] <- d1, not $ or p1 = go d2
    | otherwise = common
    where
    x = Mat [p1, p2]
    common = go $ x :@ d1 :@ d2
    zs = zipWithDefault False (,) g1 g2
    go = (uncurry (||) <$> zs,) . etaRight
    (p1, p2) = unzip $ reverse $ filter (uncurry (||)) zs
    etaRight (Mat [False:t1, True:t2] :@ d :@ Mat []) = etaRight $ Mat [t1, t2] :@ d
    etaRight d = d

zipWithDefault d f     []     ys = f d <$> ys
zipWithDefault d f     xs     [] = flip f d <$> xs
zipWithDefault d f (x:xt) (y:yt) = f x y : zipWithDefault d f xt yt
