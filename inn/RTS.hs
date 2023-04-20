module RTS where

import Base
import Ast
import Kiselyov
import Map
import Parser

import_qq_here = import_qq_here

libc = [r|#include<stdio.h>
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

preamble = [r|#define EXPORT(f, sym, n) void f() asm(sym) __attribute__((export_name(sym))); void f(){rts_reduce(root[n]);}
void *malloc(unsigned long);
enum { FORWARD = 127, REDUCING = 126 };
enum { TOP = 1<<24 };
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
    if (x != _NUM) altmem[di] = evac(altmem[di]);
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
|]

-- Main VM loop.
comdefsrc = [r|
F x = "foreign(num(1));"
Y x = x "sp[1]"
Q x y z = z(y x)
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
ADD x y = "_NUM" "num(1) + num(2)"
SUB x y = "_NUM" "num(1) - num(2)"
MUL x y = "_NUM" "num(1) * num(2)"
DIV x y = "_NUM" "num(1) / num(2)"
MOD x y = "_NUM" "num(1) % num(2)"
EQ x y = "num(1) == num(2) ? lazy2(2, _I, _K) : lazy2(2, _K, _I);"
LE x y = "num(1) <= num(2) ? lazy2(2, _I, _K) : lazy2(2, _K, _I);"
REF x y = y "sp[1]"
NEWREF x y z = z ("_REF" x) y
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

cTypeName (TC "()") = "void"
cTypeName (TC "Int") = "int"
cTypeName (TC "Char") = "int"

ffiDeclare (name, t) = let tys = argList t in concat
  [cTypeName $ last tys, " ", name, "(", intercalate "," $ cTypeName <$> init tys, ");\n"]

ffiArgs n t = case t of
  TC s -> ("", ((True, s), n))
  TAp (TC "IO") (TC u) -> ("", ((False, u), n))
  TAp (TAp (TC "->") x) y -> first (((if 3 <= n then ", " else "") ++ "num(" ++ shows n ")") ++) $ ffiArgs (n + 1) y

ffiDefine n ffis = case ffis of
  [] -> id
  (name, t):xt -> let
    (args, ((isPure, ret), count)) = ffiArgs 2 t
    lazyn = ("lazy2(" ++) . shows (if isPure then count - 1 else count + 1) . (", " ++)
    cont tgt = if isPure then ("_I, "++) . tgt else ("app(arg("++) . shows (count + 1) . ("), "++) . tgt . ("), arg("++) . shows count . (")"++)
    longDistanceCall = (name++) . ("("++) . (args++) . ("); "++) . lazyn
    in ("case " ++) . shows n . (": " ++) . if ret == "()"
      then longDistanceCall . cont ("_K"++) . ("); break;"++) . ffiDefine (n - 1) xt
      else ("{u r = "++) . longDistanceCall . cont ("app(_NUM, r)" ++) . ("); break;}\n"++) . ffiDefine (n - 1) xt

genMain n = "int main(int argc,char**argv){env_argc=argc;env_argv=argv;rts_reduce(" ++ shows n ");return 0;}\n"

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

comb = (,) <$> conId <*> ((,) <$> many varId <*> (res "=" *> combExpr))
combExpr = foldl1 A <$> some
  (V <$> varId <|> E . StrCon <$> lexeme tokStr <|> paren combExpr)
comdefs = case parse (lexemePrelude *> braceSep comb <* eof) comdefsrc of
  Left e -> error e
  Right (cs, _) -> cs
comEnum s = maybe (error s) id $ lookup s $ zip (fst <$> comdefs) [1..]
comName i = maybe undefined id $ lookup i $ zip [1..] (fst <$> comdefs)

runFun = ([r|static void run() {
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

void rts_init() {
  mem = malloc(TOP * sizeof(u)); altmem = malloc(TOP * sizeof(u));
  hp = 128;
  for (u i = 0; i < sizeof(prog)/sizeof(*prog); i++) mem[hp++] = prog[i];
  spTop = mem + TOP - 1;
}

void rts_reduce(u n) {
  static u ready;if (!ready){ready=1;rts_init();}
  *(sp = spTop) = app(app(n, _UNDEFINED), _END);
  run();
}
|]++)

resolve bigmap (m, s) = either (resolve bigmap) id $ (bigmap ! m) ! s

mayResolve bigmap (m, s) = mlookup m bigmap
  >>= fmap (either (resolve bigmap) id) . mlookup s

appCell (hp, bs) x y = (Right hp, (hp + 2, bs . (x:) . (y:)))

enc tab mem = \case
  Lf n -> case n of
    Basic c -> (Right $ comEnum c, mem)
    Const c -> appCell mem (Right $ comEnum "NUM") $ Right c
    ChrCon c -> appCell mem (Right $ comEnum "NUM") $ Right $ ord c
    StrCon s -> enc tab mem $ foldr (\h t -> Nd (Nd (lf "CONS") (Lf $ ChrCon h)) t) (lf "K") s
    Link m s _ -> (Left (m, s), mem)
  LfVar s -> maybe (error $ "resolve " ++ s) (, mem) $ mlookup s tab
  Nd x y -> let
    (xAddr, mem') = enc tab mem x
    (yAddr, mem'') = enc tab mem' y
    in appCell mem'' xAddr yAddr

asm hp0 combs = tabmem where
  tabmem = foldl (\(as, m) (s, t) -> let (p, m') = enc (fst tabmem) m t
    in (insert s p as, m')) (Tip, (hp0, id)) combs

optiComb' (subs, combs) (s, lamb) = let
  gosub t = case t of
    LfVar v -> maybe t id $ lookup v subs
    Nd a b -> Nd (gosub a) (gosub b)
    _ -> t
  c = optim $ gosub $ nolam lamb
  combs' = combs . ((s, c):)
  in case c of
    Lf (Basic _) -> ((s, c):subs, combs')
    LfVar v -> if v == s then (subs, combs . ((s, Nd (lf "Y") (lf "I")):)) else ((s, gosub c):subs, combs')
    _ -> (subs, combs')
optiComb lambs = ($[]) . snd $ foldl optiComb' ([], id) lambs

lambsList typed = toAscList $ snd <$> typed

codegenLocal (name, (typed, _)) (bigmap, (hp, f)) =
  (insert name localmap bigmap, (hp', f . memF))
  where
  (localmap, (hp', memF)) = asm hp $ optiComb $ lambsList typed

codegen mods = (bigmap, mem) where
  (bigmap, (_, memF)) = foldr codegenLocal (Tip, (128, id)) $ toAscList mods
  mem = either (resolve bigmap) id <$> memF []

getIOType (Qual [] (TAp (TC "IO") t)) = Right t
getIOType q = Left $ "main : " ++ shows q ""

ffcat (name, (_, (ffis, ffes))) (xs, ys) = (ffis ++ xs, ((name,) <$> ffes) ++ ys)

compile mods = do
  let
    (bigmap, mem) = codegen mods
    (ffis, ffes) = foldr ffcat ([], []) $ toAscList mods
    mustType modName s = case mlookup s (fst $ mods ! modName) of
      Just (Qual [] t, _) -> t
      _ -> error "TODO: report bad exports"
    mayMain = do
      mainAddr <- mayResolve bigmap ("Main", "main")
      (mainType, _) <- mlookup "main" $ fst $ mods ! "Main"
      pure (mainAddr, mainType)
  mainStr <- case mayMain of
    Nothing -> pure ""
    Just (a, q) -> do
      getIOType q
      pure $ genMain a

  pure
    $ ("typedef unsigned u;\n"++)
    . ("enum{_UNDEFINED=0,"++)
    . foldr (.) id (map (\(s, _) -> ('_':) . (s++) . (',':)) comdefs)
    . ("};\n"++)
    . ("static const u prog[]={" ++)
    . foldr (.) id (map (\n -> shows n . (',':)) mem)
    . ("};\nstatic u root[]={" ++)
    . foldr (\(modName, (_, ourName)) f -> shows (resolve bigmap (modName, ourName)) . (", " ++) . f) id ffes
    . ("0};\n" ++)
    . (preamble++)
    . (libc++)
    . (concatMap ffiDeclare ffis ++)
    . ("static void foreign(u n) {\n  switch(n) {\n" ++)
    . ffiDefine (length ffis - 1) ffis
    . ("\n  }\n}\n" ++)
    . runFun
    . foldr (.) id (zipWith (\(modName, (expName, ourName))  n -> ("EXPORT(f"++) . shows n . (", \""++) . (expName++) . ("\")\n"++)
      . genExport (arrCount $ mustType modName ourName) n) ffes [0..])
    $ mainStr
