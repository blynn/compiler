-- Default class methods.
-- Accept instance methods in any order.
infixr 9 .
infixl 7 * , `div` , `mod`
infixl 6 + , -
infixr 5 ++
infixl 4 <*> , <$> , <* , *>
infix 4 == , /= , <=
infixl 3 && , <|>
infixl 2 ||
infixl 1 >> , >>=
infixr 0 $

foreign import ccall "putchar" putChar :: Int -> IO Int
foreign import ccall "getchar" getChar :: IO Int
foreign import ccall "getargcount" getArgCount :: IO Int
foreign import ccall "getargchar" getArgChar :: Int -> Int -> IO Char

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

class Functor f where fmap :: (a -> b) -> f a -> f b
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
(<$>) = fmap
liftA2 f x y = f <$> x <*> y
(>>) f g = f >>= \_ -> g
class Eq a where (==) :: a -> a -> Bool
instance Eq Int where (==) = intEq
instance Eq Char where (==) = charEq
($) f x = f x
id x = x
const x y = x
flip f x y = f y x
(&) x f = f x
class Ord a where (<=) :: a -> a -> Bool
compare x y = if x <= y then if y <= x then EQ else LT else GT
instance Ord Int where (<=) = intLE
instance Ord Char where (<=) = charLE
data Ordering = LT | GT | EQ
instance Ord a => Ord [a] where
  xs <= ys = case xs of
    [] -> True
    x:xt -> case ys of
      [] -> False
      y:yt -> case compare x y of
        LT -> True
        GT -> False
        EQ -> xt <= yt
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
fst (x, y) = x
snd (x, y) = y
uncurry f (x, y) = f x y
first f (x, y) = (f x, y)
second f (x, y) = (x, f y)
not a = if a then False else True
x /= y = not $ x == y
(.) f g x = f (g x)
(||) f g = if f then True else g
(&&) f g = if f then g else False
instance Eq a => Eq [a] where
  xs == ys = case xs of
    [] -> case ys of
      [] -> True
      _ -> False
    x:xt -> case ys of
      [] -> False
      y:yt -> x == y && xt == yt
take 0 xs = []
take _ [] = []
take n (h:t) = h : take (n - 1) t
maybe n j m = case m of Nothing -> n; Just x -> j x
instance Functor Maybe where fmap f = maybe Nothing (Just . f)
instance Applicative Maybe where pure = Just ; mf <*> mx = maybe Nothing (\f -> maybe Nothing (Just . f) mx) mf
instance Monad Maybe where return = Just ; mf >>= mg = maybe Nothing mg mf
instance Alternative Maybe where empty = Nothing ; x <|> y = maybe y Just x
foldr c n = \case [] -> n; h:t -> c h $ foldr c n t
length = foldr (\_ n -> n + 1) 0
mapM f = foldr (\a rest -> liftA2 (:) (f a) rest) (pure [])
mapM_ f = foldr ((>>) . f) (pure ())
foldM f z0 xs = foldr (\x k z -> f z x >>= k) pure xs z0
instance Applicative IO where pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y)
instance Monad IO where return = ioPure ; (>>=) = ioBind
instance Functor IO where fmap f x = ioPure f <*> x
class Show a where showsPrec :: Int -> a -> String -> String
shows = showsPrec 0
show x = shows x ""
showInt__ n
  | 0 == n = id
  | True = showInt__ (n`div`10) . (chr (48+n`mod`10):)
instance Show Int where
  showsPrec _ n
    | 0 == n = ('0':)
    | 1 <= n = showInt__ n
    | 2 * n == 0 = ("-2147483648"++)
    | True = ('-':) . showInt__ (0 - n)
putStr = mapM_ $ putChar . ord
getContents = getChar >>= \n -> if 0 <= n then (chr n:) <$> getContents else pure []
interact f = getContents >>= putStr . f
error s = unsafePerformIO $ putStr s >> putChar (ord '\n') >> exitSuccess
undefined = error "undefined"
foldr1 c l@(h:t) = maybe undefined id $ foldr (\x m -> Just $ maybe x (c x) m) Nothing l
foldl f a bs = foldr (\b g x -> g (f x b)) (\x -> x) bs a
foldl1 f (h:t) = foldl f h t
elem k xs = foldr (\x t -> x == k || t) False xs
find f xs = foldr (\x t -> if f x then Just x else t) Nothing xs
(++) = flip (foldr (:))
concat = foldr (++) []
map = flip (foldr . ((:) .)) []
head (h:_) = h
tail (_:t) = t
isSpace c = elem (ord c) [32, 9, 10, 11, 12, 13, 160]
instance Functor [] where fmap = map
concatMap = (concat .) . map
lookup s = foldr (\(k, v) t -> if s == k then Just v else t) Nothing
filter f = foldr (\x xs -> if f x then x:xs else xs) []
union xs ys = foldr (\y acc -> (if elem y acc then id else (y:)) acc) xs ys
intersect xs ys = filter (\x -> maybe False (\_ -> True) $ find (x ==) ys) xs
last (x:xt) = go x xt where go x xt = case xt of [] -> x; y:yt -> go y yt
init (x:xt) = case xt of [] -> []; _ -> x : init xt
intercalate sep = \case [] -> []; x:xt -> x ++ concatMap (sep ++) xt
intersperse sep = \case [] -> []; x:xt -> x : foldr ($) [] (((sep:) .) . (:) <$> xt)
all f = foldr (&&) True . map f
any f = foldr (||) False . map f
and = foldr (&&) True
or = foldr (||) False
upFrom n = n : upFrom (n + 1)
zipWith f xs ys = case xs of [] -> []; x:xt -> case ys of [] -> []; y:yt -> f x y : zipWith f xt yt
zip = zipWith (,)
data State s a = State (s -> (a, s))
runState (State f) = f
instance Functor (State s) where fmap f = \(State h) -> State (first f . h)
instance Applicative (State s) where
  pure a = State (a,)
  (State f) <*> (State x) = State \s -> case f s of (g, s') -> first g $ x s'
instance Monad (State s) where
  return a = State (a,)
  (State h) >>= f = State $ uncurry (runState . f) . h
evalState m s = fst $ runState m s
get = State \s -> (s, s)
put n = State \s -> ((), n)
either l r e = case e of Left x -> l x; Right x -> r x
instance Functor (Either a) where fmap f e = either Left (Right . f) e
instance Applicative (Either a) where
  pure = Right
  ef <*> ex = case ef of
    Left s -> Left s
    Right f -> either Left (Right . f) ex
instance Monad (Either a) where
  return = Right
  ex >>= f = either Left f ex
class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
asum = foldr (<|>) empty
(*>) = liftA2 \x y -> y
(<*) = liftA2 \x y -> x
many p = liftA2 (:) p (many p) <|> pure []
some p = liftA2 (:) p (many p)
sepBy1 p sep = liftA2 (:) p (many (sep *> p))
sepBy p sep = sepBy1 p sep <|> pure []
between x y p = x *> (p <* y)

-- Map.
data Map k a = Tip | Bin Int k a (Map k a) (Map k a)
size m = case m of Tip -> 0 ; Bin sz _ _ _ _ -> sz
node k x l r = Bin (1 + size l + size r) k x l r
singleton k x = Bin 1 k x Tip Tip
singleL k x l (Bin _ rk rkx rl rr) = node rk rkx (node k x l rl) rr
doubleL k x l (Bin _ rk rkx (Bin _ rlk rlkx rll rlr) rr) =
  node rlk rlkx (node k x l rll) (node rk rkx rlr rr)
singleR k x (Bin _ lk lkx ll lr) r = node lk lkx ll (node k x lr r)
doubleR k x (Bin _ lk lkx ll (Bin _ lrk lrkx lrl lrr)) r =
  node lrk lrkx (node lk lkx ll lrl) (node k x lrr r)
balance k x l r = f k x l r where
  f | size l + size r <= 1 = node
    | 5 * size l + 3 <= 2 * size r = case r of
      Tip -> node
      Bin sz _ _ rl rr -> if 2 * size rl + 1 <= 3 * size rr
        then singleL
        else doubleL
    | 5 * size r + 3 <= 2 * size l = case l of
      Tip -> node
      Bin sz _ _ ll lr -> if 2 * size lr + 1 <= 3 * size ll
        then singleR
        else doubleR
    | True = node
insert kx x t = case t of
  Tip -> singleton kx x
  Bin sz ky y l r -> case compare kx ky of
    LT -> balance ky y (insert kx x l) r
    GT -> balance ky y l (insert kx x r)
    EQ -> Bin sz kx x l r
insertWith f kx x t = case t of
  Tip -> singleton kx x
  Bin sy ky y l r -> case compare kx ky of
    LT -> balance ky y (insertWith f kx x l) r
    GT -> balance ky y l (insertWith f kx x r)
    EQ -> Bin sy kx (f x y) l r
mlookup kx t = case t of
  Tip -> Nothing
  Bin _ ky y l r -> case compare kx ky of
    LT -> mlookup kx l
    GT -> mlookup kx r
    EQ -> Just y
fromList = foldl (\t (k, x) -> insert k x t) Tip
member k t = maybe False (const True) $ mlookup k t
t ! k = maybe undefined id $ mlookup k t

foldrWithKey f = go where
  go z t = case t of
    Tip -> z
    Bin _ kx x l r -> go (f kx x (go z r)) l

toAscList = foldrWithKey (\k x xs -> (k,x):xs) []

-- Syntax tree.
data Type = TC String | TV String | TAp Type Type
arr a b = TAp (TAp (TC "->") a) b
data Extra = Basic String | Const Int | ChrCon Char | StrCon String
data Pat = PatLit Ast | PatVar String (Maybe Pat) | PatCon String [Pat]
data Ast = E Extra | V String | A Ast Ast | L String Ast | Pa [([Pat], Ast)] | Proof Pred
data Constr = Constr String [Type]
data Pred = Pred String Type
data Qual = Qual [Pred] Type
noQual = Qual []

instance Eq Type where
  (TC s) == (TC t) = s == t
  (TV s) == (TV t) = s == t
  (TAp a b) == (TAp c d) = a == c && b == d
  _ == _ = False

instance Eq Pred where (Pred s a) == (Pred t b) = s == t && a == b

data Instance = Instance
  -- Type, e.g. Int for Eq Int.
  Type
  -- Dictionary name, e.g. "{Eq Int}"
  String
  -- Context.
  [Pred]
  -- Method definitions
  (Map String Ast)

data Tycl = Tycl [String] [Instance]

data Neat = Neat
  (Map String Tycl)
  -- | Top-level definitions
  [(String, Ast)]
  -- | Typed ASTs, ready for compilation, including ADTs and methods,
  -- e.g. (==), (Eq a => a -> a -> Bool, select-==)
  [(String, (Qual, Ast))]
  -- | Data constructor table.
  (Map String [Constr])
  -- | FFI declarations.
  [(String, Type)]
  -- | Exports.
  [(String, String)]

patVars = \case
  PatLit _ -> []
  PatVar s m -> s : maybe [] patVars m
  PatCon _ args -> concat $ patVars <$> args

fv bound = \case
  V s | not (elem s bound) -> [s]
  A x y -> fv bound x `union` fv bound y
  L s t -> fv (s:bound) t
  _ -> []

fvPro bound expr = case expr of
  V s | not (elem s bound) -> [s]
  A x y -> fvPro bound x `union` fvPro bound y
  L s t -> fvPro (s:bound) t
  Pa vsts -> foldr union [] $ map (\(vs, t) -> fvPro (concatMap patVars vs ++ bound) t) vsts
  _ -> []

overFree s f t = case t of
  E _ -> t
  V s' -> if s == s' then f t else t
  A x y -> A (overFree s f x) (overFree s f y)
  L s' t' -> if s == s' then t else L s' $ overFree s f t'

overFreePro s f t = case t of
  E _ -> t
  V s' -> if s == s' then f t else t
  A x y -> A (overFreePro s f x) (overFreePro s f y)
  L s' t' -> if s == s' then t else L s' $ overFreePro s f t'
  Pa vsts -> Pa $ map (\(vs, t) -> (vs, if any (elem s . patVars) vs then t else overFreePro s f t)) vsts

beta s t x = overFree s (const t) x

showParen b f = if b then ('(':) . f . (')':) else f

-- Parser.
data ParserState = ParserState
  [(Char, (Int, Int))]
  String
  [Int]
  (Map String (Int, Assoc))

readme  (ParserState x _ _ _) = x
landin  (ParserState _ x _ _) = x
indents (ParserState _ _ x _) = x
precs   (ParserState _ _ _ x) = x
putReadme  x (ParserState _ a b c) = ParserState x a b c
putLandin  x (ParserState a _ b c) = ParserState a x b c
modIndents f (ParserState a b x c) = ParserState a b (f x) c
data Parser a = Parser (ParserState -> Either String (a, ParserState))
getParser (Parser p) = p
instance Functor Parser where fmap f x = pure f <*> x
instance Applicative Parser where
  pure x = Parser \inp -> Right (x, inp)
  (Parser f) <*> (Parser x) = Parser \inp -> do
    (fun, t) <- f inp
    (arg, u) <- x t
    pure (fun arg, u)
instance Monad Parser where
  return = pure
  (Parser x) >>= f = Parser \inp -> do
    (a, t) <- x inp
    getParser (f a) t
instance Alternative Parser where
  empty = bad ""
  x <|> y = Parser \inp -> either (const $ getParser y inp) Right $ getParser x inp
getPrecs = Parser \st -> Right (precs st, st)
putPrecs ps = Parser \(ParserState a b c _) -> Right ((), ParserState a b c ps)

notFollowedBy p = do
  saved <- Parser \pasta -> Right (pasta, pasta)
  ret <- p *> pure (bad "") <|> pure (pure ())
  Parser \_ -> Right ((), saved)
  ret

parse f str = getParser f $ ParserState (rowcol str (1, 1)) [] [] $ singleton ":" (5, RAssoc) where
  rowcol s rc = case s of
    [] -> []
    h:t -> (h, rc) : rowcol t (advanceRC (ord h) rc)
  advanceRC n (r, c)
    | n `elem` [10, 11, 12, 13] = (r + 1, 1)
    | n == 9 = (r, (c + 8)`mod`8)
    | True = (r, c + 1)

indentOf pasta = case readme pasta of
  [] -> 1
  (_, (_, c)):_ -> c

ins c pasta = putLandin (c:landin pasta) pasta

angle n pasta = case indents pasta of
  m:ms | m == n -> ins ';' pasta
       | n + 1 <= m -> ins '}' $ angle n $ modIndents tail pasta
  _ -> pasta

curly n pasta = case indents pasta of
  m:ms | m + 1 <= n -> ins '{' $ modIndents (n:) pasta
  [] | 1 <= n -> ins '{' $ modIndents (n:) pasta
  _ -> ins '{' . ins '}' $ angle n pasta

sat f = Parser \pasta -> case landin pasta of
  c:t -> if f c then Right (c, putLandin t pasta) else Left "unsat"
  [] -> case readme pasta of
    [] -> case indents pasta of
      [] -> Left "EOF"
      m:ms | m /= 0 && f '}' -> Right ('}', modIndents tail pasta)
      _ -> Left "unsat"
    (h, _):t | f h -> let
      p' = putReadme t pasta
      in case h of
        '}' -> case indents pasta of
          0:ms -> Right (h, modIndents tail p')
          _ -> Left "unsat"
        '{' -> Right (h, modIndents (0:) p')
        _ -> Right (h, p')
    _ -> Left "unsat"

char c = sat (c ==)

rawSat f = Parser \pasta -> case readme pasta of
  [] -> Left "EOF"
  (h, _):t -> if f h then Right (h, putReadme t pasta) else Left "unsat"

eof = Parser \pasta -> case pasta of
  ParserState [] [] _ _ -> Right ((), pasta)
  _ -> badpos pasta "want eof"

comment = rawSat ('-' ==) *> some (rawSat ('-' ==)) *>
  (rawSat isNewline <|> rawSat (not . isSymbol) *> many (rawSat $ not . isNewline) *> rawSat isNewline) *> pure True
spaces = isNewline <$> rawSat isSpace
whitespace = do
  offside <- or <$> many (spaces <|> comment)
  Parser \pasta -> Right ((), if offside then angle (indentOf pasta) pasta else pasta)

hexValue d
  | d <= '9' = ord d - ord '0'
  | d <= 'F' = 10 + ord d - ord 'A'
  | d <= 'f' = 10 + ord d - ord 'a'
isNewline c = ord c `elem` [10, 11, 12, 13]
isSymbol = (`elem` "!#$%&*+./<=>?@\\^|-~:")
isSmall c = c <= 'z' && 'a' <= c || c == '_'
small = sat isSmall
large = sat \x -> (x <= 'Z') && ('A' <= x)
hexit = sat \x -> (x <= '9') && ('0' <= x)
  || (x <= 'F') && ('A' <= x)
  || (x <= 'f') && ('a' <= x)
digit = sat \x -> (x <= '9') && ('0' <= x)
decimal = foldl (\n d -> 10*n + ord d - ord '0') 0 <$> some digit
hexadecimal = foldl (\n d -> 16*n + hexValue d) 0 <$> some hexit
nameTailChar = small <|> large <|> digit <|> char '\''
nameTailed p = liftA2 (:) p $ many nameTailChar

escape = char '\\' *> (sat (`elem` "'\"\\") <|> char 'n' *> pure '\n' <|> char '0' *> pure (chr 0) <|> char 'x' *> (chr <$> hexadecimal))
tokOne delim = escape <|> rawSat (delim /=)

charSeq = mapM char
tokChar = between (char '\'') (char '\'') (tokOne '\'')
quoteStr = between (char '"') (char '"') $ many $ many (charSeq "\\&") *> tokOne '"'
quasiquoteStr = charSeq "[r|" *> quasiquoteBody
quasiquoteBody = charSeq "|]" *> pure [] <|> (:) <$> rawSat (const True) <*> quasiquoteBody
tokStr = quoteStr <|> quasiquoteStr
integer = char '0' *> (char 'x' <|> char 'X') *> hexadecimal <|> decimal
literal = lexeme . fmap E $ Const <$> integer <|> ChrCon <$> tokChar <|> StrCon <$> tokStr
varish = lexeme $ nameTailed small
bad s = Parser \pasta -> badpos pasta s
badpos pasta s = Left $ loc $ ": " ++ s where
  loc = case readme pasta of
    [] -> ("EOF"++)
    (_, (r, c)):_ -> ("row "++) . shows r . (" col "++) . shows c
varId = do
  s <- varish
  if elem s
    ["export", "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"]
    then bad $ "reserved: " ++ s else pure s
varSymish = lexeme $ (:) <$> sat (\c -> isSymbol c && c /= ':') <*> many (sat isSymbol)
varSym = lexeme $ do
  s <- varSymish
  if elem s ["..", "=", "\\", "|", "<-", "->", "@", "~", "=>"] then bad $ "reserved: " ++ s else pure s

conId = lexeme $ nameTailed large
conSymish = lexeme $ liftA2 (:) (char ':') $ many $ sat isSymbol
conSym = do
  s <- conSymish
  if elem s [":", "::"] then bad $ "reserved: " ++ s else pure s
special c = lexeme $ sat (c ==)
comma = special ','
semicolon = special ';'
lParen = special '('
rParen = special ')'
lBrace = special '{'
rBrace = special '}'
lSquare = special '['
rSquare = special ']'
backquote = special '`'

lexeme f = f <* whitespace

lexemePrelude = whitespace *>
  Parser \pasta -> case getParser (res "module" <|> (:[]) <$> char '{') pasta of
    Left _ -> Right ((), curly (indentOf pasta) pasta)
    Right _ -> Right ((), pasta)

curlyCheck f = do
  Parser \pasta -> Right ((), modIndents (0:) pasta)
  r <- f
  Parser \pasta -> let pasta' = modIndents tail pasta in case readme pasta of
    []              -> Right ((), curly 0 pasta')
    ('{', _):_      -> Right ((), pasta')
    (_, (_, col)):_ -> Right ((), curly col pasta')
  pure r

ro = E . Basic
conOf (Constr s _) = s
specialCase (h:_) = '|':conOf h
mkCase t cs = (specialCase cs,
  ( noQual $ arr t $ foldr arr (TV "case") $ map (\(Constr _ ts) -> foldr arr (TV "case") ts) cs
  , ro "I"))
mkStrs = snd . foldl (\(s, l) u -> ('@':s, s:l)) ("@", [])
scottEncode _ ":" _ = ro "CONS"
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs)
scottConstr t cs (Constr s ts) = (s,
  (noQual $ foldr arr t ts , scottEncode (map conOf cs) s $ mkStrs ts))
mkAdtDefs t cs = mkCase t cs : map (scottConstr t cs) cs

mkFFIHelper n t acc = case t of
  TC s -> acc
  TAp (TC "IO") _ -> acc
  TAp (TAp (TC "->") x) y -> L (show n) $ mkFFIHelper (n + 1) y $ A (V $ show n) acc

updateDcs cs dcs = foldr (\(Constr s _) m -> insert s cs m) dcs cs
addAdt t cs (Neat tycl fs typed dcs ffis exs) =
  Neat tycl fs (mkAdtDefs t cs ++ typed) (updateDcs cs dcs) ffis exs

emptyTycl = Tycl [] []
addClass classId v (sigs, defs) (Neat tycl fs typed dcs ffis ffes) = let
  vars = take (size sigs) $ show <$> upFrom 0
  selectors = zipWith (\var (s, t) -> (s, (Qual [Pred classId v] t,
    L "@" $ A (V "@") $ foldr L (V var) vars))) vars $ toAscList sigs
  defaults = map (\(s, t) -> if member s sigs then ("{default}" ++ s, t) else error $ "bad default method: " ++ s) $ toAscList defs
  Tycl ms is = maybe emptyTycl id $ mlookup classId tycl
  tycl' = insert classId (Tycl (fst <$> toAscList sigs) is) tycl
  in Neat tycl' (defaults ++ fs) (selectors ++ typed) dcs ffis ffes

addInstance classId ps ty ds (Neat tycl fs typed dcs ffis exs) = let
  Tycl ms is = maybe emptyTycl id $ mlookup classId tycl
  tycl' = insert classId (Tycl ms $ Instance ty name ps (fromList ds):is) tycl
  name = '{':classId ++ (' ':shows ty "}")
  in Neat tycl' fs typed dcs ffis exs

addFFI foreignname ourname t (Neat tycl fs typed dcs ffis exs) = let
  fn = A (ro "F") $ E $ Const $ length ffis
  in Neat tycl fs ((ourname, (Qual [] t, mkFFIHelper 0 t fn)) : typed) dcs ((foreignname, t):ffis) exs
addDefs ds (Neat tycl fs typed dcs ffis exs) = Neat tycl (ds ++ fs) typed dcs ffis exs
addExport e f (Neat tycl fs typed dcs ffis exs) = Neat tycl fs typed dcs ffis ((e, f):exs)

parseErrorRule = Parser \pasta -> case indents pasta of
  m:ms | m /= 0 -> Right ('}', modIndents tail pasta)
  _ -> badpos pasta "missing }"

res w@(h:_) = reservedSeq *> pure w <|> bad ("want \"" ++ w ++ "\"") where
  reservedSeq = if elem w ["let", "where", "do", "of"]
    then curlyCheck $ lexeme $ charSeq w *> notFollowedBy nameTailChar
    else lexeme $ charSeq w *> notFollowedBy (if isSmall h then nameTailChar else sat isSymbol)

paren = between lParen rParen
braceSep f = between lBrace (rBrace <|> parseErrorRule) $ foldr ($) [] <$> sepBy ((:) <$> f <|> pure id) semicolon

maybeFix s x = if elem s $ fvPro [] x then A (V "fix") (L s x) else x

nonemptyTails [] = []
nonemptyTails xs@(x:xt) = xs : nonemptyTails xt

joinIsFail t = A (L "join#" t) (V "fail#")

addLets ls x = foldr triangle x components where
  vs = fst <$> ls
  ios = foldr (\(s, dsts) (ins, outs) ->
    (foldr (\dst -> insertWith union dst [s]) ins dsts, insertWith union s dsts outs))
    (Tip, Tip) $ map (\(s, t) -> (s, intersect (fvPro [] t) vs)) ls
  components = scc (\k -> maybe [] id $ mlookup k $ fst ios) (\k -> maybe [] id $ mlookup k $ snd ios) vs
  triangle names expr = let
    tnames = nonemptyTails names
    suball t = foldr (\(x:xt) t -> overFreePro x (const $ foldl (\acc s -> A acc (V s)) (V x) xt) t) t tnames
    insLams vs t = foldr L t vs
    in foldr (\(x:xt) t -> A (L x t) $ maybeFix x $ insLams xt $ suball $ maybe undefined joinIsFail $ lookup x ls) (suball expr) tnames

data Assoc = NAssoc | LAssoc | RAssoc
instance Eq Assoc where
  NAssoc == NAssoc = True
  LAssoc == LAssoc = True
  RAssoc == RAssoc = True
  _ == _ = False
precOf s precTab = maybe 9 fst $ mlookup s precTab
assocOf s precTab = maybe LAssoc snd $ mlookup s precTab

opFold precTab f x xs = case xs of
  [] -> pure x
  (op, y):xt -> case find (\(op', _) -> assocOf op precTab /= assocOf op' precTab) xt of
    Nothing -> case assocOf op precTab of
      NAssoc -> case xt of
        [] -> pure $ f op x y
        y:yt -> bad "NAssoc repeat"
      LAssoc -> pure $ foldl (\a (op, y) -> f op a y) x xs
      RAssoc -> pure $ foldr (\(op, y) b -> \e -> f op e (b y)) id xs $ x
    Just y -> bad "Assoc clash"

qconop = conSym <|> res ":" <|> between backquote backquote conId

qconsym = conSym <|> res ":"

op = qconsym <|> varSym <|> between backquote backquote (conId <|> varId)
con = conId <|> paren qconsym
var = varId <|> paren varSym

tycon = do
  s <- conId
  pure $ if s == "String" then TAp (TC "[]") (TC "Char") else TC s

aType =
  lParen *>
    (   rParen *> pure (TC "()")
    <|> (foldr1 (TAp . TAp (TC ",")) <$> sepBy1 _type comma) <* rParen)
  <|> tycon
  <|> TV <$> varId
  <|> (lSquare *> (rSquare *> pure (TC "[]") <|> TAp (TC "[]") <$> (_type <* rSquare)))
bType = foldl1 TAp <$> some aType
_type = foldr1 arr <$> sepBy bType (res "->")

fixityDecl w a = do
  res w
  n <- lexeme integer
  os <- sepBy op comma
  precs <- getPrecs
  putPrecs $ foldr (\o m -> insert o (n, a) m) precs os
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc

cDecls = first fromList . second fromList . foldr ($) ([], []) <$> braceSep cDecl
cDecl = first . (:) <$> genDecl <|> second . (++) <$> defSemi

genDecl = (,) <$> var <*> (res "::" *> _type)

classDecl = res "class" *> (addClass <$> conId <*> (TV <$> varId) <*> (res "where" *> cDecls))

simpleClass = Pred <$> conId <*> _type
scontext = (:[]) <$> simpleClass <|> paren (sepBy simpleClass comma)

instDecl = res "instance" *>
  ((\ps cl ty defs -> addInstance cl ps ty defs) <$>
  (scontext <* res "=>" <|> pure [])
    <*> conId <*> _type <*> (res "where" *> braceDef))

letin = addLets <$> between (res "let") (res "in") braceDef <*> expr
ifthenelse = (\a b c -> A (A (A (V "if") a) b) c) <$>
  (res "if" *> expr) <*> (res "then" *> expr) <*> (res "else" *> expr)
listify = foldr (\h t -> A (A (V ":") h) t) (V "[]")

alts = joinIsFail . Pa <$> braceSep ((\x y -> ([x], y)) <$> pat <*> guards "->")
cas = flip A <$> between (res "case") (res "of") expr <*> alts
lamCase = curlyCheck (res "case") *> alts
lam = res "\\" *> (lamCase <|> liftA2 onePat (some apat) (res "->" *> expr))

flipPairize y x = A (A (V ",") x) y
moreCommas = foldr1 (A . A (V ",")) <$> sepBy1 expr comma
thenComma = comma *> ((flipPairize <$> moreCommas) <|> pure (A (V ",")))
parenExpr = (&) <$> expr <*> (((\v a -> A (V v) a) <$> op) <|> thenComma <|> pure id)
rightSect = ((\v a -> L "@" $ A (A (V v) $ V "@") a) <$> (op <|> (:"") <$> comma)) <*> expr
section = lParen *> (parenExpr <* rParen <|> rightSect <* rParen <|> rParen *> pure (V "()"))

maybePureUnit = maybe (V "pure" `A` V "()") id
stmt = (\p x -> Just . A (V ">>=" `A` x) . onePat [p] . maybePureUnit) <$> pat <*> (res "<-" *> expr)
  <|> (\x -> Just . maybe x (\y -> (V ">>=" `A` x) `A` (L "_" y))) <$> expr
  <|> (\ds -> Just . addLets ds . maybePureUnit) <$> (res "let" *> braceDef)
doblock = res "do" *> (maybePureUnit . foldr ($) Nothing <$> braceSep stmt)

compQual =
  (\p xs e -> A (A (V "concatMap") $ onePat [p] e) xs)
    <$> pat <*> (res "<-" *> expr)
  <|> (\b e -> A (A (A (V "if") b) e) $ V "[]") <$> expr
  <|> addLets <$> (res "let" *> braceDef)

sqExpr = between lSquare rSquare $
  ((&) <$> expr <*>
    (   res ".." *>
      (   (\hi lo -> (A (A (V "enumFromTo") lo) hi)) <$> expr
      <|> pure (A (V "enumFrom"))
      )
    <|> res "|" *>
      ((. A (V "pure")) . foldr (.) id <$> sepBy1 compQual comma)
    <|> (\t h -> listify (h:t)) <$> many (comma *> expr)
    )
  )
  <|> pure (V "[]")

atom = ifthenelse <|> doblock <|> letin <|> sqExpr <|> section
  <|> cas <|> lam <|> (paren comma *> pure (V ","))
  <|> V <$> (con <|> var) <|> literal

aexp = foldl1 A <$> some atom

withPrec precTab n p = p >>= \s ->
  if n == precOf s precTab then pure s else Parser $ const $ Left ""

exprP n = if n <= 9
  then getPrecs >>= \precTab
    -> exprP (succ n) >>= \a
    -> many ((,) <$> withPrec precTab n op <*> exprP (succ n)) >>= \as
    -> opFold precTab (\op x y -> A (A (V op) x) y) a as
  else aexp
expr = exprP 0

gcon = conId <|> paren (qconsym <|> (:"") <$> comma) <|> (lSquare *> rSquare *> pure "[]")

apat = PatVar <$> var <*> (res "@" *> (Just <$> apat) <|> pure Nothing)
  <|> flip PatVar Nothing <$> (res "_" *> pure "_")
  <|> flip PatCon [] <$> gcon
  <|> PatLit <$> literal
  <|> foldr (\h t -> PatCon ":" [h, t]) (PatCon "[]" [])
    <$> between lSquare rSquare (sepBy pat comma)
  <|> paren (foldr1 pairPat <$> sepBy1 pat comma <|> pure (PatCon "()" []))
  where pairPat x y = PatCon "," [x, y]

binPat f x y = PatCon f [x, y]
patP n = if n <= 9
  then getPrecs >>= \precTab
    -> patP (succ n) >>= \a
    -> many ((,) <$> withPrec precTab n qconop <*> patP (succ n)) >>= \as
    -> opFold precTab binPat a as
  else PatCon <$> gcon <*> many apat <|> apat
pat = patP 0

maybeWhere p = (&) <$> p <*> (res "where" *> (addLets <$> braceDef) <|> pure id)

guards s = maybeWhere $ res s *> expr <|> foldr ($) (V "join#") <$> some ((\x y -> case x of
  V "True" -> \_ -> y
  _ -> A (A (A (V "if") x) y)
  ) <$> (res "|" *> expr) <*> (res s *> expr))

onePat vs x = joinIsFail $ Pa [(vs, x)]
defOnePat vs x = Pa [(vs, x)]
opDef x f y rhs = [(f, defOnePat [x, y] rhs)]
leftyPat p expr = case pvars of
  [] -> []
  (h:t) -> let gen = '@':h in
    (gen, expr):map (\v -> (v, A (Pa [([p], V v)]) $ V gen)) pvars
  where
  pvars = filter (/= "_") $ patVars p
def = liftA2 (\l r -> [(l, r)]) var (liftA2 defOnePat (many apat) $ guards "=")
  <|> (pat >>= \x -> opDef x <$> varSym <*> pat <*> guards "=" <|> leftyPat x <$> guards "=")
coalesce = \case
  [] -> []
  h@(s, x):t -> case t of
    [] -> [h]
    (s', x'):t' -> let
      f (Pa vsts) (Pa vsts') = Pa $ vsts ++ vsts'
      f _ _ = error "bad multidef"
      in if s == s' then coalesce $ (s, f x x'):t' else h:coalesce t
defSemi = coalesce . concat <$> sepBy1 def (some semicolon)
braceDef = concat <$> braceSep defSemi

simpleType c vs = foldl TAp (TC c) (map TV vs)
conop = conSym <|> between backquote backquote conId
constr = (\x c y -> Constr c [x, y]) <$> aType <*> conop <*> aType
  <|> Constr <$> conId <*> many aType
adt = addAdt <$> between (res "data") (res "=") (simpleType <$> conId <*> many varId) <*> sepBy constr (res "|")

topdecls = braceSep
  $   adt
  <|> classDecl
  <|> instDecl
  <|> res "foreign" *>
    (   res "import" *> var *> (addFFI <$> lexeme tokStr <*> var <*> (res "::" *> _type))
    <|> res "export" *> var *> (addExport <$> lexeme tokStr <*> var)
    )
  <|> addDefs <$> defSemi
  <|> fixity *> pure id

program s = parse (between lexemePrelude eof topdecls) s

-- Primitives.
primAdts =
  [ addAdt (TC "()") [Constr "()" []]
  , addAdt (TC "Bool") [Constr "True" [], Constr "False" []]
  , addAdt (TAp (TC "[]") (TV "a")) [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]]
  , addAdt (TAp (TAp (TC ",") (TV "a")) (TV "b")) [Constr "," [TV "a", TV "b"]]]

prims = let
  dyad s = TC s `arr` (TC s `arr` TC s)
  bin s = A (ro "Q") (ro s)
  in map (second (first noQual)) $
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "EQ"))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "LE"))
    , ("charEq", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "EQ"))
    , ("charLE", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "LE"))
    , ("fix", (arr (arr (TV "a") (TV "a")) (TV "a"), ro "Y"))
    , ("if", (arr (TC "Bool") $ arr (TV "a") $ arr (TV "a") (TV "a"), ro "I"))
    , ("chr", (arr (TC "Int") (TC "Char"), ro "I"))
    , ("ord", (arr (TC "Char") (TC "Int"), ro "I"))
    , ("ioBind", (arr (TAp (TC "IO") (TV "a")) (arr (arr (TV "a") (TAp (TC "IO") (TV "b"))) (TAp (TC "IO") (TV "b"))), ro "C"))
    , ("ioPure", (arr (TV "a") (TAp (TC "IO") (TV "a")), ro "V"))
    , ("newIORef", (arr (TV "a") (TAp (TC "IO") (TAp (TC "IORef") (TV "a"))), ro "NEWREF"))
    , ("readIORef", (arr (TAp (TC "IORef") (TV "a")) (TAp (TC "IO") (TV "a")),
      A (ro "T") (ro "READREF")))
    , ("writeIORef", (arr (TAp (TC "IORef") (TV "a")) (arr (TV "a") (TAp (TC "IO") (TC "()"))),
      A (A (ro "R") (ro "WRITEREF")) (ro "B")))
    , ("exitSuccess", (TAp (TC "IO") (TV "a"), ro "END"))
    , ("unsafePerformIO", (arr (TAp (TC "IO") (TV "a")) (TV "a"), A (A (ro "C") (A (ro "T") (ro "END"))) (ro "K")))
    , ("join#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    , ("fail#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    ]
    ++ map (\(s, v) -> (s, (dyad "Int", bin v)))
      [ ("intAdd", "ADD")
      , ("intSub", "SUB")
      , ("intMul", "MUL")
      , ("intDiv", "DIV")
      , ("intMod", "MOD")
      , ("intQuot", "DIV")
      , ("intRem", "MOD")
      ]

-- Conversion to De Bruijn indices.
data LC = Ze | Su LC | Pass IntTree | La LC | App LC LC

debruijn n e = case e of
  E x -> Pass $ Lf x
  V v -> maybe (Pass $ LfVar v) id $
    foldr (\h found -> if h == v then Just Ze else Su <$> found) Nothing n
  A x y -> App (debruijn n x) (debruijn n y)
  L s t -> La (debruijn (s:n) t)

-- Kiselyov bracket abstraction.
data IntTree = Lf Extra | LfVar String | Nd IntTree IntTree
data Sem = Defer | Closed IntTree | Need Sem | Weak Sem

lf = Lf . Basic

ldef y = case y of
  Defer -> Need $ Closed (Nd (Nd (lf "S") (lf "I")) (lf "I"))
  Closed d -> Need $ Closed (Nd (lf "T") d)
  Need e -> Need $ (Closed (Nd (lf "S") (lf "I"))) ## e
  Weak e -> Need $ (Closed (lf "T")) ## e

lclo d y = case y of
  Defer -> Need $ Closed d
  Closed dd -> Closed $ Nd d dd
  Need e -> Need $ (Closed (Nd (lf "B") d)) ## e
  Weak e -> Weak $ (Closed d) ## e

lnee e y = case y of
  Defer -> Need $ Closed (lf "S") ## e ## Closed (lf "I")
  Closed d -> Need $ Closed (Nd (lf "R") d) ## e
  Need ee -> Need $ Closed (lf "S") ## e ## ee
  Weak ee -> Need $ Closed (lf "C") ## e ## ee

lwea e y = case y of
  Defer -> Need e
  Closed d -> Weak $ e ## Closed d
  Need ee -> Need $ (Closed (lf "B")) ## e ## ee
  Weak ee -> Weak $ e ## ee

x ## y = case x of
  Defer -> ldef y
  Closed d -> lclo d y
  Need e -> lnee e y
  Weak e -> lwea e y

babs t = case t of
  Ze -> Defer
  Su x -> Weak (babs x)
  Pass x -> Closed x
  La t -> case babs t of
    Defer -> Closed (lf "I")
    Closed d -> Closed (Nd (lf "K") d)
    Need e -> e
    Weak e -> Closed (lf "K") ## e
  App x y -> babs x ## babs y

nolam x = (\(Closed d) -> d) $ babs $ debruijn [] x

optim t = case t of
  Nd x y -> go (optim x) (optim y)
  _ -> t
  where
  go (Lf (Basic "I")) q = q
  go p q@(Lf (Basic c)) = case c of
    "I" -> case p of
      Lf (Basic "C") -> lf "T"
      Lf (Basic "B") -> lf "I"
      Nd p1 p2 -> case p1 of
        Lf (Basic "B") -> p2
        Lf (Basic "R") -> Nd (lf "T") p2
        _ -> Nd (Nd p1 p2) q
      _ -> Nd p q
    "T" -> case p of
      Nd (Lf (Basic "B")) (Lf (Basic "C")) -> lf "V"
      _ -> Nd p q
    _ -> Nd p q
  go p q = Nd p q

app01 s x y = maybe (A (L s x) y) snd $ go x where
  go expr = case expr of
    E _ -> Just (False, expr)
    V v -> Just $ if s == v then (True, y) else (False, expr)
    A l r -> do
      (a, l') <- go l
      (b, r') <- go r
      if a && b then Nothing else pure (a || b, A l' r')
    L v t -> if v == s then Just (False, expr) else second (L v) <$> go t

optiApp t = case t of
  A x y -> let
    x' = optiApp x
    y' = optiApp y
    in case x' of
      L s v -> app01 s v y'
      _ -> A x' y'
  L s x -> L s (optiApp x)
  _ -> t

-- Pattern compiler.
rewritePats dcs = \case
  [] -> pure $ V "join#"
  vsxs@((as0, _):_) -> case as0 of
    [] -> pure $ foldr1 (A . L "join#") $ snd <$> vsxs
    _ -> do
      let k = length as0
      n <- get
      put $ n + k
      let vs@(vh:vt) = take k $ (`shows` "#") <$> upFrom n
      cs <- flip mapM vsxs \(a:at, x) -> (a,) <$> foldM (\b (p, v) -> rewriteCase dcs v Tip [(p, b)]) x (zip at vt)
      flip (foldr L) vs <$> rewriteCase dcs vh Tip cs

patEq lit b x y = A (L "join#" $ A (A (A (V "if") (A (A (V "==") lit) b)) x) $ V "join#") y

rewriteCase dcs caseVar tab = \case
  [] -> flush $ V "join#"
  ((v, x):rest) -> go v x rest
  where
  rec = rewriteCase dcs caseVar
  go v x rest = case v of
    PatLit lit -> patEq lit (V caseVar) x <$> rec Tip rest >>= flush
    PatVar s m -> let x' = beta s (V caseVar) x in case m of
      Nothing -> A (L "join#" x') <$> rec Tip rest >>= flush
      Just v' -> go v' x' rest
    PatCon con args -> rec (insertWith (flip (.)) con ((args, x):) tab) rest
  flush onFail = case toAscList tab of
    [] -> pure onFail
    -- TODO: Check rest of `tab` lies in cs.
    (firstC, _):_ -> do
      let cs = dcs ! firstC
      jumpTable <- mapM (\(Constr s ts) -> case mlookup s tab of
          Nothing -> pure $ foldr L (V "join#") $ const "_" <$> ts
          Just f -> rewritePats dcs $ f []
        ) cs
      pure $ A (L "join#" $ foldl A (A (V $ specialCase cs) $ V caseVar) jumpTable) onFail

secondM f (a, b) = (a,) <$> f b
patternCompile dcs t = optiApp $ evalState (go t) 0 where
  go t = case t of
    E _ -> pure t
    V _ -> pure t
    A x y -> liftA2 A (go x) (go y)
    L s x -> L s <$> go x
    Pa vsxs -> mapM (secondM go) vsxs >>= rewritePats dcs

-- Unification and matching.
apply sub t = case t of
  TC v -> t
  TV v -> maybe t id $ lookup v sub
  TAp a b -> TAp (apply sub a) (apply sub b)

(@@) s1 s2 = map (second (apply s1)) s2 ++ s1

occurs s t = case t of
  TC v -> False
  TV v -> s == v
  TAp a b -> occurs s a || occurs s b

varBind s t = case t of
  TC v -> Right [(s, t)]
  TV v -> Right $ if v == s then [] else [(s, t)]
  TAp a b -> if occurs s t then Left "occurs check" else Right [(s, t)]

ufail t u = Left $ ("unify fail: "++) . shows t . (" vs "++) . shows u $ ""

mgu t u = case t of
  TC a -> case u of
    TC b -> if a == b then Right [] else ufail t u
    TV b -> varBind b t
    TAp a b -> ufail t u
  TV a -> varBind a u
  TAp a b -> case u of
    TC b -> ufail t u
    TV b -> varBind b t
    TAp c d -> mgu a c >>= unify b d

unify a b s = (@@ s) <$> mgu (apply s a) (apply s b)

merge s1 s2 = if all (\v -> apply s1 (TV v) == apply s2 (TV v))
  $ map fst s1 `intersect` map fst s2 then Just $ s1 ++ s2 else Nothing

match h t = case h of
  TC a -> case t of
    TC b | a == b -> Just []
    _ -> Nothing
  TV a -> Just [(a, t)]
  TAp a b -> case t of
    TAp c d -> case match a c of
      Nothing -> Nothing
      Just ac -> case match b d of
        Nothing -> Nothing
        Just bd -> merge ac bd
    _ -> Nothing

-- Type inference.
instantiate' t n tab = case t of
  TC s -> ((t, n), tab)
  TV s -> case lookup s tab of
    Nothing -> let va = TV $ show n in ((va, n + 1), (s, va):tab)
    Just v -> ((v, n), tab)
  TAp x y -> let
    ((t1, n1), tab1) = instantiate' x n tab
    ((t2, n2), tab2) = instantiate' y n1 tab1
    in ((TAp t1 t2, n2), tab2)

instantiatePred (Pred s t) ((out, n), tab) = first (first ((:out) . Pred s)) (instantiate' t n tab)

instantiate (Qual ps t) n = first (Qual ps1) $ fst $ instantiate' t n1 tab where
  ((ps1, n1), tab) = foldr instantiatePred (([], n), []) ps

proofApply sub a = case a of
  Proof (Pred cl ty) -> Proof (Pred cl $ apply sub ty)
  A x y -> A (proofApply sub x) (proofApply sub y)
  L s t -> L s $ proofApply sub t
  _ -> a

typeAstSub sub (t, a) = (apply sub t, proofApply sub a)

infer typed loc ast csn@(cs, n) = case ast of
  E x -> Right $ case x of
    Const _ -> ((TC "Int", ast), csn)
    ChrCon _ -> ((TC "Char", ast), csn)
    StrCon _ -> ((TAp (TC "[]") (TC "Char"), ast), csn)
  V s -> maybe (Left $ "undefined: " ++ s) Right
    $ (\t -> ((t, ast), csn)) <$> lookup s loc
    <|> insta <$> mlookup s typed
  A x y -> infer typed loc x (cs, n + 1) >>=
    \((tx, ax), csn1) -> infer typed loc y csn1 >>=
    \((ty, ay), (cs2, n2)) -> unify tx (arr ty va) cs2 >>=
    \cs -> Right ((va, A ax ay), (cs, n2))
  L s x -> first (\(t, a) -> (arr va t, L s a)) <$> infer typed ((s, va):loc) x (cs, n + 1)
  where
  va = TV $ show n
  insta ty = ((ty1, foldl A ast (map Proof preds)), (cs, n1))
    where (Qual preds ty1, n1) = instantiate ty n

findInstance tycl qn@(q, n) p@(Pred cl ty) insts = case insts of
  [] -> let v = '*':show n in Right (((p, v):q, n + 1), V v)
  Instance h name ps _:rest -> case match h ty of
    Nothing -> findInstance tycl qn p rest
    Just subs -> foldM (\(qn1, t) (Pred cl1 ty1) -> second (A t)
      <$> findProof tycl (Pred cl1 $ apply subs ty1) qn1) (qn, V name) ps

findProof tycl pred@(Pred classId t) psn@(ps, n) = case lookup pred ps of
  Nothing -> findInstance tycl psn pred $ case mlookup classId tycl of
    Nothing -> []
    Just (Tycl _ insts) -> insts
  Just s -> Right (psn, V s)

prove' tycl psn a = case a of
  Proof pred -> findProof tycl pred psn
  A x y -> prove' tycl psn x >>= \(psn1, x1) ->
    second (A x1) <$> prove' tycl psn1 y
  L s t -> second (L s) <$> prove' tycl psn t
  _ -> Right (psn, a)

depGraph typed (s, t) (vs, es) = (insert s t vs, foldr go es $ fv [] t) where
  go k ios@(ins, outs) = case lookup k typed of
    Nothing -> (insertWith union k [s] ins, insertWith union s [k] outs)
    Just _ -> ios

depthFirstSearch = (foldl .) \relation st@(visited, sequence) vertex ->
  if vertex `elem` visited then st else second (vertex:)
    $ depthFirstSearch relation (vertex:visited, sequence) (relation vertex)

spanningSearch   = (foldl .) \relation st@(visited, setSequence) vertex ->
  if vertex `elem` visited then st else second ((:setSequence) . (vertex:))
    $ depthFirstSearch relation (vertex:visited, []) (relation vertex)

scc ins outs = spanning . depthFirst where
  depthFirst = snd . depthFirstSearch outs ([], [])
  spanning   = snd . spanningSearch   ins  ([], [])

inferno tycl typed defmap syms = let
  loc = zip syms $ TV . (' ':) <$> syms
  in foldM (\(acc, (subs, n)) s ->
    maybe (Left $ "missing: " ++ s) Right (mlookup s defmap) >>=
    \expr -> infer typed loc expr (subs, n) >>=
    \((t, a), (ms, n1)) -> unify (TV (' ':s)) t ms >>=
    \cs -> Right ((s, (t, a)):acc, (cs, n1))
  ) ([], ([], 0)) syms >>=
  \(stas, (soln, _)) -> mapM id $ (\(s, ta) -> prove tycl s $ typeAstSub soln ta) <$> stas

prove tycl s (t, a) = flip fmap (prove' tycl ([], 0) a) \((ps, _), x) -> let
  applyDicts expr = foldl A expr $ map (V . snd) ps
  in (s, (Qual (map fst ps) t, foldr L (overFree s applyDicts x) $ map snd ps))
inferDefs' tycl defmap (typeTab, lambF) syms = let
  add stas = foldr (\(s, (q, cs)) (tt, f) -> (insert s q tt, f . ((s, cs):))) (typeTab, lambF) stas
  in add <$> inferno tycl typeTab defmap syms
inferDefs tycl defs typed = let
  typeTab = foldr (\(k, (q, _)) -> insert k q) Tip typed
  lambs = second snd <$> typed
  (defmap, graph) = foldr (depGraph typed) (Tip, (Tip, Tip)) defs
  ins k = maybe [] id $ mlookup k $ fst graph
  outs k = maybe [] id $ mlookup k $ snd graph
  in foldM (inferDefs' tycl defmap) (typeTab, (lambs++)) $ scc ins outs $ map fst $ toAscList defmap

dictVars ps n = (zip ps $ map (('*':) . show) $ upFrom n, n + length ps)

inferTypeclasses tycl typed dcs = concat <$> mapM perClass (toAscList tycl) where
  perClass (classId, Tycl sigs insts) = do
    let
      perInstance (Instance ty name ps idefs) = do
        let
          dvs = map snd $ fst $ dictVars ps 0
          perMethod s = do
            let Just expr = mlookup s idefs <|> pure (V $ "{default}" ++ s)
            (ta, (sub, n)) <- infer typed [] (patternCompile dcs expr) ([], 0)
            let
              (tx, ax) = typeAstSub sub ta
-- e.g. qc = Eq a => a -> a -> Bool
-- We instantiate: Eq a1 => a1 -> a1 -> Bool.
              Just qc = mlookup s typed
              (Qual [Pred _ headT] tc, n1) = instantiate qc n
-- Mix the predicates `ps` with the type of `headT`, applying a
-- substitution such as (a1, [a]) so the variable names match.
-- e.g. Eq a => [a] -> [a] -> Bool
              Just subc = match headT ty
              (Qual ps2 t2, n2) = instantiate (Qual ps $ apply subc tc) n1
            case match tx t2 of
              Nothing -> Left "class/instance type conflict"
              Just subx -> do
                ((ps3, _), tr) <- prove' tycl (dictVars ps2 0) (proofApply subx ax)
                if length ps2 /= length ps3
                  then Left $ ("want context: "++) . (foldr (.) id $ shows . fst <$> ps3) $ name
                  else pure tr
        ms <- mapM perMethod sigs
        pure (name, flip (foldr L) dvs $ L "@" $ foldl A (V "@") ms)
    mapM perInstance insts

untangle s = case fst <$> program s of
  Left e -> Left $ "parse error: " ++ e
  Right prog -> case foldr ($) (Neat Tip [] prims Tip [] []) $ primAdts ++ prog of
    Neat tycl defs typed dcs ffis exs -> do
      (qs, lambF) <- inferDefs tycl (second (patternCompile dcs) <$> defs) typed
      mets <- inferTypeclasses tycl qs dcs
      pure ((qs, lambF mets), (ffis, exs))

optiComb' (subs, combs) (s, lamb) = let
  gosub t = case t of
    LfVar v -> maybe t id $ lookup v subs
    Nd a b -> Nd (gosub a) (gosub b)
    _ -> t
  c = optim $ gosub $ nolam $ optiApp lamb
  combs' = combs . ((s, c):)
  in case c of
    Lf (Basic _) -> ((s, c):subs, combs')
    LfVar v -> if v == s then (subs, combs . ((s, Nd (lf "Y") (lf "I")):)) else ((s, gosub c):subs, combs')
    _ -> (subs, combs')
optiComb lambs = ($[]) . snd $ foldl optiComb' ([], id) lambs

instance Show Type where
  showsPrec _ = \case
    TC s -> (s++)
    TV s -> (s++)
    TAp (TAp (TC "->") a) b -> showParen True $ shows a . (" -> "++) . shows b
    TAp a b -> showParen True $ shows a . (' ':) . shows b
instance Show Pred where
  showsPrec _ (Pred s t) = (s++) . (' ':) . shows t . (" => "++)
instance Show Qual where
  showsPrec _ (Qual ps t) = foldr (.) id (map shows ps) . shows t
instance Show Extra where
  showsPrec _ = \case
    Basic s -> (s++)
    Const i -> shows i
    ChrCon c -> ('\'':) . (c:) . ('\'':)
    StrCon s -> ('"':) . (s++) . ('"':)
instance Show Pat where
  showsPrec _ = \case
    PatLit e -> shows e
    PatVar s mp -> (s++) . maybe id ((('@':) .) . shows) mp
    PatCon s ps -> (s++) . foldr (.) id (((' ':) .) . shows <$> ps)

showVar s@(h:_) = showParen (elem h ":!#$%&*+./<=>?@\\^|-~") (s++)

instance Show Ast where
  showsPrec prec = \case
    E e -> shows e
    V s -> showVar s
    A x y -> showParen (1 <= prec) $ shows x . (' ':) . showsPrec 1 y
    L s t -> showParen True $ ('\\':) . (s++) . (" -> "++) . shows t
    Pa vsts -> ('\\':) . showParen True (foldr (.) id $ intersperse (';':) $ map (\(vs, t) -> foldr (.) id (intersperse (' ':) $ map (showParen True . shows) vs) . (" -> "++) . shows t) vsts)
    Proof p -> ("{Proof "++) . shows p . ("}"++)

instance Show IntTree where
  showsPrec prec = \case
    LfVar s -> showVar s
    Lf extra -> shows extra
    Nd x y -> showParen (1 <= prec) $ shows x . (' ':) . showsPrec 1 y

disasm (s, t) = (s++) . (" = "++) . shows t . (";\n"++)

dumpCombs s = case untangle s of
  Left err -> err
  Right ((_, lambs), _) -> foldr ($) [] $ map disasm $ optiComb lambs

dumpLambs s = case untangle s of
  Left err -> err
  Right ((_, lambs), _) -> foldr ($) [] $
    (\(s, t) -> (s++) . (" = "++) . shows t . ('\n':)) <$> lambs

dumpTypes s = case untangle s of
  Left err -> err
  Right ((typed, _), _) -> ($ "") $ foldr (.) id $
    map (\(s, q) -> (s++) . (" :: "++) . shows q . ('\n':)) $ toAscList typed

appCell (hp, bs) x y = (hp, (hp + 2, bs . (x:) . (y:)))

enc tab mem = \case
  Lf n -> case n of
    Basic c -> (comEnum c, mem)
    Const c -> appCell mem (comEnum "NUM") c
    ChrCon c -> appCell mem (comEnum "NUM") $ ord c
    StrCon s -> enc tab mem $ foldr (\h t -> Nd (Nd (lf "CONS") (Lf $ ChrCon h)) t) (lf "K") s
  LfVar s -> maybe (error $ "resolve " ++ s) (, mem) $ mlookup s tab
  Nd x y -> let
    (xAddr, mem') = enc tab mem x
    (yAddr, mem'') = enc tab mem' y
    in appCell mem'' xAddr yAddr

asm combs = tabmem where
  tabmem = foldl (\(as, m) (s, t) -> let (p, m') = enc (fst tabmem) m t
    in (insert s p as, m')) (Tip, (128, id)) combs

-- Code generation.
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

genMain n = "int main(int argc,char**argv){env_argc=argc;env_argv=argv;rts_init();rts_reduce(" ++ shows n ");return 0;}\n"

compile s = case untangle s of
  Left err -> err
  Right ((_, lambs), (ffis, exs)) -> let
    (tab, (_, memF)) = (asm $ optiComb lambs)
    mem = memF []
    in ("typedef unsigned u;\n"++)
    . ("enum{_UNDEFINED=0,"++)
    . foldr (.) id (map (\(s, _) -> ('_':) . (s++) . (',':)) comdefs)
    . ("};\n"++)
    . ("static const u prog[]={" ++)
    . foldr (.) id (map (\n -> shows n . (',':)) mem)
    . ("};\nstatic const u prog_size="++) . shows (length mem) . (";\n"++)
    . ("static u root[]={" ++)
    . foldr (\(x, y) f -> maybe undefined shows (mlookup y tab) . (", " ++) . f) id exs
    . ("0};\n" ++)
    . (preamble++)
    . (libc++)
    . (concatMap ffiDeclare ffis ++)
    . ("static void foreign(u n) {\n  switch(n) {\n" ++)
    . ffiDefine (length ffis - 1) ffis
    . ("\n  }\n}\n" ++)
    . runFun
    . (foldr (.) id $ zipWith (\p n -> (("EXPORT(f" ++ shows n ", \"" ++ fst p ++ "\", " ++ shows n ")\n") ++)) exs (upFrom 0))
    $ maybe "" genMain (mlookup "main" tab)

-- Main VM loop.
comdefsrc = [r|
F x = "foreign(num(1));"
Y x = x "sp[1]"
Q x y z = z(y x)
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
|]
comb = (,) <$> conId <*> ((,) <$> many varId <*> (res "=" *> combExpr))
combExpr = foldl1 A <$> some
  (V <$> varId <|> E . StrCon <$> lexeme tokStr <|> paren combExpr)
comdefs = case parse (lexemePrelude *> braceSep comb <* eof) comdefsrc of
  Left e -> error e
  Right (cs, _) -> cs
comEnum s = maybe (error s) id $ lookup s $ zip (fst <$> comdefs) (upFrom 1)
comName i = maybe undefined id $ lookup i $ zip (upFrom 1) (fst <$> comdefs)

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
  for (u i = 0; i < prog_size; i++) mem[hp++] = prog[i];
  spTop = mem + TOP - 1;
}

void rts_reduce(u n) {
  *(sp = spTop) = app(app(n, _UNDEFINED), _END);
  run();
}
|]++)

genArg m a = case a of
  V s -> ("arg("++) . (maybe undefined shows $ lookup s m) . (')':)
  E (StrCon s) -> (s++)
  A x y -> ("app("++) . genArg m x . (',':) . genArg m y . (')':)
genArgs m as = foldl1 (.) $ map (\a -> (","++) . genArg m a) as
genComb (s, (args, body)) = let
  argc = ('(':) . shows (length args)
  m = zip args $ upFrom 1
  in ("case _"++) . (s++) . (':':) . (case body of
    A (A x y) z -> ("lazy3"++) . argc . genArgs m [x, y, z] . (");"++)
    A x y -> ("lazy2"++) . argc . genArgs m [x, y] . (");"++)
    E (StrCon s) -> (s++)
  ) . ("break;\n"++)

main = getArgs >>= \case
  "comb":_ -> interact dumpCombs
  "lamb":_ -> interact dumpLambs
  "type":_ -> interact dumpTypes
  _ -> interact compile
  where
  getArg' k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> getArg' (k + 1) n
  getArgs = getArgCount >>= \n -> mapM (getArg' 0) (take (n - 1) $ upFrom 1)

iterate f x = x : iterate f (f x)
takeWhile _ [] = []
takeWhile p xs@(x:xt)
  | p x  = x : takeWhile p xt
  | True = []

class Enum a where
  succ           :: a -> a
  pred           :: a -> a
  toEnum         :: Int -> a
  fromEnum       :: a -> Int
  enumFrom       :: a -> [a]
  enumFromTo     :: a -> a -> [a]
instance Enum Int where
  succ = (+1)
  pred = (+(0-1))
  toEnum = id
  fromEnum = id
  enumFrom = iterate succ
  enumFromTo lo hi = takeWhile (<= hi) $ enumFrom lo
instance Enum Char where
  succ = chr . (+1) . ord
  pred = chr . (+(0-1)) . ord
  toEnum = chr
  fromEnum = ord
  enumFrom = iterate succ
  enumFromTo lo hi = takeWhile (<= hi) $ enumFrom lo

(+) = intAdd
(-) = intSub
(*) = intMul
div = intDiv
mod = intMod
