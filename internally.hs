-- String interning. Work-in-progress.
infixr 9 .;
infixl 7 * , / , %;
infixl 6 + , -;
infixr 5 ++;
infixl 4 <*> , <$> , <* , *>;
infix 4 == , /= , <=;
infixl 3 && , <|>;
infixl 2 ||;
infixl 1 >> , >>=;
infixr 0 $;

ffi "putchar" putChar :: Int -> IO Int;
ffi "getchar" getChar :: IO Int;
ffi "getargcount" getArgCount :: IO Int;
ffi "getargchar" getArgChar :: Int -> Int -> IO Char;

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where
{ pure :: a -> f a
; (<*>) :: f (a -> b) -> f a -> f b
};
class Monad m where
{ return :: a -> m a
; (>>=) :: m a -> (a -> m b) -> m b
};
(<$>) = fmap;
liftA2 f x y = f <$> x <*> y;
(>>) f g = f >>= \_ -> g;
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Int where { (==) = intEq };
instance Eq Char where { (==) = charEq };
($) f x = f x;
id x = x;
const x y = x;
flip f x y = f y x;
(&) x f = f x;
class Ord a where { (<=) :: a -> a -> Bool };
instance Ord Int where { (<=) = intLE };
instance Ord Char where { (<=) = charLE };
data Ordering = LT | GT | EQ;
compare x y = if x <= y then if y <= x then EQ else LT else GT;
instance Ord a => Ord [a] where {
  (<=) xs ys = case xs of
    { [] -> True
    ; x:xt -> case ys of
      { [] -> False
      ; y:yt -> case compare x y of
        { LT -> True
        ; GT -> False
        ; EQ -> xt <= yt
        }
      }
    }
};
data Maybe a = Nothing | Just a;
data Either a b = Left a | Right b;
fpair (x, y) f = f x y;
fst (x, y) = x;
snd (x, y) = y;
uncurry f (x, y) = f x y;
first f (x, y) = (f x, y);
second f (x, y) = (x, f y);
not a = if a then False else True;
x /= y = not $ x == y;
(.) f g x = f (g x);
(||) f g = if f then True else g;
(&&) f g = if f then g else False;
flst xs n c = case xs of { [] -> n; h:t -> c h t };
instance Eq a => Eq [a] where { (==) xs ys = case xs of
  { [] -> case ys of
    { [] -> True
    ; _ -> False
    }
  ; x:xt -> case ys of
    { [] -> False
    ; y:yt -> x == y && xt == yt
    }
  }};
take n xs = if n == 0 then [] else flst xs [] \h t -> h:take (n - 1) t;
maybe n j m = case m of { Nothing -> n; Just x -> j x };
fmaybe m n j = case m of { Nothing -> n; Just x -> j x };
instance Functor Maybe where { fmap f = maybe Nothing (Just . f) };
instance Applicative Maybe where { pure = Just ; mf <*> mx = maybe Nothing (\f -> maybe Nothing (Just . f) mx) mf };
instance Monad Maybe where { return = Just ; mf >>= mg = maybe Nothing mg mf };
foldr c n l = flst l n (\h t -> c h(foldr c n t));
length = foldr (\_ n -> n + 1) 0;
mapM f = foldr (\a rest -> liftA2 (:) (f a) rest) (pure []);
mapM_ f = foldr ((>>) . f) (pure ());
foldM f z0 xs = foldr (\x k z -> f z x >>= k) pure xs z0;
instance Applicative IO where { pure = ioPure ; (<*>) f x = ioBind f \g -> ioBind x \y -> ioPure (g y) };
instance Monad IO where { return = ioPure ; (>>=) = ioBind };
instance Functor IO where { fmap f x = ioPure f <*> x };
putStr = mapM_ $ putChar . ord;
error s = unsafePerformIO $ putStr s >> putChar (ord '\n') >> exitSuccess;
undefined = error "undefined";
foldr1 c l@(h:t) = maybe undefined id $ foldr (\x m -> Just $ maybe x (c x) m) Nothing l;
foldl f a bs = foldr (\b g x -> g (f x b)) (\x -> x) bs a;
foldl1 f (h:t) = foldl f h t;
elem k xs = foldr (\x t -> x == k || t) False xs;
find f xs = foldr (\x t -> if f x then Just x else t) Nothing xs;
(++) = flip (foldr (:));
concat = foldr (++) [];
itemize c = c:[];
map = flip (foldr . ((:) .)) [];
instance Functor [] where { fmap = map };
concatMap = (concat .) . map;
lookup s = foldr (\(k, v) t -> if s == k then Just v else t) Nothing;
all f = foldr (&&) True . map f;
any f = foldr (||) False . map f;
upFrom n = n : upFrom (n + 1);
zipWith f xs ys = flst xs [] $ \x xt -> flst ys [] $ \y yt -> f x y : zipWith f xt yt;
zip = zipWith (,);
data State s a = State (s -> (a, s));
runState (State f) = f;
instance Functor (State s) where { fmap f = \(State h) -> State (first f . h) };
instance Applicative (State s) where
{ pure a = State (a,)
; (State f) <*> (State x) = State \s -> fpair (f s) \g s' -> first g $ x s'
};
instance Monad (State s) where
{ return a = State (a,)
; (State h) >>= f = State $ uncurry (runState . f) . h
};
evalState m s = fst $ runState m s;
get = State \s -> (s, s);
put n = State \s -> ((), n);
either l r e = case e of { Left x -> l x; Right x -> r x };
instance Functor (Either a) where { fmap f e = case e of
  { Left x -> Left x
  ; Right x -> Right $ f x
  }
};
instance Applicative (Either a) where { pure = Right ; ef <*> ex = case ef of
  { Left s -> Left s
  ; Right f -> case ex of
    { Left s -> Left s
    ; Right x -> Right $ f x
    }
  }
};
instance Monad (Either a) where { return = Right ; ex >>= f = case ex of
  { Left s -> Left s
  ; Right x -> f x
  }
};

-- Map.

data Map k a = Tip | Bin Int k a (Map k a) (Map k a);
size m = case m of { Tip -> 0 ; Bin sz _ _ _ _ -> sz };
node k x l r = Bin (1 + size l + size r) k x l r;
singleton k x = Bin 1 k x Tip Tip;
singleL k x l (Bin _ rk rkx rl rr) = node rk rkx (node k x l rl) rr;
doubleL k x l (Bin _ rk rkx (Bin _ rlk rlkx rll rlr) rr) =
  node rlk rlkx (node k x l rll) (node rk rkx rlr rr);
singleR k x (Bin _ lk lkx ll lr) r = node lk lkx ll (node k x lr r);
doubleR k x (Bin _ lk lkx ll (Bin _ lrk lrkx lrl lrr)) r =
  node lrk lrkx (node lk lkx ll lrl) (node k x lrr r);
balance k x l r = (if size l + size r <= 1
  then node
  else if 5 * size l + 3 <= 2 * size r
    then case r of
      { Tip -> node
      ; Bin sz _ _ rl rr -> if 2 * size rl + 1 <= 3 * size rr
        then singleL
        else doubleL
      }
    else if 5 * size r + 3 <= 2 * size l
      then case l of
        { Tip -> node
        ; Bin sz _ _ ll lr -> if 2 * size lr + 1 <= 3 * size ll
          then singleR
          else doubleR
        }
      else node
  ) k x l r;
insert kx x t = case t of
  { Tip -> singleton kx x
  ; Bin sz ky y l r -> case compare kx ky of
    { LT -> balance ky y (insert kx x l) r
    ; GT -> balance ky y l (insert kx x r)
    ; EQ -> Bin sz kx x l r
    }
  };
insertWith f kx x t = case t of
  { Tip -> singleton kx x
  ; Bin sy ky y l r -> case compare kx ky of
    { LT -> balance ky y (insertWith f kx x l) r
    ; GT -> balance ky y l (insertWith f kx x r)
    ; EQ -> Bin sy kx (f x y) l r
    }
  };
mlookup kx t = case t of
  { Tip -> Nothing
  ; Bin _ ky y l r -> case compare kx ky of
    { LT -> mlookup kx l
    ; GT -> mlookup kx r
    ; EQ -> Just y
    }
  };
fromList = foldl (\t (k, x) -> insert k x t) Tip;

foldrWithKey f = let
  { go z t = case t of
    { Tip -> z
    ; Bin _ kx x l r -> go (f kx x (go z r)) l
    }
  } in go;

toAscList = foldrWithKey (\k x xs -> (k,x):xs) [];

-- Parsing.

data Type = TC String | TV String | TAp Type Type;
arr a b = TAp (TAp (TC "->") a) b;
data Extra = Basic Char | Const Int | ChrCon Char | StrCon String;
data Pat = PatLit Extra | PatVar Int (Maybe Pat) | PatCon Int [Pat];
data Ast = E Extra | V Int | A Ast Ast | L Int Ast | Pa [([Pat], Ast)] | Ca Ast [(Pat, Ast)] | Proof Pred;
data ParseState = ParseState String (Map Int (Int, Assoc)) (Int, Map String Int);
data Parser a = Parser (ParseState -> Maybe (a, ParseState));
data Constr = Constr Int [Type];
data Pred = Pred String Type;
data Qual = Qual [Pred] Type;
noQual = Qual [];

data Neat = Neat
  -- | Instance environment.
  (Map String [(Int, Qual)])
  -- | Either top-level or instance definitions.
  [Either (Int, Ast) (Int, (Qual, [(Int, Ast)]))]
  -- | Typed ASTs, ready for compilation, including ADTs and methods,
  -- e.g. (==), (Eq a => a -> a -> Bool, select-==)
  [(Int, (Qual, Ast))]
  -- | Data constructor table.
  (Map Int [Constr])  -- AdtTab
  -- | FFI declarations.
  [(String, Type)]
  -- | Exports.
  [(String, Int)]
  ;
fneat (Neat a b c d e f) z = z a b c d e f;

getPrecs = Parser \st@(ParseState _ precs _) -> Just (precs, st);
putPrecs precs = Parser \(ParseState s _ strs) -> Just ((), ParseState s precs strs);

getStrs = Parser \st@(ParseState _ _ strs) -> Just (strs, st);
putStrs strs = Parser \(ParseState s precs _) -> Just ((), ParseState s precs strs);

intern s st@(next, m) = case mlookup s m of
  { Nothing -> let { next' = next + 2 } in (next', (next', insert s next' m))
  ; Just k -> (k, st)
  };

internParse s = getStrs >>= \st -> fpair (intern s st) \k st' -> putStrs st' >> pure k;

dropWhile p xs = flst xs [] \x xt -> if p x then dropWhile p xt else xs;
break p xs = flst xs ([], []) \x xt -> if p x then ([], xs) else first (x:) $ break p xt;
words s = case dropWhile (' ' ==) s of
  { [] -> []
  ; t -> fpair (break (' '==) t) \w s' -> w:words s'
  };
internInit = foldr ((snd .) . intern) (0, Tip) $
  words "== True False () : [] , pjoin# cjoin# main" ++ (fst <$> prims);
strCode s = maybe (error $ "MISSING " ++ s) id $ mlookup s $ snd internInit;

ro = E . Basic;
conOf (Constr s _) = s;
specialCase (h:_) = conOf h + 1000000;
mkCase t cs = (specialCase cs,
  ( noQual $ arr t $ foldr arr (TV "case") $ map (\(Constr _ ts) -> foldr arr (TV "case") ts) cs
  , ro 'I'));
mkStrs = snd . foldl (\(s, l) u -> (s + 2, s:l)) (1, []);
scottEncode vs s ts
  | s == strCode ":" = ro ':'
  | True = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs);
scottConstr t cs c = case c of { Constr s ts -> (s,
  ( noQual $ foldr arr t ts
  , scottEncode (map conOf cs) s $ mkStrs ts)) };
mkAdtDefs t cs = mkCase t cs : map (scottConstr t cs) cs;

showInt' n = if 0 == n then id else (showInt' $ n/10) . ((:) (chr $ 48+n%10));
showInt n = if 0 == n then ('0':) else showInt' n;

mkFFIHelper n t acc = case t of
  { TC s -> acc
  ; TAp (TC "IO") _ -> acc
  ; TAp (TAp (TC "->") x) y -> L n $ mkFFIHelper (n + 1) y $ A (V n) acc
  };

updateDcs cs dcs = foldr (\(Constr s _) m -> insert s cs m) dcs cs;
addAdt t cs (Neat ienv fs typed dcs ffis exs) =
  Neat ienv fs (mkAdtDefs t cs ++ typed) (updateDcs cs dcs) ffis exs;

addClass classId v ms (Neat ienv fs typed dcs ffis exs) = let
  { vars = zipWith const (upFrom 0) ms
  } in Neat ienv fs (zipWith (\var (s, t) ->
    (s, (Qual [Pred classId v] t,
      L 1 $ A (V 1) $ foldr L (V var) vars))) vars ms ++ typed) dcs ffis exs;
addInst name cl q ds (Neat ienv fs typed dcs ffis exs) =
  Neat (insertWith (++) cl [(name, q)] ienv) (Right (name, (q, ds)):fs) typed dcs ffis exs;
addFFI foreignname ourname t (Neat ienv fs typed dcs ffis exs) =
  Neat ienv fs ((ourname, (Qual [] t, mkFFIHelper 0 t $ A (ro 'F') (ro $ chr $ length ffis))) : typed) dcs ((foreignname, t):ffis) exs;
addDefs ds (Neat ienv fs typed dcs ffis exs) = Neat ienv (map Left ds ++ fs) typed dcs ffis exs;
addExport e f (Neat ienv fs typed dcs ffis exs) = Neat ienv fs typed dcs ffis ((e, f):exs);

parse (Parser f) inp = f inp;
instance Applicative Parser where
{ pure x = Parser \inp -> Just (x, inp)
; (<*>) x y = Parser \inp -> case parse x inp of
  { Nothing -> Nothing
  ; Just (fun, t) -> case parse y t of
    { Nothing -> Nothing
    ; Just (arg, u) -> Just (fun arg, u)
    }
  }
};
instance Monad Parser where
{ return = pure
; (>>=) x f = Parser \inp -> case parse x inp of
  { Nothing -> Nothing
  ; Just (a, t) -> parse (f a) t
  }
};

sat f = Parser \(ParseState inp precs strs) -> flst inp Nothing \h t ->
  if f h then Just (h, ParseState t precs strs) else Nothing;

instance Functor Parser where { fmap f x = pure f <*> x };
(<|>) x y = Parser \inp -> fmaybe (parse x inp) (parse y inp) Just;
(*>) = liftA2 \x y -> y;
(<*) = liftA2 \x y -> x;
many p = liftA2 (:) p (many p) <|> pure [];
some p = liftA2 (:) p (many p);
sepBy1 p sep = liftA2 (:) p (many (sep *> p));
sepBy p sep = sepBy1 p sep <|> pure [];

char c = sat (c ==);
between x y p = x *> (p <* y);
com = char '-' *> between (char '-') (char '\n') (many $ sat ('\n' /=));
sp = many ((itemize <$> (sat (\c -> (c == ' ') || (c == '\n')))) <|> com);
spc f = f <* sp;
spch = spc . char;
wantWith pred f = Parser \inp -> case parse f inp of
  { Nothing -> Nothing
  ; Just at -> if pred $ fst at then Just at else Nothing
  };
paren = between (spch '(') (spch ')');
small = sat \x -> ((x <= 'z') && ('a' <= x)) || (x == '_');
large = sat \x -> (x <= 'Z') && ('A' <= x);
digit = sat \x -> (x <= '9') && ('0' <= x);
symbo = sat \c -> elem c "!#$%&*+./<=>?@\\^|-~";
varLex = liftA2 (:) small (many (small <|> large <|> digit <|> char '\''));
conId = spc (liftA2 (:) large (many (small <|> large <|> digit <|> char '\'')));
varId = spc $ wantWith (\s -> not $ elem s ["class", "data", "instance", "of", "where", "if", "then", "else"]) varLex;
opTail = many $ char ':' <|> symbo;
conSym = spc $ liftA2 (:) (char ':') opTail;
varSym = spc $ wantWith (not . (`elem` ["@", "=", "|", "->", "=>"])) $ liftA2 (:) symbo opTail;
con = (conId <|> paren conSym) >>= internParse;
var = (varId <|> paren varSym) >>= internParse;
op = varSym <|> conSym <|> between (spch '`') (spch '`') (conId <|> varId) >>= internParse;
conop = (conSym <|> between (spch '`') (spch '`') conId) >>= internParse;
escChar = char '\\' *> ((sat \c -> elem c "'\"\\") <|> ((\c -> '\n') <$> char 'n'));
litOne delim = escChar <|> sat (delim /=);
litInt = Const . foldl (\n d -> 10*n + ord d - ord '0') 0 <$> spc (some digit);
litChar = ChrCon <$> between (char '\'') (spch '\'') (litOne '\'');
litStr = between (char '"') (spch '"') $ many (litOne '"');
lit = StrCon <$> litStr <|> litChar <|> litInt;
sqList r = between (spch '[') (spch ']') $ sepBy r (spch ',');
want f s = wantWith (s ==) f;
tok s = spc $ want (some (char '_' <|> symbo) <|> varLex) s;

gcon = (conId <|> paren (conSym <|> (itemize <$> spch ',')) <|> ((:) <$> spch '[' <*> (itemize <$> spch ']')))
  >>= internParse;

apat = PatVar <$> var <*> (tok "@" *> (Just <$> apat) <|> pure Nothing)
  <|> flip PatCon [] <$> gcon
  <|> PatLit <$> lit
  <|> foldr (\h t -> PatCon (strCode ":") [h, t]) (PatCon (strCode "[]") []) <$> sqList pat
  <|> paren ((&) <$> pat <*> ((spch ',' *> ((\y x -> PatCon (strCode ",") [x, y]) <$> pat)) <|> pure id))
  ;
pat = PatCon <$> gcon <*> many apat
  <|> (&) <$> apat <*> ((\s r l -> PatCon s [l, r]) <$> conop <*> apat <|> pure id);

maybeWhere p = (&) <$> p <*> (tok "where" *> (addLets . coalesce <$> braceSep def) <|> pure id);

vpjoin = V $ strCode "pjoin#";

guards s = maybeWhere $ tok s *> expr <|> foldr ($) vpjoin <$> some ((\x y -> case x of
  { V tr | tr == strCode "True" -> \_ -> y
  ; _ -> A (A (A (V $ strCode "if") x) y)
  }) <$> (spch '|' *> expr) <*> (tok s *> expr));
alt = (,) <$> pat <*> guards "->";
braceSep f = between (spch '{') (spch '}') (sepBy f (spch ';'));
alts = braceSep alt;
cas = Ca <$> between (tok "case") (tok "of") expr <*> alts;
lamCase = tok "case" *> (L 1 . Ca (V 1) <$> alts);
onePat vs x = Pa [(vs, x)];
lam = spch '\\' *> (lamCase <|> liftA2 onePat (some apat) (tok "->" *> expr));

flipPairize y x = A (A (V $ strCode ",") x) y;
thenComma = spch ',' *> ((flipPairize <$> expr) <|> pure (A (V $ strCode ",")));
parenExpr = (&) <$> expr <*> (((\v a -> A (V v) a) <$> op) <|> thenComma <|> pure id);
rightSect = ((\v a -> L 1 $ A (A (V v) $ V 1) a) <$> (op <|> (strCode . itemize <$> spch ','))) <*> expr;
section = spch '(' *> (parenExpr <* spch ')' <|> rightSect <* spch ')' <|> spch ')' *> pure (V $ strCode "()"));

isFreePat v = \case
  { PatLit _ -> False
  ; PatVar s m -> s == v || maybe False (isFreePat v) m
  ; PatCon _ args -> any (isFreePat v) args
  };

isFree v expr = case expr of
  { E _ -> False
  ; V s -> s == v
  ; A x y -> isFree v x || isFree v y
  ; L w t -> v /= w && isFree v t
  ; Pa vsts -> any (\(vs, t) -> not (any (isFreePat v) vs) && isFree v t) vsts
  ; Ca x as -> isFree v x || isFree v (Pa $ first (:[]) <$> as)
  };

overFree s f t = case t of
  { E _ -> t
  ; V s' -> if s == s' then f t else t
  ; A x y -> A (overFree s f x) (overFree s f y)
  ; L s' t' -> if s == s' then t else L s' $ overFree s f t'
  };

beta s t x = overFree s (const t) x;

maybeFix s x = if isFree s x then A (ro 'Y') (L s x) else x;

opDef x f y rhs = (f, onePat [x, y] rhs);

coalesce ds = flst ds [] \h@(s, x) t -> flst t [h] \(s', x') t' -> let
  { f (Pa vsts) (Pa vsts') = Pa $ vsts ++ vsts'
  ; f _ _ = error "bad multidef"
  } in if s == s' then coalesce $ (s, f x x'):t' else h:coalesce t;

def = opDef <$> apat <*> (varSym >>= internParse) <*> apat <*> guards "="
  <|> liftA2 (,) var (liftA2 onePat (many apat) $ guards "=");

addLets ls x = foldr (\(name, def) t -> A (L name t) $ maybeFix name def) x ls;
letin = addLets <$> between (tok "let") (tok "in") (coalesce <$> braceSep def) <*> expr;
ifthenelse = (\a b c -> A (A (A (V $ strCode "if") a) b) c) <$>
  (tok "if" *> expr) <*> (tok "then" *> expr) <*> (tok "else" *> expr);
listify = foldr (\h t -> A (A (V $ strCode ":") h) t) (V $ strCode "[]");
atom = ifthenelse <|> letin <|> listify <$> sqList expr <|> section <|> cas <|> lam <|> (paren (spch ',') *> pure (V $ strCode ",")) <|> fmap V (con <|> var) <|> E <$> lit;
aexp = foldl1 A <$> some atom;

data Assoc = NAssoc | LAssoc | RAssoc;
instance Eq Assoc where
{ NAssoc == NAssoc = True
; LAssoc == LAssoc = True
; RAssoc == RAssoc = True
; _ == _ = False
};
precOf s precTab = fmaybe (mlookup s precTab) 5 fst;
assocOf s precTab = fmaybe (mlookup s precTab) LAssoc snd;
opWithPrec precTab n = wantWith (\s -> n == precOf s precTab) op;
opFold precTab e xs = case xs of
  { [] -> e
  ; x:xt -> case find (\y -> assocOf (fst x) precTab /= assocOf (fst y) precTab) xt of
    { Nothing -> case assocOf (fst x) precTab of
      { NAssoc -> case xt of
        { [] -> fpair x (\op y -> A (A (V op) e) y)
        ; y:yt -> undefined
        }
      ; LAssoc -> foldl (\a (op, y) -> A (A (V op) a) y) e xs
      ; RAssoc -> foldr (\(op, y) b -> \e -> A (A (V op) e) (b y)) id xs $ e
      }
    ; Just y -> undefined
    }
  };
exprP n = if n <= 9
  then getPrecs >>= \precTab -> liftA2 (opFold precTab) (exprP $ succ n) (many (liftA2 (,) (opWithPrec precTab n) (exprP $ succ n)))
  else aexp;
expr = exprP 0;

bType = foldl1 TAp <$> some aType;
_type = foldr1 arr <$> sepBy bType (spc (tok "->"));
typeConst = (\s -> if s == "String" then TAp (TC "[]") (TC "Char") else TC s) <$> conId;
aType = spch '(' *> (spch ')' *> pure (TC "()") <|> ((&) <$> _type <*> ((spch ',' *> ((\a b -> TAp (TAp (TC ",") b) a) <$> _type)) <|> pure id)) <* spch ')') <|>
  typeConst <|> (TV <$> varId) <|>
  (spch '[' *> (spch ']' *> pure (TC "[]") <|> TAp (TC "[]") <$> (_type <* spch ']')));

simpleType c vs = foldl TAp (TC c) (map TV vs);

constr = (\x c y -> Constr c [x, y]) <$> aType <*> (conSym >>= internParse) <*> aType
  <|> Constr <$> (conId >>= internParse) <*> many aType;

adt = addAdt <$> between (tok "data") (spch '=') (simpleType <$> conId <*> many varId) <*> sepBy constr (spch '|');

fixityList a =
  (\c -> ord c - ord '0') <$> spc digit >>= \n ->
  sepBy op (spch ',') >>= \os ->
  getPrecs >>= \precs -> putPrecs (foldr (\o m -> insert o (n, a) m) precs os) >>
  pure id;
fixityDecl kw a = tok kw *> fixityList a;
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc;

genDecl = (,) <$> var <*> (char ':' *> spch ':' *> _type);
classDecl = tok "class" *> (addClass <$> conId <*> (TV <$> varId) <*> (tok "where" *> braceSep genDecl));

dictName cl t = '{':cl ++ (' ':showType t "") ++ "}";
inst = _type;
instDecl = tok "instance" *>
  ((\ps cl ty defs -> internParse (dictName cl ty) >>= \name -> pure $ addInst name cl (Qual ps ty) defs) <$>
  (((itemize .) . Pred <$> conId <*> (inst <* tok "=>")) <|> pure [])
    <*> conId <*> inst <*> (tok "where" *> (coalesce <$> braceSep def)) >>= id);

ffiDecl = tok "ffi" *> (addFFI <$> litStr <*> var <*> (char ':' *> spch ':' *> _type));

tops = sepBy
  (   adt
  <|> classDecl
  <|> instDecl
  <|> ffiDecl
  <|> addDefs . coalesce <$> sepBy1 def (spch ';')
  <|> fixity
  <|> tok "export" *> (addExport <$> litStr <*> var)
  ) (spch ';');
program s = parse (between sp (spch ';' <|> pure ';') tops) $ ParseState s (insert (strCode ":") (5, RAssoc) Tip) internInit;

-- Primitives.

primAdts =
  [ addAdt (TC "Bool") [Constr (strCode "True") [], Constr (strCode "False") []]
  , addAdt (TAp (TC "[]") (TV "a")) [Constr (strCode "[]") [], Constr (strCode ":") [TV "a", TAp (TC "[]") (TV "a")]]
  , addAdt (TAp (TAp (TC ",") (TV "a")) (TV "b")) [Constr (strCode ",") [TV "a", TV "b"]]];

prims = let
  { ii = arr (TC "Int") (TC "Int")
  ; iii = arr (TC "Int") ii
  ; bin s = A (ro 'Q') (ro s) } in map (second (first noQual)) $
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin '='))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin 'L'))
    , ("charEq", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin '='))
    , ("charLE", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin 'L'))
    , ("if", (arr (TC "Bool") $ arr (TV "a") $ arr (TV "a") (TV "a"), ro 'I'))
    , ("()", (TC "()", ro 'K'))
    , ("chr", (arr (TC "Int") (TC "Char"), ro 'I'))
    , ("ord", (arr (TC "Char") (TC "Int"), ro 'I'))
    , ("succ", (ii, A (ro 'T') (A (E $ Const $ 1) (ro '+'))))
    , ("ioBind", (arr (TAp (TC "IO") (TV "a")) (arr (arr (TV "a") (TAp (TC "IO") (TV "b"))) (TAp (TC "IO") (TV "b"))), ro 'C'))
    , ("ioPure", (arr (TV "a") (TAp (TC "IO") (TV "a")), A (A (ro 'B') (ro 'C')) (ro 'T')))
    , ("newIORef", (arr (TV "a") (TAp (TC "IO") (TAp (TC "IORef") (TV "a"))), ro 'n'))
    , ("readIORef", (arr (TAp (TC "IORef") (TV "a")) (TAp (TC "IO") (TV "a")), ro 'r'))
    , ("writeIORef", (arr (TAp (TC "IORef") (TV "a")) (arr (TV "a") (TAp (TC "IO") (TC "()"))), ro 'w'))
    , ("exitSuccess", (TAp (TC "IO") (TV "a"), ro '.'))
    , ("unsafePerformIO", (arr (TAp (TC "IO") (TV "a")) (TV "a"), A (A (ro 'C') (A (ro 'T') (ro '?'))) (ro 'K')))
    , ("fail#", (TV "a", A (V $ strCode "unsafePerformIO") (V $ strCode "exitSuccess")))
    ] ++ map (\s -> (itemize s, (iii, bin s))) "+-*/%";

-- Conversion to De Bruijn indices.

data LC = Ze | Su LC | Pass Extra | PassVar Int | La LC | App LC LC;

debruijn n e = case e of
  { E x -> Pass x
  ; V v -> maybe (PassVar v) id $
    foldr (\h found -> if h == v then Just Ze else Su <$> found) Nothing n
  ; A x y -> App (debruijn n x) (debruijn n y)
  ; L s t -> La (debruijn (s:n) t)
  };

-- Kiselyov bracket abstraction.

data IntTree = Lf Extra | LfVar Int | Nd IntTree IntTree;
data Sem = Defer | Closed IntTree | Need Sem | Weak Sem;

lf = Lf . Basic;

ldef y = case y of
  { Defer -> Need $ Closed (Nd (Nd (lf 'S') (lf 'I')) (lf 'I'))
  ; Closed d -> Need $ Closed (Nd (lf 'T') d)
  ; Need e -> Need $ (Closed (Nd (lf 'S') (lf 'I'))) ## e
  ; Weak e -> Need $ (Closed (lf 'T')) ## e
  };

lclo d y = case y of
  { Defer -> Need $ Closed d
  ; Closed dd -> Closed $ Nd d dd
  ; Need e -> Need $ (Closed (Nd (lf 'B') d)) ## e
  ; Weak e -> Weak $ (Closed d) ## e
  };

lnee e y = case y of
  { Defer -> Need $ Closed (lf 'S') ## e ## Closed (lf 'I')
  ; Closed d -> Need $ Closed (Nd (lf 'R') d) ## e
  ; Need ee -> Need $ Closed (lf 'S') ## e ## ee
  ; Weak ee -> Need $ Closed (lf 'C') ## e ## ee
  };

lwea e y = case y of
  { Defer -> Need e
  ; Closed d -> Weak $ e ## Closed d
  ; Need ee -> Need $ (Closed (lf 'B')) ## e ## ee
  ; Weak ee -> Weak $ e ## ee
  };

x ## y = case x of
  { Defer -> ldef y
  ; Closed d -> lclo d y
  ; Need e -> lnee e y
  ; Weak e -> lwea e y
  };

babs t = case t of
  { Ze -> Defer
  ; Su x -> Weak (babs x)
  ; Pass x -> Closed (Lf x)
  ; PassVar s -> Closed (LfVar s)
  ; La t -> case babs t of
    { Defer -> Closed (lf 'I')
    ; Closed d -> Closed (Nd (lf 'K') d)
    ; Need e -> e
    ; Weak e -> Closed (lf 'K') ## e
    }
  ; App x y -> babs x ## babs y
  };

nolam x = (\(Closed d) -> d) $ babs $ debruijn [] x;

isLeaf t c = case t of { Lf (Basic n) -> n == c ; _ -> False };

optim t = case t of
  { Nd x y -> let { p = optim x ; q = optim y } in
    if isLeaf p 'I' then q else
    if isLeaf q 'I' then case p of
      { Lf (Basic c)
        | c == 'C' -> lf 'T'
        | c == 'B' -> lf 'I'
      ; Nd p1 p2 -> case p1 of
        { Lf (Basic c)
          | c == 'B' -> p2
          | c == 'R' -> Nd (lf 'T') p2
        ; _ -> Nd (Nd p1 p2) q
        }
      ; _ -> Nd p q
      } else
    if isLeaf q 'T' then case p of
      { Nd (Lf (Basic 'B')) (Lf (Basic 'C')) -> lf 'V'
      ; _ -> Nd p q
      } else Nd p q
  ; _ -> t
  };

freeCount v expr = case expr of
  { E _ -> 0
  ; V s -> if s == v then 1 else 0
  ; A x y -> freeCount v x + freeCount v y
  ; L w t -> if v == w then 0 else freeCount v t
  };

optiApp s x = let { n = freeCount s x } in
  if 2 <= n then A $ L s x else
    if 0 == n then const x else flip (beta s) x;
optiApp' t = case t of
  { A (L s x) y -> optiApp s (optiApp' x) (optiApp' y)
  ; A x y -> A (optiApp' x) (optiApp' y)
  ; L s x -> L s (optiApp' x)
  ; _ -> t
  };

-- Type checking.

apply sub t = case t of
  { TC v -> t
  ; TV v -> maybe t id $ lookup v sub
  ; TAp a b -> TAp (apply sub a) (apply sub b)
  };

(@@) s1 s2 = map (second (apply s1)) s2 ++ s1;

occurs s t = case t of
  { TC v -> False
  ; TV v -> s == v
  ; TAp a b -> occurs s a || occurs s b
  };

varBind s t = case t of
  { TC v -> Right [(s, t)]
  ; TV v -> Right $ if v == s then [] else [(s, t)]
  ; TAp a b -> if occurs s t then Left "occurs check" else Right [(s, t)]
  };

mgu t u = case t of
  { TC a -> case u of
    { TC b -> if a == b then Right [] else Left "TC-TC clash"
    ; TV b -> varBind b t
    ; TAp a b -> Left "TC-TAp clash"
    }
  ; TV a -> varBind a u
  ; TAp a b -> case u of
    { TC b -> Left "TAp-TC clash"
    ; TV b -> varBind b t
    ; TAp c d -> mgu a c >>= unify b d
    }
  };

unify a b s = (@@ s) <$> mgu (apply s a) (apply s b);

--instantiate' :: Type -> Int -> [(String, Type)] -> ((Type, Int), [(String, Type)])
instantiate' t n tab = case t of
  { TC s -> ((t, n), tab)
  ; TV s -> case lookup s tab of
    { Nothing -> let { va = TV (showInt n "") } in ((va, n + 1), (s, va):tab)
    ; Just v -> ((v, n), tab)
    }
  ; TAp x y ->
    fpair (instantiate' x n tab) \(t1, n1) tab1 ->
    fpair (instantiate' y n1 tab1) \(t2, n2) tab2 ->
    ((TAp t1 t2, n2), tab2)
  };

instantiatePred (Pred s t) ((out, n), tab) = first (first ((:out) . Pred s)) (instantiate' t n tab);

--instantiate :: Qual -> Int -> (Qual, Int)
instantiate (Qual ps t) n =
  fpair (foldr instantiatePred (([], n), []) ps) \(ps1, n1) tab ->
  first (Qual ps1) (fst (instantiate' t n1 tab));

proofApply sub a = case a of
  { Proof (Pred cl ty) -> Proof (Pred cl $ apply sub ty)
  ; A x y -> A (proofApply sub x) (proofApply sub y)
  ; L s t -> L s $ proofApply sub t
  ; _ -> a
  };

typeAstSub sub (t, a) = (apply sub t, proofApply sub a);

infer dcs typed loc ast csn = fpair csn \cs n ->
  let
    { va = TV (showInt n "")
    ; insta ty = fpair (instantiate ty n) \(Qual preds ty) n1 -> ((ty, foldl A ast (map Proof preds)), (cs, n1))
    }
  in case ast of
  { E x -> Right $ case x of
    { Basic 'Y' -> insta $ noQual $ arr (arr (TV "a") (TV "a")) (TV "a")
    ; Const _ -> ((TC "Int",  ast), csn)
    ; ChrCon _ -> ((TC "Char",  ast), csn)
    ; StrCon _ -> ((TAp (TC "[]") (TC "Char"),  ast), csn)
    }
  ; V s -> fmaybe (lookup s loc)
    (fmaybe (mlookup s typed) (error "depGraph bug!") $ Right . insta)
    \t -> Right ((t, ast), csn)
  ; A x y -> infer dcs typed loc x (cs, n + 1) >>=
    \((tx, ax), csn1) -> infer dcs typed loc y csn1 >>=
    \((ty, ay), (cs2, n2)) -> unify tx (arr ty va) cs2 >>=
    \cs -> Right ((va, A ax ay), (cs, n2))
  ; L s x -> first (\(t, a) -> (arr va t, L s a)) <$> infer dcs typed ((s, va):loc) x (cs, n + 1)
  };

instance Eq Type where
  { (TC s) == (TC t) = s == t
  ; (TV s) == (TV t) = s == t
  ; (TAp a b) == (TAp c d) = a == c && b == d
  ; _ == _ = False
  };

instance Eq Pred where { (Pred s a) == (Pred t b) = s == t && a == b };

filter f = foldr (\x xs -> if f x then x:xs else xs) [];
intersect xs ys = filter (\x -> fmaybe (find (x ==) ys) False (\_ -> True)) xs;

merge s1 s2 = if all (\v -> apply s1 (TV v) == apply s2 (TV v))
  $ map fst s1 `intersect` map fst s2 then Just $ s1 ++ s2 else Nothing;

match h t = case h of
  { TC a -> case t of
    { TC b | a == b -> Just []
    ; _ -> Nothing
    }
  ; TV a -> Just [(a, t)]
  ; TAp a b -> case t of
    { TAp c d -> case match a c of
      { Nothing -> Nothing
      ; Just ac -> case match b d of
        { Nothing -> Nothing
        ; Just bd -> merge ac bd
        }
      }
    ; _ -> Nothing
    }
  };

par f = ('(':) . f . (')':);
showType t = case t of
  { TC s -> (s++)
  ; TV s -> (s++)
  ; TAp (TAp (TC "->") a) b -> par $ showType a . (" -> "++) . showType b
  ; TAp a b -> par $ showType a . (' ':) . showType b
  };
showPred (Pred s t) = (s++) . (' ':) . showType t . (" => "++);

findInst ienv qn p@(Pred cl ty) insts = case insts of
  { [] -> fpair qn \q n -> Right (((p, n):q, n + 4), V n)
  ; (name, Qual ps h):is -> case match h ty of
    { Nothing -> findInst ienv qn p is
    ; Just subs -> foldM (\(qn1, t) (Pred cl1 ty1) -> second (A t)
      <$> findProof ienv (Pred cl1 $ apply subs ty1) qn1) (qn, V name) ps
  }};

findProof ienv pred psn@(ps, n) = case lookup pred ps of
  { Nothing -> case pred of { Pred s t -> case mlookup s ienv of
    { Nothing -> Left $ "no instances: " ++ s
    ; Just insts -> findInst ienv psn pred insts
    }}
  ; Just s -> Right (psn, V s)
  };

prove' ienv psn a = case a of
  { Proof pred -> findProof ienv pred psn
  ; A x y -> prove' ienv psn x >>= \(psn1, x1) ->
    second (A x1) <$> prove' ienv psn1 y
  ; L s t -> second (L s) <$> prove' ienv psn t
  ; _ -> Right (psn, a)
  };

dictVars ps n = flst ps ([], n) \p pt -> first ((p, n):) (dictVars pt $ n + 4);

todo s = "TODO!" ++ showInt s "";

-- The 4th argument: e.g. Qual [Eq a] "[a]" for Eq a => Eq [a].
inferMethod ienv dcs typed (Qual psi ti) (s, expr) =
  infer dcs typed [] expr ([], 5) >>=
  \(ta, (sub, n)) -> fpair (typeAstSub sub ta) \tx ax -> case mlookup s typed of
    { Nothing -> Left $ "no such method: " ++ todo s
    -- e.g. qc = Eq a => a -> a -> Bool
    -- We instantiate: Eq a1 => a1 -> a1 -> Bool.
    ; Just qc -> fpair (instantiate qc n) \(Qual [Pred _ headT] tc) n1 ->
      -- We mix the predicates `psi` with the type of `headT`, applying a
      -- substitution such as (a1, [a]) so the variable names match.
      -- e.g. Eq a => [a] -> [a] -> Bool
      -- Then instantiate and match.
      case match headT ti of { Just subc ->
        fpair (instantiate (Qual psi $ apply subc tc) n1) \(Qual ps2 t2) n2 ->
          case match tx t2 of
            { Nothing -> Left "class/instance type conflict"
            ; Just subx -> snd <$> prove' ienv (dictVars ps2 5) (proofApply subx ax)
          }}};

genProduct ds = foldr L (L 1 $ foldl A (V 1) $ map V ds) ds;

inferInst ienv dcs typed (name, (q@(Qual ps t), ds)) = let { dvs = map snd $ fst $ dictVars ps 5 } in
  (name,) . flip (foldr L) dvs . foldl A (genProduct $ map fst ds)
    <$> mapM (inferMethod ienv dcs typed q) ds;

singleOut s cs = \scrutinee x ->
  foldl A (A (V $ specialCase cs) scrutinee) $ map (\(Constr s' ts) ->
    if s == s' then x else foldr L vpjoin $ map (const 1) ts) cs;

patEq lit b x y = A (A (A (V $ strCode "if") (A (A (V $ strCode "==") (E lit)) b)) x) y;

unpat dcs as t = case as of
  { [] -> pure t
  ; a:at -> get >>= \n -> put (n + 4) >> L n <$> let
    { go p x = case p of
      { PatLit lit -> unpat dcs at $ patEq lit (V n) x vpjoin
      ; PatVar s m -> maybe (unpat dcs at) (\p1 x1 -> go p1 x1) m $ beta s (V n) x
      ; PatCon con args -> case mlookup con dcs of
        { Nothing -> error "bad data constructor"
        ; Just cons -> unpat dcs args x >>= \y -> unpat dcs at $ singleOut con cons (V n) y
        }
      }
    } in go a t
  };

unpatTop dcs als x = case als of
  { [] -> pure x
  ; (a, l):alt -> let
    { go p t = case p of
      { PatLit lit -> unpatTop dcs alt $ patEq lit (V l) t vpjoin
      ; PatVar s m -> maybe (unpatTop dcs alt) go m $ beta s (V l) t
      ; PatCon con args -> case mlookup con dcs of
        { Nothing -> error "bad data constructor"
        ; Just cons -> unpat dcs args t >>= \y -> unpatTop dcs alt $ singleOut con cons (V l) y
        }
      }
    } in go a x
  };

rewritePats' dcs asxs ls = case asxs of
  { [] -> pure $ V $ strCode "fail#"
  ; (as, t):asxt -> unpatTop dcs (zip as ls) t >>=
    \y -> A (L (strCode "pjoin#") y) <$> rewritePats' dcs asxt ls
  };

rewritePats dcs vsxs@((vs0, _):_) = get >>= \n -> let
  { ls = map (\k -> n + 4*k) $ take (length vs0) $ upFrom 0 }
  in put (n + 4 * length ls) >> flip (foldr L) ls <$> rewritePats' dcs vsxs ls;

classifyAlt v x = case v of
  { PatLit lit -> Left $ patEq lit (V 1) x
  ; PatVar s m -> maybe (Left . A . L (strCode "cjoin#")) classifyAlt m $ beta s (V 1) x
  ; PatCon s ps -> Right (insertWith (flip (.)) s ((ps, x):))
  };

genCase dcs tab = if size tab == 0 then id else A . L (strCode "cjoin#") $ let
  { firstC = flst (toAscList tab) undefined (\h _ -> fst h)
  ; cs = maybe (error $ "bad constructor: " ++ todo firstC) id $ mlookup firstC dcs
  } in foldl A (A (V $ specialCase cs) (V 1))
    $ map (\(Constr s ts) -> case mlookup s tab of
      { Nothing -> foldr L (V (strCode "cjoin#")) $ const 1 <$> ts
      ; Just f -> Pa $ f [(const (PatVar 1 Nothing) <$> ts, V $ strCode "cjoin#")]
      }) cs;

updateCaseSt dcs (acc, tab) alt = case alt of
  { Left f -> (acc . genCase dcs tab . f, Tip)
  ; Right upd -> (acc, upd tab)
  };

rewriteCase dcs as = fpair (foldl (updateCaseSt dcs) (id, Tip) $ uncurry classifyAlt <$> as) \acc tab ->
  acc . genCase dcs tab $ V $ strCode "fail#";

secondM f (a, b) = (a,) <$> f b;
rewritePatterns dcs = let {
  go t = case t of
    { E _ -> pure t
    ; V _ -> pure t
    ; A x y -> liftA2 A (go x) (go y)
    ; L s x -> L s <$> go x
    ; Pa vsxs -> mapM (secondM go) vsxs >>= rewritePats dcs
    ; Ca x as -> liftA2 A (L 1 . rewriteCase dcs <$> mapM (secondM go) as >>= go) (go x)
    }
  } in \case
  { Left (s, t) -> Left (s, optiApp' $ evalState (go t) 3)
  ; Right (cl, (q, ds)) -> Right (cl, (q, second (\t -> optiApp' $ evalState (go t) 3) <$> ds))
  };

union xs ys = foldr (\y acc -> (if elem y acc then id else (y:)) acc) xs ys;
fv bound = \case
  { V s | not (elem s bound) -> [s]
  ; A x y -> fv bound x `union` fv bound y
  ; L s t -> fv (s:bound) t
  ; _ -> []
  };

depGraph typed (s, ast) (vs, es) = (insert s ast vs,
  foldr (\k ios@(ins, outs) -> case lookup k typed of
    { Nothing -> (insertWith union k [s] ins, insertWith union s [k] outs)
    ; Just _ -> ios
    }) es $ fv [] ast);

depthFirstSearch = (foldl .) \relation st@(visited, sequence) vertex ->
  if vertex `elem` visited then st else second (vertex:)
    $ depthFirstSearch relation (vertex:visited, sequence) (relation vertex);

spanningSearch   = (foldl .) \relation st@(visited, setSequence) vertex ->
  if vertex `elem` visited then st else second ((:setSequence) . (vertex:))
    $ depthFirstSearch relation (vertex:visited, []) (relation vertex);

scc ins outs = let
  { depthFirst = snd . depthFirstSearch outs ([], [])
  ; spanning   = snd . spanningSearch   ins  ([], [])
  } in spanning . depthFirst;

inferno prove dcs typed defmap syms = let
  { loc = zip syms $ TV . (' ':) . flip showInt "" <$> syms
  } in foldM (\(acc, (subs, n)) s ->
      maybe (Left $ "missing: " ++ todo s) Right (mlookup s defmap) >>=
      \expr -> infer dcs typed loc expr (subs, n) >>=
      \((t, a), (ms, n1)) -> unify (TV (' ':showInt s "")) t ms >>=
      \cs -> Right ((s, (t, a)):acc, (cs, n1))
    ) ([], ([], 0)) syms >>=
    \(stas, (soln, _)) -> mapM id $ (\(s, ta) -> prove s $ typeAstSub soln ta) <$> stas;

prove ienv s (t, a) = flip fmap (prove' ienv ([], 5) a) \((ps, _), x) ->
  let { applyDicts expr = foldl A expr $ map (V . snd) ps }
  in (s, (Qual (map fst ps) t, foldr L (overFree s applyDicts x) $ map snd ps));
inferDefs' ienv dcs defmap (typeTab, lambF) syms = let
  { add stas = foldr (\(s, (q, cs)) (tt, f) -> (insert s q tt, f . ((s, cs):))) (typeTab, lambF) stas
  } in add <$> inferno (prove ienv) dcs typeTab defmap syms
  ;
inferDefs ienv defs dcs typed = let
  { typeTab = foldr (\(k, (q, _)) -> insert k q) Tip typed
  ; lambs = second snd <$> typed
  ; plains = rewritePatterns dcs <$> defs
  ; lrs = foldr (either (\def -> (first (def:) .)) (\i -> (second (i:) .))) id plains ([], [])
  ; defmapgraph = foldr (depGraph typed) (Tip, (Tip, Tip)) $ fst lrs
  ; defmap = fst defmapgraph
  ; graph = snd defmapgraph
  ; ins k = maybe [] id $ mlookup k $ fst graph
  ; outs k = maybe [] id $ mlookup k $ snd graph
  ; mainLambs = foldM (inferDefs' ienv dcs defmap) (typeTab, (lambs++)) $ scc ins outs $ map fst $ toAscList defmap
  } in case mainLambs of
  { Left err -> Left err
  ; Right (tt, lambF) -> (\instLambs -> (tt, lambF . (instLambs++))) <$> mapM (inferInst ienv dcs tt) (snd lrs)
  };

last' x xt = flst xt x \y yt -> last' y yt;
last xs = flst xs undefined last';
init (x:xt) = flst xt [] \_ _ -> x : init xt;
intercalate sep xs = flst xs [] \x xt -> x ++ concatMap (sep ++) xt;
intersperse sep xs = flst xs [] \x xt -> x : foldr ($) [] (((sep:) .) . (:) <$> xt);

argList t = case t of
  { TC s -> [TC s]
  ; TV s -> [TV s]
  ; TAp (TC "IO") (TC u) -> [TC u]
  ; TAp (TAp (TC "->") x) y -> x : argList y
  };

cTypeName (TC "()") = "void";
cTypeName (TC "Int") = "int";
cTypeName (TC "Char") = "char";

ffiDeclare (name, t) = let { tys = argList t } in concat
  [cTypeName $ last tys, " ", name, "(", intercalate "," $ cTypeName <$> init tys, ");\n"];

ffiArgs n t = case t of
  { TC s -> ("", ((True, s), n))
  ; TAp (TC "IO") (TC u) -> ("", ((False, u), n))
  ; TAp (TAp (TC "->") x) y -> first (((if 3 <= n then ", " else "") ++ "num(" ++ showInt n ")") ++) $ ffiArgs (n + 1) y
  };

ffiDefine n ffis = case ffis of
  { [] -> id
  ; (name, t):xt -> fpair (ffiArgs 2 t) \args ((isPure, ret), count) -> let
    { lazyn = ("lazy(" ++) . showInt (if isPure then count - 1 else count + 1) . (", " ++)
    ; aa tgt = "app(arg(" ++ showInt (count + 1) "), " ++ tgt ++ "), arg(" ++ showInt count ")"
    ; longDistanceCall = name ++ "(" ++ args ++ ")"
    } in
    ("case " ++) . showInt n . (": " ++) . if ret == "()"
      then (longDistanceCall ++) . (';':) . lazyn . (((if isPure then "'I', 'K'" else aa "'K'") ++ "); break;") ++) . ffiDefine (n - 1) xt
      else lazyn . (((if isPure then "'#', " ++ longDistanceCall else aa $ "app('#', " ++ longDistanceCall ++ ")") ++ "); break;") ++) . ffiDefine (n - 1) xt
  };

getContents = getChar >>= \n -> if n <= 255 then (chr n:) <$> getContents else pure [];

swp (a, b) = (b, a);
unintern s m
  | 1000000 <= s = maybe (error $ "BUG! " ++ showInt s "") ('|':) $ mlookup (s - 1000000) m
  | True = maybe ('#':showInt s "") id $ mlookup s m
  ;

untangle s = fmaybe (program s) (Left "parse error") \(prog, rest) -> case rest of
  { ParseState s _ (_, strmap) -> if s == ""
    then fneat (foldr ($) (Neat Tip [] (first strCode <$> prims) Tip [] []) $ primAdts ++ prog) \ienv fs typed dcs ffis exs ->
      case inferDefs ienv fs dcs typed of
        { Left err -> Left err
        ; Right qas -> Right (fromList $ swp <$> toAscList strmap, (qas, (ffis, exs)))
        }
    else Left $ "dregs: " ++ s
  };

optiComb' (subs, combs) (s, lamb) = let
  { gosub t = case t of
    { LfVar v -> maybe t id $ lookup v subs
    ; Nd a b -> Nd (gosub a) (gosub b)
    ; _ -> t
    }
  ; c = optim $ gosub $ nolam $ optiApp' lamb
  ; combs' = combs . ((s, c):)
  } in case c of
  { Lf (Basic b) -> ((s, c):subs, combs')
  ; LfVar v -> if v == s then (subs, combs . ((s, lf '.'):)) else ((s, gosub c):subs, combs')
  ; _ -> (subs, combs')
  };
optiComb lambs = ($[]) . snd $ foldl optiComb' ([], id) lambs;

genMain n = "int main(int argc,char**argv){env_argc=argc;env_argv=argv;rts_init();rts_reduce(" ++ showInt n ");return 0;}\n";

compile s = case untangle s of
  { Left err -> err
  ; Right (_, ((_, lambF), (ffis, exs))) -> fpair (hashcons $ optiComb $ lambF []) \tab mem ->
    (concatMap ffiDeclare ffis ++) .
    ("static void foreign(u n) {\n  switch(n) {\n" ++) .
    ffiDefine (length ffis - 1) ffis .
    ("\n  }\n}\n" ++) .
    ("static const u prog[]={" ++) .
    foldr (.) id (map (\n -> showInt n . (',':)) mem) .
    ("};\nstatic const u prog_size=sizeof(prog)/sizeof(*prog);\n" ++) .
    ("static u root[]={" ++) .
    foldr (\(x, y) f -> maybe undefined showInt (mlookup y tab) . (", " ++) . f) id exs .
    ("};\n" ++) .
    ("static const u root_size=" ++) . showInt (length exs) . (";\n" ++) .
    (foldr (.) id $ zipWith (\p n -> (("EXPORT(f" ++ showInt n ", \"" ++ fst p ++ "\", " ++ showInt n ")\n") ++)) exs (upFrom 0)) $
    maybe "" genMain (mlookup (strCode "main") tab)
  };

showVar s@(h:_) = (if elem h ":!#$%&*+./<=>?@\\^|-~" then par else id) (s++);

showAst strs prec t = case t of
  { E n -> case n of
    { Basic i -> (i:)
    ; Const i -> showInt i
    ; ChrCon c -> ('\'':) . (c:) . ('\'':)
    ; StrCon s -> ('"':) . (s++) . ('"':)
    }
  ; V s -> showVar $ unintern s strs
  ; A (E (Basic 'F')) (E (Basic c)) -> ("FFI_"++) . showInt (ord c)
  ; A x y -> (if prec then par else id) (showAst strs False x . (' ':) . showAst strs True y)
  ; L s t -> par $ ('\\':) . (unintern s strs++) . (" -> "++) . showAst strs prec t
  ; _ -> ("TODO"++)
  };

showTree strs prec t = case t of
  { LfVar s -> showVar $ unintern s strs
  ; Lf n -> case n of
    { Basic i -> (i:)
    ; Const i -> showInt i
    ; ChrCon c -> ('\'':) . (c:) . ('\'':)
    ; StrCon s -> ('"':) . (s++) . ('"':)
    }
  ; Nd (Lf (Basic 'F')) (Lf (Basic c)) -> ("FFI_"++) . showInt (ord c)
  ; Nd x y -> (if prec then par else id) (showTree strs False x . (' ':) . showTree strs True y)
  };
disasm strs (s, t) = (unintern s strs++) . (" = "++) . showTree strs False t . (";\n"++);

dumpCombs s = case untangle s of
  { Left err -> err
  ; Right (strs, ((_, lambF), _)) -> foldr ($) [] $ map (disasm strs) $ optiComb $ lambF []
  };

dumpLambs s = case untangle s of
  { Left err -> err
  ; Right (strs, ((_, lambF), _)) -> foldr ($) [] $
    (\(s, t) -> (unintern s strs++) . (" = "++) . showAst strs False t . ('\n':)) <$> lambF []
  };

showQual (Qual ps t) = foldr (.) id (map showPred ps) . showType t;

dumpTypes s = case untangle s of
  { Left err -> err
  ; Right (strs, ((typed, _), _)) -> ($ "") $ foldr (.) id $
    map (\(s, q) -> (unintern s strs++) . (" :: "++) . showQual q . ('\n':)) $ toAscList typed
  };

data MemKey = VarKey Int | NdKey (Either Int Int) (Either Int Int);
instance Eq (Either Int Int) where
{ (Left a) == (Left b) = a == b
; (Right a) == (Right b) = a == b
; _ == _ = False
};
instance Ord (Either Int Int) where
{ x <= y = case x of
  { Left a -> case y of
    { Left b -> a <= b
    ; Right _ -> True
    }
  ; Right a -> case y of
    { Left _ -> False
    ; Right b -> a <= b
    }
  }
};

instance Eq MemKey where
{ (VarKey a) == (VarKey b) = a == b
; (NdKey a1 b1) == (NdKey a2 b2) = a1 == a2 && b1 == b2
; _ == _ = False
};
instance Ord MemKey where
{ x <= y = case x of
  { VarKey a -> case y of
    { VarKey b -> a <= b
    ; NdKey _ _ -> True
    }
  ; NdKey a1 b1 -> case y of
    { VarKey _ -> False
    ; NdKey a2 b2 | a1 <= a2 -> if a1 == a2 then b1 <= b2 else True
                  | True -> False
    }
  }
};

memget k = get >>=
  \((n, tab), (hp, f)) -> case mlookup k tab of
  { Nothing -> case k of
    { NdKey a b -> put ((n, insert k hp tab), (hp + 2, f . (a:) . (b:))) >> pure hp
    ; VarKey v -> put ((n + 2, insert k n tab), (hp, f)) >> pure n
    }
  ; Just v -> pure v
  };

enc t = case t of
  { Lf n -> case n of
    { Basic c -> pure $ Right $ ord c
    ; Const c -> Right <$> memget (NdKey (Right 35) (Right c))
    ; ChrCon c -> enc $ Lf $ Const $ ord c
    ; StrCon s -> enc $ foldr (\h t -> Nd (Nd (lf ':') (Lf $ ChrCon h)) t) (lf 'K') s
    }
  ; LfVar s -> Left <$> memget (VarKey s)
  ; Nd x y ->
    enc x >>=
    \hx -> enc y >>=
    \hy -> Right <$> memget (NdKey hx hy)
  };

asm combs = foldM
  (\(symtab, ntab) (s, t) -> memget (VarKey s) >>=
    \n -> enc t >>=
    \ea -> let
      { a = either (maybe undefined id . (`mlookup` ntab)) id ea
      } in pure (insert s a symtab, insert n a ntab))
  (Tip, Tip) combs;

hashcons combs = fpair (runState (asm combs) ((129, Tip), (128, id)))
  \(symtab, ntab) (_, (_, f)) -> (symtab,) $ either (maybe undefined id . (`mlookup` ntab)) id <$> f [];

getArg' k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> getArg' (k + 1) n;
getArgs = getArgCount >>= \n -> mapM (getArg' 0) (take (n - 1) $ upFrom 1);

interact f = getContents >>= putStr . f;
main = getArgs >>= \case
  { "comb":_ -> interact dumpCombs
  ; "lamb":_ -> interact dumpLambs
  ; "type":_ -> interact dumpTypes
  ; _ -> interact compile
  };
