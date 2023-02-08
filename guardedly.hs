-- Guards.
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

foreign import ccall "putchar" putChar :: Int -> IO Int;
foreign import ccall "getchar" getChar :: IO Int;

class Functor f where { fmap :: (a -> b) -> f a -> f b };
class Applicative f where
{ pure :: a -> f a
; (<*>) :: f (a -> b) -> f a -> f b
};
class Monad m where
{ return :: a -> m a
; (>>=) :: m a -> (a -> m b) -> m b
};
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
compare x y = case x <= y of
  { True -> case y <= x of
    { True -> EQ
    ; False -> LT
    }
  ; False -> GT
  };
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
foldr c n l = flst l n (\h t -> c h(foldr c n t));
length = foldr (\_ n -> n + 1) 0;
mapM_ f = foldr ((>>) . f) (pure ());
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
wrap c = c:[];
map = flip (foldr . ((:) .)) [];
instance Functor [] where { fmap = map };
concatMap = (concat .) . map;
lookup s = foldr (\(k, v) t -> if s == k then Just v else t) Nothing;
all f = foldr (&&) True . map f;
any f = foldr (||) False . map f;
upFrom n = n : upFrom (n + 1);
zipWith f xs ys = flst xs [] $ \x xt -> flst ys [] $ \y yt -> f x y : zipWith f xt yt;
zip = zipWith (,);

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
balance k x l r = case size l + size r <= 1 of
  { True -> node
  ; False -> case 5 * size l + 3 <= 2 * size r of
    { True -> case r of
      { Tip -> node
      ; Bin sz _ _ rl rr -> case 2 * size rl + 1 <= 3 * size rr of
        { True -> singleL
        ; False -> doubleL
        }
      }
    ; False -> case 5 * size r + 3 <= 2 * size l of
      { True -> case l of
        { Tip -> node
        ; Bin sz _ _ ll lr -> case 2 * size lr + 1 <= 3 * size ll of
          { True -> singleR
          ; False -> doubleR
          }
        }
      ; False -> node
      }
    }
  } k x l r;
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
data Extra = Basic Int | Const Int | StrCon String | Proof Pred;
data Pat = PatPred Ast | PatVar String (Maybe Pat) | PatCon String [Pat];
data Ast = E Extra | V String | A Ast Ast | L String Ast | Pa [([Pat], [(Ast, Ast)])] | Ca Ast [(Pat, [(Ast, Ast)])];
data Parser a = Parser (String -> Maybe (a, String));
data Constr = Constr String [Type];
data Pred = Pred String Type;
data Qual = Qual [Pred] Type;
noQual = Qual [];

data Neat = Neat
  -- | Instance environment.
  (Map String [Qual])
  -- | Either top-level or instance definitions.
  [Either (String, Ast) (String, (Qual, [(String, Ast)]))]
  -- | Typed ASTs, ready for compilation, including ADTs and methods,
  -- e.g. (==), (Eq a => a -> a -> Bool, select-==)
  [(String, (Qual, Ast))]
  -- | Data constructor table.
  (Map String [Constr])  -- AdtTab
  -- | FFI declarations.
  [(String, Type)]
  -- | Exports.
  [(String, String)]
  ;
fneat (Neat a b c d e f) z = z a b c d e f;

ro = E . Basic . ord;
conOf (Constr s _) = s;
specialCase = concatMap (('|':) . conOf);
mkCase t cs = (specialCase cs,
  ( noQual $ arr t $ foldr arr (TV "case") $ map (\(Constr _ ts) -> foldr arr (TV "case") ts) cs
  , ro 'I'));
mkStrs = snd . foldl (\(s, l) u -> ('@':s, s:l)) ("@", []);
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs);
scottConstr t cs c = case c of { Constr s ts -> (s,
  ( noQual $ foldr arr t ts
  , scottEncode (map conOf cs) s $ mkStrs ts)) };
mkAdtDefs t cs = mkCase t cs : map (scottConstr t cs) cs;

mkSel ms s = L "@" $ A (V "@") $ foldr L (V $ '*':s) $ map (('*':) . fst) ms;

showInt' n = if 0 == n then id else (showInt' $ n/10) . ((:) (chr $ 48+n%10));
showInt n = if 0 == n then ('0':) else showInt' n;

mkFFIHelper n t acc = case t of
  { TC s -> acc
  ; TAp (TC "IO") _ -> acc
  ; TAp (TAp (TC "->") x) y -> L (showInt n "") $ mkFFIHelper (n + 1) y $ A (V $ showInt n "") acc
  };

updateDcs cs dcs = foldr (\(Constr s _) m -> insert s cs m) dcs cs;
addAdt t cs acc = fneat acc \ienv fs typed dcs ffis exs ->
  Neat ienv fs (mkAdtDefs t cs ++ typed) (updateDcs cs dcs) ffis exs;
addClass classId v ms acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv fs
  (map (\(s, t) -> (s, (Qual [Pred classId v] t, mkSel ms s))) ms ++ typed) dcs ffis exs;
addInst cl q ds acc = fneat acc \ienv fs typed dcs ffis exs -> Neat (insertWith (++) cl [q] ienv) (Right (cl, (q, ds)):fs) typed dcs ffis exs;
addFFI foreignname ourname t acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv fs
  ((ourname, (Qual [] t, mkFFIHelper 0 t $ A (ro 'F') (ro $ chr $ length ffis))) : typed) dcs ((foreignname, t):ffis) exs;
addDefs ds acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv (map Left ds ++ fs) typed dcs ffis exs;
addExport e f acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv fs typed dcs ffis ((e, f):exs);

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

sat' f = \h t -> if f h then Just (h, t) else Nothing;
sat f = Parser \inp -> flst inp Nothing (sat' f);

instance Functor Parser where { fmap f x = pure f <*> x };
(<|>) x y = Parser \inp -> case parse x inp of
  { Nothing -> parse y inp
  ; Just at -> Just at
  };
(<$>) = fmap;
liftA2 f x y = f <$> x <*> y;
(*>) = liftA2 \x y -> y;
(<*) = liftA2 \x y -> x;
many p = liftA2 (:) p (many p) <|> pure [];
some p = liftA2 (:) p (many p);
sepBy1 p sep = liftA2 (:) p (many (sep *> p));
sepBy p sep = sepBy1 p sep <|> pure [];

char c = sat (c ==);
between x y p = x *> (p <* y);
com = char '-' *> between (char '-') (char '\n') (many $ sat ('\n' /=));
sp = many ((wrap <$> (sat (\c -> (c == ' ') || (c == '\n')))) <|> com);
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
con = conId <|> paren conSym;
var = varId <|> paren varSym;
op = varSym <|> conSym <|> between (spch '`') (spch '`') (conId <|> varId);
conop = conSym <|> between (spch '`') (spch '`') conId;
escChar = char '\\' *> ((sat (\c -> elem c "'\"\\")) <|> ((\c -> '\n') <$> char 'n'));
litOne delim = escChar <|> sat (delim /=);
litInt = Const . foldl (\n d -> 10*n + ord d - ord '0') 0 <$> spc (some digit);
litChar = Const . ord <$> between (char '\'') (spch '\'') (litOne '\'');
litStr = between (char '"') (spch '"') $ many (litOne '"');
lit = E <$> (StrCon <$> litStr <|> litChar <|> litInt);
sqLst r = between (spch '[') (spch ']') $ sepBy r (spch ',');
want f s = wantWith (s ==) f;
tok s = spc $ want (some (char '_' <|> symbo) <|> varLex) s;

gcon = conId <|> paren (conSym <|> (wrap <$> spch ',')) <|> ((:) <$> spch '[' <*> (wrap <$> spch ']'));

apat' r = PatVar <$> var <*> (tok "@" *> (Just <$> apat' r) <|> pure Nothing)
  <|> flip PatCon [] <$> gcon
  <|> PatPred . A (V "if#") . A (V "==") <$> lit
  <|> foldr (\h t -> PatCon ":" [h, t]) (PatCon "[]" []) <$> sqLst r
  <|> paren ((&) <$> r <*> ((spch ',' *> ((\y x -> PatCon "," [x, y]) <$> r)) <|> pure id))
  ;
pat = PatCon <$> gcon <*> many (apat' pat)
  <|> (&) <$> apat' pat <*> ((\s r l -> PatCon s [l, r]) <$> conop <*> apat' pat <|> pure id);
apat = apat' pat;
guards s r = tok s *> (wrap . (V "True",) <$> r) <|> some ((,) <$> (spch '|' *> r) <*> (tok s *> r));
alt r = (,) <$> pat <*> guards "->" r;
braceSep f = between (spch '{') (spch '}') (sepBy f (spch ';'));
alts r = braceSep (alt r);
cas r = Ca <$> between (tok "case") (tok "of") r <*> alts r;
lamCase r = tok "case" *> (L "\\case" . Ca (V "\\case") <$> alts r);
onePat vs x = Pa [(vs, x)];
lam r = spch '\\' *> (lamCase r <|> liftA2 onePat (some apat) (tok "->" *> (wrap . (V "True",) <$> r)));

flipPairize y x = A (A (V ",") x) y;
thenComma r = spch ',' *> ((flipPairize <$> r) <|> pure (A (V ",")));
parenExpr r = (&) <$> r <*> (((\v a -> A (V v) a) <$> op) <|> thenComma r <|> pure id);
rightSect r = ((\v a -> L "@" $ A (A (V v) $ V "@") a) <$> (op <|> (wrap <$> spch ','))) <*> r;
section r = spch '(' *> (parenExpr r <* spch ')' <|> rightSect r <* spch ')' <|> spch ')' *> pure (V "()"));

isFreePat v = \case
  { PatPred _ -> False
  ; PatVar s m -> s == v || maybe False (isFreePat v) m
  ; PatCon _ args -> any (isFreePat v) args
  };

isFree v expr = case expr of
  { E _ -> False
  ; V s -> s == v
  ; A x y -> isFree v x || isFree v y
  ; L w t -> v /= w && isFree v t
  ; Pa vsts -> any (\(vs, gs) -> not (any (isFreePat v) vs) && any (\(g, t) -> isFree v g || isFree v t) gs) vsts
  ; Ca x as -> isFree v x || isFree v (Pa $ first (:[]) <$> as)
  };

overFree s f t = case t of
  { E _ -> t
  ; V s' -> if s == s' then f t else t
  ; A x y -> A (overFree s f x) (overFree s f y)
  ; L s' t' -> if s == s' then t else L s' $ overFree s f t'
  ; Pa vsxs -> Pa $ map (\vsx@(vs, gs) -> if any (isFreePat s) vs then vsx else (vs, map (\(g, t) -> (overFree s f g, overFree s f t)) gs)) vsxs
  ; Ca x as -> Ca (overFree s f x) $ map (\vx@(v, gs) -> if isFreePat s v then vx else (v, map (\(g, t) -> (overFree s f g, overFree s f t)) gs)) as
  };

beta s t x = overFree s (const t) x;

maybeFix s x = if isFree s x then A (ro 'Y') (L s x) else x;

opDef x f y rhs = (f, onePat [x, y] rhs);

coalesce ds = flst ds [] \h@(s, x) t -> flst t [h] \(s', x') t' -> let
  { f (Pa vsts) (Pa vsts') = Pa $ vsts ++ vsts'
  ; f _ _ = error "bad multidef"
  } in if s == s' then coalesce $ (s, f x x'):t' else h:coalesce t;

def r = opDef <$> apat <*> varSym <*> apat <*> guards "=" r
  <|> liftA2 (,) var (liftA2 onePat (many apat) (guards "=" r));

addLets ls x = foldr (\(name, def) t -> A (L name t) $ maybeFix name def) x ls;
letin r = addLets <$> between (tok "let") (tok "in") (coalesce <$> braceSep (def r)) <*> r;
ifthenelse r = (\a b c -> A (A (A (V "if") a) b) c) <$>
  (tok "if" *> r) <*> (tok "then" *> r) <*> (tok "else" *> r);
listify = foldr (\h t -> A (A (V ":") h) t) (V "[]");
anyChar = sat \_ -> True;
rawBody = (char '|' *> char ']' *> pure []) <|> (:) <$> anyChar <*> rawBody;
rawQQ = spc $ char '[' *> char 'r' *> char '|' *> (E . StrCon <$> rawBody);
atom r = ifthenelse r <|> letin r <|> rawQQ <|> listify <$> sqLst r <|> section r <|> cas r <|> lam r <|> (paren (spch ',') *> pure (V ",")) <|> fmap V (con <|> var) <|> lit;
aexp r = fmap (foldl1 A) (some (atom r));
fix f = f (fix f);

data Assoc = NAssoc | LAssoc | RAssoc;
instance Eq Assoc where
{ NAssoc == NAssoc = True
; LAssoc == LAssoc = True
; RAssoc == RAssoc = True
; _ == _ = False
};
precOf s precTab = fmaybe (lookup s precTab) 9 fst;
assocOf s precTab = fmaybe (lookup s precTab) LAssoc snd;
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
expr precTab = fix \r n -> if n <= 9
  then liftA2 (opFold precTab) (r $ succ n) (many (liftA2 (,) (opWithPrec precTab n) (r $ succ n)))
  else aexp (r 0);

bType r = foldl1 TAp <$> some r;
_type r = foldr1 arr <$> sepBy (bType r) (spc (tok "->"));
typeConst = (\s -> if s == "String" then TAp (TC "[]") (TC "Int") else TC s) <$> conId;
aType = spch '(' *> (spch ')' *> pure (TC "()") <|> ((&) <$> _type aType <*> ((spch ',' *> ((\a b -> TAp (TAp (TC ",") b) a) <$> _type aType)) <|> pure id)) <* spch ')') <|>
  typeConst <|> (TV <$> varId) <|>
  (spch '[' *> (spch ']' *> pure (TC "[]") <|> TAp (TC "[]") <$> (_type aType <* spch ']')));

simpleType c vs = foldl TAp (TC c) (map TV vs);

-- Can we reduce backtracking here?
constr = (\x c y -> Constr c [x, y]) <$> aType <*> conSym <*> aType
  <|> Constr <$> conId <*> many aType;

adt = addAdt <$> between (tok "data") (spch '=') (simpleType <$> conId <*> many varId) <*> sepBy constr (spch '|');

prec = (\c -> ord c - ord '0') <$> spc digit;
fixityList a n os = map (\o -> (o, (n, a))) os;
fixityDecl kw a = between (tok kw) (spch ';') (fixityList a <$> prec <*> sepBy op (spch ','));
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc;

genDecl = (,) <$> var <*> (char ':' *> spch ':' *> _type aType);
classDecl = tok "class" *> (addClass <$> conId <*> (TV <$> varId) <*> (tok "where" *> braceSep genDecl));

inst = _type aType;
instDecl r = tok "instance" *>
  ((\ps cl ty defs -> addInst cl (Qual ps ty) defs) <$>
  (((wrap .) . Pred <$> conId <*> (inst <* tok "=>")) <|> pure [])
    <*> conId <*> inst <*> (tok "where" *> (coalesce <$> braceSep (def r))));

tops precTab = sepBy
  (   adt
  <|> classDecl
  <|> instDecl (expr precTab 0)
  <|> tok "foreign" *>
    ( tok "import" *> var *> (addFFI <$> litStr <*> var <*> (char ':' *> spch ':' *> _type aType))
    <|> tok "export" *> var *> (addExport <$> litStr <*> var)
    )
  <|> addDefs . coalesce <$> sepBy1 (def $ expr precTab 0) (spch ';')
  ) (spch ';');
program' = sp *> (((":", (5, RAssoc)):) . concat <$> many fixity) >>= tops;

-- Primitives.

program = parse $ (
  [ addAdt (TC "Bool") [Constr "True" [], Constr "False" []]
  , addAdt (TAp (TC "[]") (TV "a")) [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]]
  , addAdt (TAp (TAp (TC ",") (TV "a")) (TV "b")) [Constr "," [TV "a", TV "b"]]] ++) <$> program';

prims = let
  { ii = arr (TC "Int") (TC "Int")
  ; iii = arr (TC "Int") ii
  ; bin s = A (ro 'Q') (ro s) } in map (second (first noQual)) $
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin '='))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin 'L'))
    , ("charEq", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin '='))
    , ("charLE", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin 'L'))
    , ("if", (arr (TC "Bool") $ arr (TV "a") $ arr (TV "a") (TV "a"), ro 'I'))
    -- Pattern matching helper:
    , ("if#", (arr (arr (TV "a") $ TC "Bool") $ arr (TV "a") $ arr (TV "b") $ arr (TV "b") (TV "b"), ro 'I'))
    , ("()", (TC "()", ro 'K'))
    , ("chr", (ii, ro 'I'))
    , ("ord", (ii, ro 'I'))
    , ("succ", (ii, A (ro 'T') (A (E $ Const $ 1) (ro '+'))))
    , ("ioBind", (arr (TAp (TC "IO") (TV "a")) (arr (arr (TV "a") (TAp (TC "IO") (TV "b"))) (TAp (TC "IO") (TV "b"))), ro 'C'))
    , ("ioPure", (arr (TV "a") (TAp (TC "IO") (TV "a")), ro 'V'))
    , ("fail#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    , ("exitSuccess", (TAp (TC "IO") (TV "a"), ro '.'))
    , ("unsafePerformIO", (arr (TAp (TC "IO") (TV "a")) (TV "a"), A (A (ro 'C') (A (ro 'T') (ro '?'))) (ro 'K')))
    ] ++ map (\s -> (wrap s, (iii, bin s))) "+-*/%";

-- Conversion to De Bruijn indices.

data LC = Ze | Su LC | Pass Int | La LC | App LC LC;

debruijn m n e = case e of
  { E x -> case x of
    { Basic b -> Pass b
    ; Const c -> App (Pass $ ord '#') (Pass c)
    ; StrCon s -> foldr (\h t -> App (App (Pass $ ord ':') (App (Pass $ ord '#') (Pass $ ord h))) t) (Pass $ ord 'K') s
    ; Proof _ -> undefined
    }
  ; V v -> maybe (fmaybe (mlookup v m) undefined Pass) id $
    foldr (\h found -> if h == v then Just Ze else Su <$> found) Nothing n
  ; A x y -> App (debruijn m n x) (debruijn m n y)
  ; L s t -> La (debruijn m (s:n) t)
  };

-- Kiselyov bracket abstraction.

data IntTree = Lf Int | Nd IntTree IntTree;
data Sem = Defer | Closed IntTree | Need Sem | Weak Sem;

lf = Lf . ord;

ldef = \r y -> case y of
  { Defer -> Need (Closed (Nd (Nd (lf 'S') (lf 'I')) (lf 'I')))
  ; Closed d -> Need (Closed (Nd (lf 'T') d))
  ; Need e -> Need (r (Closed (Nd (lf 'S') (lf 'I'))) e)
  ; Weak e -> Need (r (Closed (lf 'T')) e)
  };

lclo = \r d y -> case y of
  { Defer -> Need (Closed d)
  ; Closed dd -> Closed (Nd d dd)
  ; Need e -> Need (r (Closed (Nd (lf 'B') d)) e)
  ; Weak e -> Weak (r (Closed d) e)
  };

lnee = \r e y -> case y of
  { Defer -> Need (r (r (Closed (lf 'S')) e) (Closed (lf 'I')))
  ; Closed d -> Need (r (Closed (Nd (lf 'R') d)) e)
  ; Need ee -> Need (r (r (Closed (lf 'S')) e) ee)
  ; Weak ee -> Need (r (r (Closed (lf 'C')) e) ee)
  };

lwea = \r e y -> case y of
  { Defer -> Need e
  ; Closed d -> Weak (r e (Closed d))
  ; Need ee -> Need (r (r (Closed (lf 'B')) e) ee)
  ; Weak ee -> Weak (r e ee)
  };

babsa x y = case x of
  { Defer -> ldef babsa y
  ; Closed d -> lclo babsa d y
  ; Need e -> lnee babsa e y
  ; Weak e -> lwea babsa e y
  };

babs t = case t of
  { Ze -> Defer
  ; Su x -> Weak (babs x)
  ; Pass n -> Closed (Lf n)
  ; La t -> case babs t of
    { Defer -> Closed (lf 'I')
    ; Closed d -> Closed (Nd (lf 'K') d)
    ; Need e -> e
    ; Weak e -> babsa (Closed (lf 'K')) e
    }
  ; App x y -> babsa (babs x) (babs y)
  };

freeCount v expr = case expr of
  { E _ -> 0
  ; V s -> if s == v then 1 else 0
  ; A x y -> freeCount v x + freeCount v y
  ; L w t -> if v == w then 0 else freeCount v t
  };

app01 s x = let { n = freeCount s x } in
  if 2 <= n then A $ L s x else
    if 0 == n then const x else flip (beta s) x;
optiApp t = case t of
  { A (L s x) y -> app01 s (optiApp x) (optiApp y)
  ; A x y -> A (optiApp x) (optiApp y)
  ; L s x -> L s (optiApp x)
  ; _ -> t
  };

nolam m x = (\(Closed d) -> d) $ babs $ debruijn m [] $ optiApp x;

isLeaf t c = case t of { Lf n -> n == ord c ; _ -> False };

ife a b c = case a of { True -> b ; False -> c };
optim t = case t of
  { Lf n -> t
  ; Nd x y -> let { p = optim x ; q = optim y } in
    ife (isLeaf p 'I') q $
    ife (isLeaf q 'I') (
      ife (isLeaf p 'C') (Lf $ ord 'T') $
      ife (isLeaf p 'B') (Lf $ ord 'I') $
      Nd p q
    ) $ Nd p q
  };

enc mem t = case optim t of
  { Lf n -> (n, mem)
  ; Nd x y -> fpair mem \hp bs -> let
    { pm qm = enc (hp + 2, bs . (fst (pm qm):) . (fst qm:)) x
    ; qm = enc (snd $ pm qm) y
    } in (hp, snd qm)
  };

asm qas = foldl (\(tab, mem) (s, (_, t)) ->
  fpair (enc mem $ nolam (insert s (fst mem) tab) t) \p m' -> let
  -- Definitions like "t = t;" must be handled with care.
  { m'' = fpair m' \hp bs -> if p == hp then (hp + 2, bs . (ord 'I':) . (p:)) else m'
  } in (insert s p tab, m''))
    (Tip, (128, id)) qas;

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
  { TC v -> Just [(s, t)]
  ; TV v -> Just $ if v == s then [] else [(s, t)]
  ; TAp a b -> if occurs s t then Nothing else Just [(s, t)]
  };

charIsInt s = if s == "Char" then "Int" else s;

mgu unify t u = case t of
  { TC a -> case u of
    { TC b -> if charIsInt a == charIsInt b then Just [] else Nothing
    ; TV b -> varBind b t
    ; TAp a b -> Nothing
    }
  ; TV a -> varBind a u
  ; TAp a b -> case u of
    { TC b -> Nothing
    ; TV b -> varBind b t
    ; TAp c d -> unify b d (mgu unify a c)
    }
  };

unify a b = maybe Nothing \s -> (@@ s) <$> (mgu unify (apply s a) (apply s b));

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

--type SymTab = [(String, (Qual, Ast))];
--type Subst = [(String, Type)];

singleOut s cs = \scrutinee x ->
  foldl A (A (V $ specialCase cs) scrutinee) $ map (\(Constr s' ts) ->
    if s == s' then x else foldr L (V "pjoin#") $ map (const "_") ts) cs;

unpat dcs n als x = case als of
  { [] -> (x, n)
  ; (a, l):alt -> let
    { go p t = case p of
      { PatPred pre -> unpat dcs n alt $ A (A (A pre $ V l) t) $ V "pjoin#"
      ; PatVar s m -> maybe (unpat dcs n alt) go m $ beta s (V l) t
      ; PatCon con args -> case mlookup con dcs of
        { Nothing -> error "bad data constructor"
        ; Just cons -> let { als = zip args $ ($ "#") . showInt <$> upFrom n }
          in fpair (unpat dcs (n + length args) als t)
            \y n2 -> unpat dcs n2 alt $ singleOut con cons (V l) $ foldr L y $ snd <$> als
        }
      }
    } in go a x
  };

rewriteGuard join gs = foldr (\(cond, x) acc -> case cond of
  { V "True" -> x
  ; _ -> A (A (A (V "if") cond) x) acc
  }) (V join) gs;

rewritePats' dcs asxs ls n = case asxs of
  { [] -> (V "fail#", n)
  ; (as, gs):asxt -> fpair (unpat dcs n (zip as ls) $ rewriteGuard "pjoin#" gs) \y n1 ->
    first (A $ L "pjoin#" y) $ rewritePats' dcs asxt ls n1
  };

rewritePats dcs vsxs@((vs0, _):_) n = let
  { ls = map (flip showInt "#") $ take (length vs0) $ upFrom n }
  in first (flip (foldr L) ls) $ rewritePats' dcs vsxs ls $ n + length ls;

classifyAlt v x = case v of
  { PatPred pre -> Left $ A (A (A pre $ V "of") x)
  ; PatVar s m -> maybe (Left . A . L "pjoin#") classifyAlt m $ A (L s x) $ V "of"
  ; PatCon s ps -> Right (insertWith (flip (.)) s ((ps, [(V "True", x)]):))
  };

genCase dcs tab = if size tab == 0 then id else A . L "cjoin#" $ let
  { firstC = flst (toAscList tab) undefined (\h _ -> fst h)
  ; cs = maybe (error $ "bad constructor: " ++ firstC) id $ mlookup firstC dcs
  } in foldl A (A (V $ specialCase cs) (V "of"))
    $ map (\(Constr s ts) -> case mlookup s tab of
      { Nothing -> foldr L (V "cjoin#") $ const "_" <$> ts
      ; Just f -> Pa $ f [(const (PatVar "_" Nothing) <$> ts, [(V "True", V "cjoin#")])]
      }) cs;

updateCaseSt dcs (acc, tab) alt = case alt of
  { Left f -> (acc . genCase dcs tab . f, Tip)
  ; Right upd -> (acc, upd tab)
  };

rewriteCase dcs as = fpair (foldl (updateCaseSt dcs) (id, Tip) $ uncurry classifyAlt . second (rewriteGuard "cjoin#") <$> as) \acc tab ->
  acc . genCase dcs tab $ V "fail#";

--infer :: AdtTab -> SymTab -> Subst -> Ast -> (Maybe Subst, Int) -> ((Type, Ast), (Maybe Subst, Int))
infer dcs typed loc ast csn = fpair csn \cs n ->
  let
    { va = TV (showInt n "")
    ; insta ty = fpair (instantiate ty n) \(Qual preds ty) n1 -> ((ty, foldl A ast (map (E . Proof) preds)), (cs, n1))
    }
  in case ast of
  { E x -> case x of
    { Basic b -> if b == ord 'Y'
      then insta $ noQual $ arr (arr (TV "a") (TV "a")) (TV "a")
      else undefined
    ; Const c -> ((TC "Int", ast), csn)
    ; StrCon _ -> ((TAp (TC "[]") (TC "Int"), ast), csn)
    ; Proof _ -> undefined
    }
  ; V s -> fmaybe (lookup s loc)
    (fmaybe (lookup s typed) (error $ "bad symbol: " ++ s) $ insta . fst)
    ((, csn) . (, ast))
  ; A x y ->
    fpair (infer dcs typed loc x (cs, n + 1)) \(tx, ax) csn1 ->
    fpair (infer dcs typed loc y csn1) \(ty, ay) csn2 ->
      ((va, A ax ay), first (unify tx (arr ty va)) csn2)
  ; L s x -> first (\ta -> fpair ta \t a -> (arr va t, L s a)) (infer dcs typed ((s, va):loc) x (cs, n + 1))
  ; Pa vsxs -> fpair (rewritePats dcs vsxs n) \re n1 -> infer dcs typed loc re (cs, n1)
  ; Ca x as -> infer dcs typed loc (A (L "of" $ rewriteCase dcs as) x) csn
  };

onType f pred = case pred of { Pred s t -> Pred s (f t) };

instance Eq Type where
  { (TC s) == (TC t) = s == t
  ; (TV s) == (TV t) = s == t
  ; (TAp a b) == (TAp c d) = a == c && b == d
  ; _ == _ = False
  };

instance Eq Pred where { (Pred s a) == (Pred t b) = s == t && a == b };

predApply sub p = onType (apply sub) p;

filter f = foldr (\x xs -> if f x then x:xs else xs) [];
intersect xs ys = filter (\x -> fmaybe (find (x ==) ys) False (\_ -> True)) xs;

merge s1 s2 = if all (\v -> apply s1 (TV v) == apply s2 (TV v))
  $ map fst s1 `intersect` map fst s2 then Just $ s1 ++ s2 else Nothing;

match h t = case h of
  { TC a -> case t of
    { TC b -> if a == b then Just [] else Nothing
    ; TV _ -> Nothing
    ; TAp _ _ -> Nothing
    }
  ; TV a -> Just [(a, t)]
  ; TAp a b -> case t of
    { TC _ -> Nothing
    ; TV _ -> Nothing
    ; TAp c d -> case match a c of
      { Nothing -> Nothing
      ; Just ac -> case match b d of
        { Nothing -> Nothing
        ; Just bd -> merge ac bd
        }
      }
    }
  };

matchPred h (Pred _ t) = match h t;
showType t = case t of
  { TC s -> s
  ; TV s -> s
  ; TAp a b -> concat ["(", showType a, " ", showType b, ")"]
  };
showPred (Pred s t) = s ++ (' ':showType t) ++ " => ";

findInst r qn p insts = case insts of
  { [] -> fpair qn \q n -> let { v = '*':showInt n "" } in (((p, v):q, n + 1), V v)
  ; (Qual ps h):is -> case matchPred h p of
    { Nothing -> findInst r qn p is
    ; Just u -> foldl (\(qn1, t) p -> second (A t)
      (r (predApply u p) qn1)) (qn, V (case p of { Pred s _ -> showPred $ Pred s h})) ps
  }};

findProof ienv pred psn@(ps, n) = case lookup pred ps of
  { Nothing -> case pred of { Pred s t -> case mlookup s ienv of
    { Nothing -> error $ "no instances: " ++ s
    ; Just insts -> findInst (findProof ienv) psn pred insts
    }}
  ; Just s -> (psn, V s)
  };

prove' ienv sub psn a = case a of
  { E (Proof raw) -> findProof ienv (predApply sub raw) psn
  ; A x y -> fpair (prove' ienv sub psn x) \psn1 x1 ->
    second (A x1) (prove' ienv sub psn1 y)
  ; L s t -> second (L s) (prove' ienv sub psn t)
  ; _ -> (psn, a)
  };

--prove :: [(String, [Qual])] -> (Type, Ast) -> Subst -> (Qual, Ast)
prove ienv s (t, a) sub = fpair (prove' ienv sub ([], 0) a) \(ps, _) x ->
  let { applyDicts expr = foldl A expr $ map (V . snd) ps }
  in (Qual (map fst ps) (apply sub t), foldr L (overFree s applyDicts x) $ map snd ps);

dictVars ps n = flst ps ([], n) \p pt -> first ((p, '*':showInt n ""):) (dictVars pt $ n + 1);

-- The 4th argument is Qual of the instance, e.g. Eq t => [t] -> [t] -> Bool
inferMethod ienv dcs typed (Qual psi ti) def = fpair def \s expr ->
  fpair (infer dcs typed [] expr (Just [], 0)) \ta (ms, n) ->
  case lookup s typed of
    { Nothing -> error $ "no such method: " ++ s
    -- e.g. qac = Eq a => a -> a -> Bool, some AST (product of single method)
    ; Just qac -> case ms of
      { Nothing -> error "method: type mismatch"
      ; Just sub -> fpair (instantiate (fst qac) n) \(Qual [Pred _ headT] tc) n1 ->
        case match headT ti of
          { Nothing -> undefined
          -- e.g. Eq t => [t] -> [t] -> Bool
          -- instantiate and match it against type of ta
          ; Just subc ->
            fpair (instantiate (Qual psi $ apply subc tc) n1) \(Qual ps2 t2) n2 ->
            fpair ta \tx ax ->
              case match (apply sub tx) t2 of
                { Nothing -> error "class/instance type conflict"
                ; Just subx -> snd $ prove' ienv (subx @@ sub) (dictVars ps2 0) ax
              }}}};

inferInst ienv dcs typed inst = fpair inst \cl qds -> fpair qds \q ds ->
  case q of { Qual ps t -> let { s = showPred $ Pred cl t } in
  (s, (,) (noQual $ TC "DICT") $ foldr L (L "@" $ foldl A (V "@") (map (inferMethod ienv dcs typed q) ds)) (map snd $ fst $ dictVars ps 0))
  };

reverse = foldl (flip (:)) [];
inferDefs ienv defs dcs typed = flst defs (Right $ reverse typed) \edef rest -> case edef of
  { Left (s, expr) ->
    fpair (infer dcs typed [(s, TV "self!")] expr (Just [], 0)) \ta (ms, _) ->
      case prove ienv s ta <$> (unify (TV "self!") (fst ta) ms) of
    { Nothing -> Left ("bad type: " ++ s)
    ; Just qa -> inferDefs ienv rest dcs ((s, qa):typed)
    }
  ; Right inst -> inferDefs ienv rest dcs (inferInst ienv dcs typed inst:typed)
  };

showQual (Qual ps t) = concatMap showPred ps ++ showType t;

untangle = foldr ($) (Neat Tip [] prims Tip [] []);

dumpTypes s = fmaybe (program s) "parse error" \(prog, rest) ->
  fneat (untangle prog) \ienv fs typed dcs ffis exs -> case inferDefs ienv fs dcs typed of
  { Left err -> err
  ; Right typed -> concatMap (\(s, (q, _)) -> s ++ " :: " ++ showQual q ++ "\n") typed
  };

last' x xt = flst xt x \y yt -> last' y yt;
last xs = flst xs undefined last';
init (x:xt) = flst xt [] \_ _ -> x : init xt;
intercalate sep xs = flst xs [] \x xt -> x ++ concatMap (sep ++) xt;

argList t = case t of
  { TC s -> [TC s]
  ; TV s -> [TV s]
  ; TAp (TC "IO") (TC u) -> [TC u]
  ; TAp (TAp (TC "->") x) y -> x : argList y
  };

cTypeName (TC "()") = "void";
cTypeName (TC "Int") = "int";
cTypeName (TC "Char") = "int";

ffiDeclare (name, t) = let { tys = argList t } in concat
  [cTypeName $ last tys, " ", name, "(", intercalate "," $ cTypeName <$> init tys, ");\n"];

ffiArgs n t = case t of
  { TC s -> ("", ((True, s), n))
  ; TAp (TC "IO") (TC u) -> ("", ((False, u), n))
  ; TAp (TAp (TC "->") x) y -> first ((ife (3 <= n) ", " "" ++ "num(" ++ showInt n ")") ++) $ ffiArgs (n + 1) y
  };

ffiDefine n ffis = case ffis of
  { [] -> id
  ; (name, t):xt -> fpair (ffiArgs 2 t) \args ((isPure, ret), count) -> let
    { lazyn = ("lazy(" ++) . showInt (if isPure then count - 1 else count + 1) . (", " ++)
    ; cont tgt = if isPure then ("'I', "++) . tgt else ("app(arg("++) . showInt (count + 1) . ("), "++) . tgt . ("), arg("++) . showInt count . (")"++)
    ; longDistanceCall = (name++) . ("("++) . (args++) . ("); "++) . lazyn
    } in ("case " ++) . showInt n . (": " ++) . if ret == "()"
      then longDistanceCall . cont ("'K'"++) . ("); break;"++) . ffiDefine (n - 1) xt
      else ("{u r = "++) . longDistanceCall . cont ("app('#', r)" ++) . ("); break;}\n"++) . ffiDefine (n - 1) xt
  };

getContents = getChar >>= \n -> if n <= 255 then (chr n:) <$> getContents else pure [];

genMain n = "int main(int argc,char**argv){env_argc=argc;env_argv=argv;rts_init();rts_reduce(" ++ showInt n ");return 0;}\n";

compile s = fmaybe (program s) "parse error" \(prog, rest) -> fneat (untangle prog)
  \ienv fs typed dcs ffis exs -> case inferDefs ienv fs dcs typed of
  { Left err -> err
  ; Right qas -> fpair (asm qas) \tab mem ->
    (concatMap ffiDeclare ffis ++) .
    ("static void foreign(u n) {\n  switch(n) {\n" ++) .
    ffiDefine (length ffis - 1) ffis .
    ("\n  }\n}\n" ++) .
    ("static const u prog[]={" ++) .
    foldr (.) id (map (\n -> showInt n . (',':)) $ snd mem []) .
    ("};\nstatic const u prog_size=sizeof(prog)/sizeof(*prog);\n" ++) .
    ("static u root[]={" ++) .
    foldr (\(x, y) f -> maybe undefined showInt (mlookup y tab) . (", " ++) . f) id exs .
    ("};\n" ++) .
    ("static const u root_size=" ++) . showInt (length exs) . (";\n" ++) $
    (foldr (.) id $ zipWith (\p n -> (("EXPORT(f" ++ showInt n ", \"" ++ fst p ++ "\", " ++ showInt n ")\n") ++)) exs (upFrom 0)) $
    maybe "" genMain (mlookup "main" tab)
  };

main = getContents >>= putStr . compile;
