-- Patterns.
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

data Bool = True | False;
ife a b c = case a of { True -> b ; False -> c };
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
($) f x = f x;
id x = x;
const x y = x;
flip f x y = f y x;
(&) x f = f x;
class Ord a where { (<=) :: a -> a -> Bool };
instance Ord Int where { (<=) = intLE };
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
    ; (:) x xt -> case ys of
      { [] -> False
      ; (:) y yt -> case compare x y of
        { LT -> True
        ; GT -> False
        ; EQ -> xt <= yt
        }
      }
    }
};
data Maybe a = Nothing | Just a;
data Either a b = Left a | Right b;
fpair p = \f -> case p of { (,) x y -> f x y };
fst p = case p of { (,) x y -> x };
snd p = case p of { (,) x y -> y };
uncurry f p = fpair p \x y -> f x y;
first f p = fpair p \x y -> (f x, y);
second f p = fpair p \x y -> (x, f y);
not a = case a of { True -> False; False -> True };
(.) f g x = f (g x);
(||) f g = ife f True g;
(&&) f g = ife f g False;
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };
instance Eq a => Eq [a] where { (==) xs ys = case xs of
  { [] -> case ys of
    { [] -> True
    ; (:) _ _ -> False
    }
  ; (:) x xt -> case ys of
    { [] -> False
    ; (:) y yt -> x == y && xt == yt
    }
  }};
take n xs = ife (n == 0) [] $ flst xs [] $ \h t -> h:take (n - 1) t;

maybe n j m = case m of { Nothing -> n; Just x -> j x };
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
foldr1 c l = maybe undefined id (flst l undefined (\h t -> foldr (\x m -> Just (case m of { Nothing -> x ; Just y -> c x y })) Nothing l));
foldl f a bs = foldr (\b g x -> g (f x b)) (\x -> x) bs a;
foldl1 f bs = flst bs undefined (\h t -> foldl f h t);
elem k xs = foldr (\x t -> ife (x == k) True t) False xs;
find f xs = foldr (\x t -> ife (f x) (Just x) t) Nothing xs;
(++) = flip (foldr (:));
concat = foldr (++) [];
wrap c = c:[];
map = flip (foldr . ((:) .)) [];
instance Functor [] where { fmap = map };
concatMap = (concat .) . map;
fmaybe m n j = case m of { Nothing -> n; Just x -> j x };
lookup s = foldr (\h t -> fpair h (\k v -> ife (s == k) (Just v) t)) Nothing;
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
singleL k x l r = case r of
  { Tip -> undefined
  ; Bin _ rk rkx rl rr -> node rk rkx (node k x l rl) rr
  };
singleR k x l r = case l of
  { Tip -> undefined
  ; Bin _ lk lkx ll lr -> node lk lkx ll (node k x lr r)
  };
doubleL k x l r = case r of
  { Tip -> undefined
  ; Bin _ rk rkx rl rr -> case rl of
    { Tip -> undefined
    ; Bin _ rlk rlkx rll rlr -> node rlk rlkx (node k x l rll) (node rk rkx rlr rr)
    }
  };
doubleR k x l r = case l of
  { Tip -> undefined
  ; Bin _ lk lkx ll lr -> case lr of
    { Tip -> undefined
    ; Bin _ lrk lrkx lrl lrr -> node lrk lrkx (node lk lkx ll lrl) (node k x lrr r)
    }
  };
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
fromList = let
  { ins t kx = case kx of { (,) k x -> insert k x t }
  } in foldl ins Tip;

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

data Ast = E Extra | V String | A Ast Ast | L String Ast | Pa [([Pat], Ast)] | Ca Ast [(Pat, Ast)];
ro = E . Basic . ord;
data Parser a = Parser (String -> Maybe (a, String));

data Constr = Constr String [Type];
data Pred = Pred String Type;
data Qual = Qual [Pred] Type;
noQual = Qual [];

data Neat = Neat
  -- | Instance environment.
  [(String, [Qual])]
  -- | Either top-level or instance definitions.
  [Either (String, Ast) (String, (Qual, [(String, Ast)]))]
  -- | Typed ASTs, ready for compilation, including ADTs and methods,
  -- e.g. (==), (Eq a => a -> a -> Bool, select-==)
  [(String, (Qual, Ast))]
  -- | Data constructor table.
  [(String, [Constr])]
  -- | FFI declarations.
  [(String, Type)]
  -- | Exports.
  [(String, String)]
  ;

parse p inp = case p of { Parser f -> f inp };

fneat neat z = case neat of { Neat a b c d e f -> z a b c d e f };

conOf con = case con of { Constr s _ -> s };
specialCase = concatMap (('|':) . conOf);
mkCase t cs = (specialCase cs,
  ( noQual $ arr t $ foldr arr (TV "case") $ map (\c -> case c of { Constr _ ts -> foldr arr (TV "case") ts}) cs
  , ro 'I'));
mkStrs = snd . foldl (\p u -> fpair p (\s l -> ('@':s, s : l))) ("@", []);
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs);
scottConstr t cs c = case c of { Constr s ts -> (s,
  ( noQual $ foldr arr t ts
  , scottEncode (map conOf cs) s $ mkStrs ts)) };
mkAdtDefs t cs = mkCase t cs : map (scottConstr t cs) cs;

adtLookups cs = map (\c -> case c of { Constr s _ -> (s, cs) }) cs;

select f xs acc = flst xs (Nothing, acc) \x xt -> ife (f x) (Just x, xt ++ acc) (select f xt (x:acc));

addInstance s q is = fpair (select (\kv -> s == fst kv) is []) \m xs -> case m of
  { Nothing -> (s, [q]):xs
  ; Just sqs -> second (q:) sqs:xs
  };

mkSel ms s = L "@" $ A (V "@") $ foldr L (V $ '*':s) $ map (('*':) . fst) ms;

ifz n = ife (0 == n);
showInt' n = ifz n id ((showInt' (n/10)) . ((:) (chr (48+(n%10)))));
showInt n = ifz n ('0':) (showInt' n);

mkFFIHelper n t acc = case t of
  { TC s -> acc
  ; TV s -> undefined
  ; TAp g y -> case g of
    { TC s -> ife (s == "IO") acc undefined
    ; TV s -> undefined
    ; TAp f x -> case f of
      { TC s -> ife (s == "->") (L (showInt n "") $ mkFFIHelper (n + 1) y $ A (V $ showInt n "") acc) undefined
      ; TV s -> undefined
      ; TAp _ _ -> undefined
      }
    }
  };

addAdt t cs acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv fs (mkAdtDefs t cs ++ typed) (adtLookups cs ++ dcs) ffis exs;
addClass classId v ms acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv fs
  (map (\st -> fpair st \s t -> (s, (Qual [Pred classId v] t, mkSel ms s))) ms ++ typed) dcs ffis exs;
addInst cl q ds acc = fneat acc \ienv fs typed dcs ffis exs -> Neat (addInstance cl q ienv) (Right (cl, (q, ds)):fs) typed dcs ffis exs;
addFFI foreignname ourname t acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv fs
  ((ourname, (Qual [] t, mkFFIHelper 0 t $ A (ro 'F') (ro $ chr $ length ffis))) : typed) dcs ((foreignname, t):ffis) exs;
addDefs ds acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv (map Left ds ++ fs) typed dcs ffis exs;
addExport e f acc = fneat acc \ienv fs typed dcs ffis exs -> Neat ienv fs typed dcs ffis ((e, f):exs);

instance Applicative Parser where
{ pure x = Parser \inp -> Just (x, inp)
; (<*>) x y = Parser \inp -> case parse x inp of
  { Nothing -> Nothing
  ; Just funt -> fpair funt \fun t -> case parse y t of
    { Nothing -> Nothing
    ; Just argu -> fpair argu \arg u -> Just (fun arg, u)
    }
  }
};
instance Monad Parser where
{ return = pure
; (>>=) x f = Parser \inp -> case parse x inp of
  { Nothing -> Nothing
  ; Just at -> fpair at \a t -> parse (f a) t
  }
};

sat' f = \h t -> ife (f h) (Just (h, t)) Nothing;
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

char c = sat \x -> x == c;
between x y p = x *> (p <* y);
com = char '-' *> between (char '-') (char '\n') (many (sat \c -> not (c == '\n')));
sp = many ((wrap <$> (sat (\c -> (c == ' ') || (c == '\n')))) <|> com);
spc f = f <* sp;
spch = spc . char;

wantWith pred f = Parser \inp -> case parse f inp of
  { Nothing -> Nothing
  ; Just at -> ife (pred $ fst at) (Just at) Nothing
  };

want f s = wantWith (s ==) f;

paren = between (spch '(') (spch ')');
small = sat \x -> ((x <= 'z') && ('a' <= x)) || (x == '_');
large = sat \x -> (x <= 'Z') && ('A' <= x);
digit = sat \x -> (x <= '9') && ('0' <= x);
symbo = sat \c -> elem c "!#$%&*+./<=>?@\\^|-~";
varLex = liftA2 (:) small (many (small <|> large <|> digit <|> char '\''));
conId = spc (liftA2 (:) large (many (small <|> large <|> digit <|> char '\'')));
keyword s = spc $ want varLex s;
varId = spc $ wantWith (\s -> not $ elem s ["class", "data", "instance", "of", "where", "if", "then", "else"]) varLex;
opTail = many $ char ':' <|> symbo;
conSym = spc $ liftA2 (:) (char ':') opTail;
varSym = spc $ liftA2 (:) symbo opTail;
con = conId <|> paren conSym;
var = varId <|> paren varSym;
op = varSym <|> conSym <|> between (spch '`') (spch '`') (conId <|> varId);
conop = conSym <|> between (spch '`') (spch '`') conId;
escChar = char '\\' *> ((sat (\c -> elem c "'\"\\")) <|> ((\c -> '\n') <$> char 'n'));
litOne delim = escChar <|> sat \c -> not (c == delim);
litInt = Const . foldl (\n d -> 10*n + ord d - ord '0') 0 <$> spc (some digit);
litStr = between (char '"') (spch '"') $ many (litOne '"');
litChar = Const . ord <$> between (char '\'') (spch '\'') (litOne '\'');
lit = E <$> (StrCon <$> litStr <|> litChar <|> litInt);
sqLst r = between (spch '[') (spch ']') $ sepBy r (spch ',');

gcon = conId <|> paren (conSym <|> (wrap <$> spch ',')) <|> ((:) <$> spch '[' <*> (wrap <$> spch ']'));

apat' r = PatVar <$> var <*> (want varSym "@" *> (Just <$> apat' r) <|> pure Nothing)
  <|> flip PatCon [] <$> gcon
  <|> PatPred . A (V "if#") . A (V "==") <$> lit
  <|> foldr (\h t -> PatCon ":" [h, t]) (PatCon "[]" []) <$> sqLst r
  <|> paren ((&) <$> r <*> ((spch ',' *> ((\y x -> PatCon "," [x, y]) <$> r)) <|> pure id))
  ;
pat = PatCon <$> gcon <*> many (apat' pat)
  <|> (&) <$> apat' pat <*> ((\s r l -> PatCon s [l, r]) <$> conop <*> apat' pat <|> pure id);
apat = apat' pat;

alt r = (,) <$> pat <*> (want varSym "->" *> r);

braceSep f = between (spch '{') (spch '}') (sepBy f (spch ';'));
alts r = braceSep (alt r);
cas r = Ca <$> between (keyword "case") (keyword "of") r <*> alts r;
lamCase r = keyword "case" *> (L "\\case" . Ca (V "\\case") <$> alts r);
onePat vs x = Pa [(vs, x)];
lam r = spch '\\' *> (lamCase r <|> liftA2 onePat (some apat) (char '-' *> (spch '>' *> r)));

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
  ; L w t -> not (v == w) && isFree v t
  ; Pa vsts -> any (\vst -> fpair vst \vs t -> not (any (isFreePat v) vs) && isFree v t) vsts
  ; Ca x as -> isFree v x || isFree v (Pa $ first (:[]) <$> as)
  };

freeCount v expr = case expr of
  { E _ -> 0
  ; V s -> ife (s == v) 1 0
  ; A x y -> freeCount v x + freeCount v y
  ; L w t -> ife (v == w) 0 $ freeCount v t
  ; Pa vsts -> foldr (+) 0 $ map (\vst -> fpair vst \vs t -> ife (any (isFreePat v) vs) 0 $ freeCount v t) vsts
  ; Ca x as -> freeCount v x + freeCount v (Pa $ first (:[]) <$> as)
  };

overFree s f t = case t of
  { E _ -> t
  ; V s' -> ife (s == s') (f t) t
  ; A x y -> A (overFree s f x) (overFree s f y)
  ; L s' t' -> ife (s == s') t $ L s' $ overFree s f t'
  ; Pa vsxs -> Pa $ map (\vsx -> fpair vsx \vs x -> ife (any (isFreePat s) vs) vsx (vs, overFree s f x)) vsxs
  ; Ca x as -> Ca (overFree s f x) $ map (\vx -> fpair vx \v x -> ife (isFreePat s v) vx (v, overFree s f x)) as
  };

beta s t x = overFree s (const t) x;

optiApp s x = let { n = freeCount s x } in
  ife (2 <= n) (A $ L s x)
    $ ife (0 == n) (const x) (flip (beta s) x);

maybeFix s x = ife (isFree s x) (A (ro 'Y') (L s x)) x;

opDef x f y rhs = (f, onePat [x, y] rhs);

coalesce ds = flst ds [] \h t -> flst t [h] \h' t' ->
  fpair h' \s' x' -> fpair h \s x -> ife (s == s')
    ( let { bad = error "bad multidef" } in case x of
      { E _ -> bad
      ; V _ -> bad
      ; A _ _ -> bad
      ; L _ _ -> bad
      ; Pa vsts -> case x' of
        { E _ -> bad
        ; V _ -> bad
        ; A _ _ -> bad
        ; L _ _ -> bad
        ; Pa vsts' -> coalesce $ (s, Pa $ vsts ++ vsts'):t'
        ; Ca _ _ -> bad
        }
      ; Ca _ _ -> bad
      }
    ) $ h:coalesce t
  ;

def r = opDef <$> apat <*> varSym <*> apat <*> (spch '=' *> r)
  <|> liftA2 (,) var (liftA2 onePat (many apat) (spch '=' *> r));

addLets ls x = foldr (\p t -> fpair p (\name def -> optiApp name t $ maybeFix name def)) x ls;
letin r = addLets <$> between (keyword "let") (keyword "in") (coalesce <$> braceSep (def r)) <*> r;
ifthenelse r = (\a b c -> A (A (A (V "if") a) b) c) <$>
  (keyword "if" *> r) <*> (keyword "then" *> r) <*> (keyword "else" *> r);
listify = foldr (\h t -> A (A (V ":") h) t) (V "[]");
atom r = ifthenelse r <|> letin r <|> listify <$> sqLst r <|> section r <|> cas r <|> lam r <|> (paren (spch ',') *> pure (V ",")) <|> fmap V (con <|> var) <|> lit;
aexp r = fmap (foldl1 A) (some (atom r));
fix f = f (fix f);

data Assoc = NAssoc | LAssoc | RAssoc;
eqAssoc x y = case x of
  { NAssoc -> case y of { NAssoc -> True  ; LAssoc -> False ; RAssoc -> False }
  ; LAssoc -> case y of { NAssoc -> False ; LAssoc -> True  ; RAssoc -> False }
  ; RAssoc -> case y of { NAssoc -> False ; LAssoc -> False ; RAssoc -> True }
  };
precOf s precTab = fmaybe (lookup s precTab) 9 fst;
assocOf s precTab = fmaybe (lookup s precTab) LAssoc snd;
opWithPrec precTab n = wantWith (\s -> n == precOf s precTab) op;
opFold precTab e xs = case xs of
  { [] -> e
  ; (:) x xt -> case find (\y -> not (eqAssoc (assocOf (fst x) precTab) (assocOf (fst y) precTab))) xt of
    { Nothing -> case assocOf (fst x) precTab of
      { NAssoc -> case xt of
        { [] -> fpair x (\op y -> A (A (V op) e) y)
        ; (:) y yt -> undefined
        }
      ; LAssoc -> foldl (\a b -> fpair b (\op y -> A (A (V op) a) y)) e xs
      ; RAssoc -> (foldr (\a b -> fpair a (\op y -> \e -> A (A (V op) e) (b y))) id xs) e
      }
    ; Just y -> undefined
    }
  };
expr precTab = fix \r n -> ife (n <= 9) (liftA2 (opFold precTab) (r (succ n)) (many (liftA2 (\a b -> (a,b)) (opWithPrec precTab n) (r (succ n))))) (aexp (r 0));

bType r = foldl1 TAp <$> some r;
_type r = foldr1 arr <$> sepBy (bType r) (spc (want varSym "->"));
typeConst = (\s -> ife (s == "String") (TAp (TC "[]") (TC "Int")) (TC s)) <$> conId;
aType = spch '(' *> (spch ')' *> pure (TC "()") <|> ((&) <$> _type aType <*> ((spch ',' *> ((\a b -> TAp (TAp (TC ",") b) a) <$> _type aType)) <|> pure id)) <* spch ')') <|>
  typeConst <|> (TV <$> varId) <|>
  (spch '[' *> (spch ']' *> pure (TC "[]") <|> TAp (TC "[]") <$> (_type aType <* spch ']')));

simpleType c vs = foldl TAp (TC c) (map TV vs);

-- Can we reduce backtracking here?
constr = (\x c y -> Constr c [x, y]) <$> aType <*> conSym <*> aType
  <|> Constr <$> conId <*> many aType;

adt = addAdt <$> between (keyword "data") (spch '=') (simpleType <$> conId <*> many varId) <*> sepBy constr (spch '|');

prec = (\c -> ord c - ord '0') <$> spc digit;
fixityList a n os = map (\o -> (o, (n, a))) os;
fixityDecl kw a = between (keyword kw) (spch ';') (fixityList a <$> prec <*> sepBy op (spch ','));
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc;

genDecl = (,) <$> var <*> (char ':' *> spch ':' *> _type aType);
classDecl = keyword "class" *> (addClass <$> conId <*> (TV <$> varId) <*> (keyword "where" *> braceSep genDecl));

inst = _type aType;
instDecl r = keyword "instance" *>
  ((\ps cl ty defs -> addInst cl (Qual ps ty) defs) <$>
  (((wrap .) . Pred <$> conId <*> (inst <* want varSym "=>")) <|> pure [])
    <*> conId <*> inst <*> (keyword "where" *> (coalesce <$> braceSep (def r))));

ffiDecl = keyword "ffi" *>
  (addFFI <$> litStr <*> var <*> (char ':' *> spch ':' *> _type aType));

tops precTab = sepBy
  (   adt
  <|> classDecl
  <|> instDecl (expr precTab 0)
  <|> ffiDecl
  <|> addDefs . coalesce <$> sepBy1 (def $ expr precTab 0) (spch ';')
  <|> keyword "export" *> (addExport <$> litStr <*> var)
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
    , ("ioPure", (arr (TV "a") (TAp (TC "IO") (TV "a")), A (A (ro 'B') (ro 'C')) (ro 'T')))
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
    foldr (\h found -> ife (h == v) (Just Ze) (maybe Nothing (Just . Su) found)) Nothing n
  ; A x y -> App (debruijn m n x) (debruijn m n y)
  ; L s t -> La (debruijn m (s:n) t)
  ; Pa _ -> undefined
  ; Ca _ _ -> undefined
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

nolam m x = case babs $ debruijn m [] x of
  { Defer -> undefined
  ; Closed d -> d
  ; Need e -> undefined
  ; Weak e -> undefined
  };

isLeaf t c = case t of { Lf n -> n == ord c ; Nd _ _ -> False };

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

asm qas = foldl (\tabmem def -> fpair def \s qt -> fpair tabmem \tab mem ->
  fpair (enc mem $ nolam (insert s (fst mem) tab) $ snd qt) \p m' -> let
  -- Definitions like "t = t;" must be handled with care.
  { m'' = fpair m' \hp bs -> ife (p == hp) (hp + 2, bs . (ord 'I':) . (p:)) m'
  } in (insert s p tab, m''))
    (Tip, (128, id)) qas;

-- Type checking.

apply sub t = case t of
  { TC v -> t
  ; TV v -> fmaybe (lookup v sub) t id
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
  ; TV v -> ife (v == s) (Just []) (Just [(s, t)])
  ; TAp a b -> ife (occurs s t) Nothing (Just [(s, t)])
  };

charIsInt s = ife (s == "Char") "Int" s;

mgu unify t u = case t of
  { TC a -> case u of
    { TC b -> ife (charIsInt a == charIsInt b) (Just []) Nothing
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
    fpair (instantiate' x n tab) \tn1 tab1 ->
    fpair tn1 \t1 n1 -> fpair (instantiate' y n1 tab1) \tn2 tab2 ->
    fpair tn2 \t2 n2 -> ((TAp t1 t2, n2), tab2)
  };

instantiatePred pred xyz = case pred of { Pred s t -> fpair xyz \xy tab -> fpair xy \out n -> first (first ((:out) . Pred s)) (instantiate' t n tab) };

--instantiate :: Qual -> Int -> (Qual, Int)
instantiate qt n = case qt of { Qual ps t ->
  fpair (foldr instantiatePred (([], n), []) ps) \xy tab -> fpair xy \ps1 n1 ->
  first (Qual ps1) (fst (instantiate' t n1 tab))
  };

--type SymTab = [(String, (Qual, Ast))];
--type Subst = [(String, Type)];

singleOut s cs = \scrutinee x ->
  foldl A (A (V $ specialCase cs) scrutinee) $ map (\c' -> case c' of { Constr s' ts ->
    ife (s == s') x $ foldr L (V "patjoin#") $ map (const "_") ts }) cs;

unpat dcs n as x = case as of
  { [] -> (x, n)
  ; a:at -> let { freshv = showInt n "#" } in first (L freshv) $ case a of
    { PatPred pre -> unpat dcs (n + 1) at $ A (A (A pre $ V freshv) x) $ V "patjoin#"
    ; PatVar s m -> maybe id (error "TODO") m $ unpat dcs (n + 1) at $ beta s (V freshv) x
    ; PatCon con args -> case lookup con dcs of
      { Nothing -> error "bad data constructor"
      ; Just cons -> fpair (unpat dcs (n + 1) args x) \y n1 -> unpat dcs n1 at $ singleOut con cons (V freshv) y
      }
    }
  };

unpatTop dcs n als x = case als of
  { [] -> (x, n)
  ; al:alt -> fpair al \a l -> let
    { go p t = case p of
      { PatPred pre -> unpatTop dcs n alt $ A (A (A pre $ V l) t) $ V "patjoin#"
      ; PatVar s m -> maybe (unpatTop dcs n alt) go m $ beta s (V l) t
      ; PatCon con args -> case lookup con dcs of
        { Nothing -> error "bad data constructor"
        ; Just cons -> fpair (unpat dcs n args t) \y n1 -> unpatTop dcs n1 alt $ singleOut con cons (V l) y
        }
      }
    } in go a x
  };

rewritePats' dcs asxs ls n = case asxs of
  { [] -> (A (V "unsafePerformIO") (V "exitSuccess"), n)
  ; (:) asx asxt -> fpair asx \as x -> fpair (unpatTop dcs n (zip as ls) x) \y n1 ->
    first (optiApp "patjoin#" y) $ rewritePats' dcs asxt ls n1
  };

rewritePats dcs vsxs n = let
  { ls = map (flip showInt "#") $ take (length $ flst vsxs undefined \h _ -> fst h) $ upFrom n }
  in first (flip (foldr L) ls) $ rewritePats' dcs vsxs ls $ n + length ls;

classifyAlt v x = case v of
  { PatPred pre -> Left $ A (A (A pre $ V "of") x)
  ; PatVar s m -> maybe (Left . optiApp "casejoin#") classifyAlt m $ A (L s x) $ V "of"
  ; PatCon s ps -> Right (insertWith (flip (.)) s ((ps, x):))
  };

genCase dcs tab = ife (size tab == 0) id $ optiApp "casejoin#" $ let
  { firstC = flst (toAscList tab) undefined (\h _ -> fst h)
  ; cs = maybe (error "bad constructor") id $ lookup firstC dcs
  } in foldl A (A (V $ specialCase cs) (V "of"))
    $ map (\c -> case c of { Constr s ts -> case mlookup s tab of
      { Nothing -> foldr L (V "casejoin#") $ const "_" <$> ts
      ; Just f -> Pa $ f [(const (PatVar "_" Nothing) <$> ts, V "casejoin#")]
      }}) cs;

updateCaseSt dcs st alt = fpair st \acc tab -> case alt of
  { Left f -> (acc . genCase dcs tab . f, Tip)
  ; Right upd -> (acc, upd tab)
  };

rewriteCase dcs as = fpair (foldl (updateCaseSt dcs) (id, Tip) $ uncurry classifyAlt <$> as) \acc tab ->
  acc . genCase dcs tab $ A (V "unsafePerformIO") (V "exitSuccess");

--type AdtTab = [(String, Ast -> Ast)]
--infer :: AdtTab -> SymTab -> Subst -> Ast -> (Maybe Subst, Int) -> ((Type, Ast), (Maybe Subst, Int))
infer dcs typed loc ast csn = fpair csn \cs n ->
  let
    { va = TV (showInt n "")
    ; insta ty = fpair (instantiate ty n) \q n1 -> case q of { Qual preds ty -> ((ty, foldl A ast (map (E . Proof) preds)), (cs, n1)) }
    }
  in case ast of
  { E x -> case x of
    { Basic b -> ife (b == ord 'Y')
      (insta $ noQual $ arr (arr (TV "a") (TV "a")) (TV "a"))
      undefined
    ; Const c -> ((TC "Int", ast), csn)
    ; StrCon _ -> ((TAp (TC "[]") (TC "Int"), ast), csn)
    ; Proof _ -> undefined
    }
  ; V s -> fmaybe (lookup s loc)
    (fmaybe (lookup s typed) (error $ "bad symbol: " ++ s) $ insta . fst)
    ((, csn) . (, ast))
  ; A x y ->
    fpair (infer dcs typed loc x (cs, n + 1)) \tax csn1 -> fpair tax \tx ax ->
    fpair (infer dcs typed loc y csn1) \tay csn2 -> fpair tay \ty ay ->
      ((va, A ax ay), first (unify tx (arr ty va)) csn2)
  ; L s x -> first (\ta -> fpair ta \t a -> (arr va t, L s a)) (infer dcs typed ((s, va):loc) x (cs, n + 1))
  ; Pa vsxs -> fpair (rewritePats dcs vsxs n) \re n1 -> infer dcs typed loc re (cs, n1)
  ; Ca x as -> infer dcs typed loc (optiApp "of" (rewriteCase dcs as) x) csn
  };

onType f pred = case pred of { Pred s t -> Pred s (f t) };

instance Eq Type where { (==) t u = case t of
  { TC s -> case u of
    { TC t -> t == s
    ; TV _ -> False
    ; TAp _ _ -> False
    }
  ; TV s ->  case u of
    { TC _ -> False
    ; TV t -> t == s
    ; TAp _ _ -> False
    }
  ; TAp a b -> case u of
    { TC _ -> False
    ; TV _ -> False
    ; TAp c d -> a == c && b == d
    }
  }};

instance Eq Pred where { (==) p q =
  case p of { Pred s a -> case q of { Pred t b -> s == t && a == b }}};

predApply sub p = onType (apply sub) p;

filter f = foldr (\x xs ->ife (f x) (x:xs) xs) [];

intersect xs ys = filter (\x -> fmaybe (find (x ==) ys) False (\_ -> True)) xs;

merge s1 s2 = ife (all (\v -> apply s1 (TV v) == apply s2 (TV v))
  $ map fst s1 `intersect` map fst s2) (Just $ s1 ++ s2) Nothing;

match h t = case h of
  { TC a -> case t of
    { TC b -> ife (a == b) (Just []) Nothing
    ; TV b -> Nothing
    ; TAp a b -> Nothing
    }
  ; TV a -> Just [(a, t)]
  ; TAp a b -> case t of
    { TC b -> Nothing
    ; TV b -> Nothing
    ; TAp c d -> case match a c of
      { Nothing -> Nothing
      ; Just ac -> case match b d of
        { Nothing -> Nothing
        ; Just bd -> merge ac bd
        }
      }
    }
  };

matchPred h p = case p of { Pred _ t -> match h t };

showType t = case t of
  { TC s -> s
  ; TV s -> s
  ; TAp a b -> concat ["(", showType a, " ", showType b, ")"]
  };
showPred p = case p of { Pred s t -> s ++ (' ':showType t) ++ " => "};

findInst r qn p insts = case insts of
  { [] ->
    fpair qn \q n -> let { v = '*':showInt n "" } in (((p, v):q, n + 1), V v)
  ; (:) i is -> case i of { Qual ps h -> case matchPred h p of
    { Nothing -> findInst r qn p is
    ; Just u -> foldl (\qnt p -> fpair qnt \qn1 t -> second (A t)
      (r (predApply u p) qn1)) (qn, V (case p of { Pred s _ -> showPred $ Pred s h})) ps
  }}};

findProof is pred psn = fpair psn \ps n -> case lookup pred ps of
  { Nothing -> case pred of { Pred s t -> case lookup s is of
    { Nothing -> error $ "no instances: " ++ s
    ; Just insts -> findInst (findProof is) psn pred insts
    }}
  ; Just s -> (psn, V s)
  };

prove' ienv sub psn a = case a of
  { E x -> case x of
    { Basic _ -> (psn, a)
    ; Const _ -> (psn, a)
    ; StrCon _ -> (psn, a)
    ; Proof raw -> findProof ienv (predApply sub raw) psn
    }
  ; V _ -> (psn, a)
  ; A x y -> let { p1 = prove' ienv sub psn x } in fpair p1 \psn1 x1 ->
    second (A x1) (prove' ienv sub psn1 y)
  ; L s t -> second (L s) (prove' ienv sub psn t)
  ; Pa _ -> undefined
  ; Ca _ _ -> undefined
  };

--prove :: [(String, [Qual])] -> (Type, Ast) -> Subst -> (Qual, Ast)
prove ienv s ta sub = fpair ta \t a ->
  fpair (prove' ienv sub ([], 0) a) \psn x -> fpair psn \ps _ ->
  let { applyDicts expr = foldl A expr $ map (V . snd) ps }
  in (Qual (map fst ps) (apply sub t), foldr L (overFree s applyDicts x) $ map snd ps);

dictVars ps n = flst ps ([], n) \p pt -> first ((p, '*':showInt n ""):) (dictVars pt $ n + 1);

-- qi = Qual of instance, e.g. Eq t => [t] -> [t] -> Bool
inferMethod ienv dcs typed qi def = fpair def \s expr ->
  fpair (infer dcs typed [] expr (Just [], 0)) \ta msn ->
  case lookup s typed of
    { Nothing -> error $ "no such method: " ++ s
    -- e.g. qac = Eq a => a -> a -> Bool, some AST (product of single method)
    ; Just qac -> fpair msn \ms n -> case ms of
      { Nothing -> error "method: type mismatch"
      ; Just sub -> fpair (instantiate (fst qac) n) \q1 n1 -> case q1 of { Qual psc tc -> case psc of
        { [] -> undefined  -- Unreachable.
        ; (:) headPred shouldBeNull -> case qi of { Qual psi ti ->
          case headPred of { Pred _ headT -> case match headT ti of
          { Nothing -> undefined
          -- e.g. Eq t => [t] -> [t] -> Bool
          -- instantiate and match it against type of ta
          ; Just subc ->
            fpair (instantiate (Qual psi $ apply subc tc) n1) \q2 n2 ->
            case q2 of { Qual ps2 t2 -> fpair ta \tx ax ->
              case match (apply sub tx) t2 of
                { Nothing -> error "class/instance type conflict"
                ; Just subx -> snd $ prove' ienv (subx @@ sub) (dictVars ps2 0) ax
              }}}}}}}}};

inferInst ienv dcs typed inst = fpair inst \cl qds -> fpair qds \q ds ->
  case q of { Qual ps t -> let { s = showPred $ Pred cl t } in
  (s, (,) (noQual $ TC "DICT") $ foldr L (L "@" $ foldl A (V "@") (map (inferMethod ienv dcs typed q) ds)) (map snd $ fst $ dictVars ps 0))
  };

reverse = foldl (flip (:)) [];
inferDefs ienv defs dcs typed = flst defs (Right $ reverse typed) \edef rest -> case edef of
  { Left def -> fpair def \s expr ->
    fpair (infer dcs typed [(s, TV "self!")] expr (Just [], 0)) \ta msn ->
      fpair msn \ms _ -> case prove ienv s ta <$> (unify (TV "self!") (fst ta) ms) of
    { Nothing -> Left ("bad type: " ++ s)
    ; Just qa -> inferDefs ienv rest dcs ((s, qa):typed)
    }
  ; Right inst -> inferDefs ienv rest dcs (inferInst ienv dcs typed inst:typed)
  };

showQual q = case q of { Qual ps t -> concatMap showPred ps ++ showType t };

untangle = foldr ($) (Neat [] [] prims [] [] []);

dumpTypes s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> fneat (untangle prog) \ienv fs typed dcs ffis exs -> case inferDefs ienv fs dcs typed of
  { Left err -> err
  ; Right typed -> concatMap (\p -> fpair p \s qa -> s ++ " :: " ++ showQual (fst qa) ++ "\n") typed
  };

last' x xt = flst xt x \y yt -> last' y yt;
last xs = flst xs undefined last';
init xs = flst xs undefined \x xt -> flst xt [] \_ _ -> x : init xt;
intercalate sep xs = flst xs [] \x xt -> x ++ concatMap (sep ++) xt;

argList t = case t of
  { TC s -> [TC s]
  ; TV s -> [TV s]
  ; TAp g y -> case g of
    { TC s -> case y of
      { TC u -> ife (s == "IO") [TC u] undefined
      ; TV _ -> undefined
      ; TAp _ _ -> undefined
      }
    ; TV s -> undefined
    ; TAp f x -> case f of
      { TC s -> ife (s == "->") (x : argList y) undefined
      ; TV s -> undefined
      ; TAp _ _ -> undefined
      }
    }
  };

cTypeName t = case t of
  { TC s -> ife (s == "()") "void" $
    ife (s == "Int") "int" $
    ife (s == "Char") "char" $ error $ "bad type constant: " ++ s
  ; TV _ -> undefined
  ; TAp _ _ -> undefined
  };

ffiDeclare namet = fpair namet \name t -> let { tys = argList t } in concat
  [ cTypeName $ last tys
  , " "
  , name
  , "("
  , intercalate "," $ cTypeName <$> init tys
  , ");\n"
  ];

ffiArgs n t = case t of
  { TC s -> ("", ((True, s), n))
  ; TV s -> undefined
  ; TAp g y -> case g of
    { TC s -> case y of
      { TC u -> ife (s == "IO") ("", ((False, u), n)) undefined
      ; TV _ -> undefined
      ; TAp _ _ -> undefined
      }
    ; TV s -> undefined
    ; TAp f x -> case f of
      { TC s -> ife (s == "->") (first ((ife (3 <= n) ", " "" ++ "num(" ++ showInt n ")") ++) $ ffiArgs (n + 1) y) undefined
      ; TV s -> undefined
      ; TAp _ _ -> undefined
      }
    }
  };

ffiDefine n ffis = case ffis of
  { [] -> id
  ; (:) x xt -> fpair x \name t -> fpair (ffiArgs 2 t) \args pRetCount -> fpair pRetCount \pRet count -> fpair pRet \isPure ret ->
    let
      { lazyn = ("lazy(" ++) . showInt (ife isPure (count - 1) (count + 1)) . (", " ++)
      ; aa tgt = "app(arg(" ++ showInt (count + 1) "), " ++ tgt ++ "), arg(" ++ showInt count ")"
      ; longDistanceCall = name ++ "(" ++ args ++ ")"
      } in
    ("case " ++) . showInt n . (": " ++) . ife (ret == "()")
      ((longDistanceCall ++) . (';':) . lazyn . ((ife isPure "'I', 'K'" (aa "'K'") ++ "); break;") ++) . ffiDefine (n - 1) xt)
      (lazyn . ((ife isPure ("'#', " ++ longDistanceCall) (aa $ "app('#', " ++ longDistanceCall ++ ")") ++ "); break;") ++) . ffiDefine (n - 1) xt)
  };

getContents = getChar >>= \n -> ife (n <= 255) ((chr n:) <$> getContents) (pure []);

compile s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> fneat (untangle prog) \ienv fs typed dcs ffis exs -> case inferDefs ienv fs dcs typed of
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
    foldr (\p f -> fpair p \x y -> maybe undefined showInt (mlookup y tab) . (", " ++) . f) id exs .
    ("};\n" ++) .
    ("static const u root_size=" ++) . showInt (length exs) . (";\n" ++) $
    flst exs ("int main(){rts_init();rts_reduce(" ++ maybe undefined showInt (mlookup (fst $ last qas) tab) ");return 0;}") $ \_ _ ->
      concat $ zipWith (\p n -> "EXPORT(f" ++ showInt n ", \"" ++ fst p ++ "\", " ++ showInt n ")\n") exs (upFrom 0)
  };

main = getContents >>= putStr . compile;
