-- Output bare memory dump instead of ION assembly.
infixr 9 .;
infixl 7 *;
infixl 6 + , -;
infixr 5 ++;
infixl 4 <*> , <$> , <* , *>;
infix 4 == , <=;
infixl 3 && , <|>;
infixl 2 ||;
infixr 0 $;
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Int where { (==) = intEq };
undefined = undefined;
($) f x = f x;
id x = x;
flip f x y = f y x;
(&) x f = f x;
data Bool = True | False;
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
fpair p = \f -> case p of { (,) x y -> f x y };
fst p = case p of { (,) x y -> x };
snd p = case p of { (,) x y -> y };
first f p = fpair p \x y -> (f x, y);
second f p = fpair p \x y -> (x, f y);
ife a b c = case a of { True -> b ; False -> c };
not a = case a of { True -> False; False -> True };
(.) f g x = f (g x);
(||) f g = ife f True (ife g True False);
(&&) f g = ife f (ife g True False) False;
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

maybe n j m = case m of { Nothing -> n; Just x -> j x };

foldr c n l = flst l n (\h t -> c h(foldr c n t));
foldr1 c l = maybe undefined id (flst l undefined (\h t -> foldr (\x m -> Just (case m of { Nothing -> x ; Just y -> c x y })) Nothing l));
foldl f a bs = foldr (\b g x -> g (f x b)) (\x -> x) bs a;
foldl1 f bs = flst bs undefined (\h t -> foldl f h t);
elem k xs = foldr (\x t -> ife (x == k) True t) False xs;
find f xs = foldr (\x t -> ife (f x) (Just x) t) Nothing xs;
(++) = flip (foldr (:));
concat = foldr (++) [];
itemize c = c:[];
map = flip (foldr . ((:) .)) [];
concatMap = (concat .) . map;
fmaybe m n j = case m of { Nothing -> n; Just x -> j x };
lookup s = foldr (\h t -> fpair h (\k v -> ife (s == k) (Just v) t)) Nothing;

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
data Extra = Basic Int | Const Int | Proof Pred;
data Ast = E Extra | V String | A Ast Ast | L String Ast;
ro = E . Basic . ord;

pure x = \inp -> Just (x, inp);
sat' f = \h t -> ife (f h) (pure h t) Nothing;
sat f inp = flst inp Nothing (sat' f);
bind f m = case m of
  { Nothing -> Nothing
  ; Just x -> fpair x f
  };
ap x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
(<*>) = ap;
fmap f x = ap (pure f) x;
(<$>) = fmap;
(>>=) x y = \inp -> bind (\a t -> y a t) (x inp);
(<|>) x y = \inp -> case x inp of
  { Nothing -> y inp
  ; Just x -> Just x
  };
liftA2 f x y = ap (fmap f x) y;
(*>) = liftA2 \x y -> y;
(<*) = liftA2 \x y -> x;
many p = liftA2 (:) p (many p) <|> pure [];
some p = liftA2 (:) p (many p);
sepBy1 p sep = liftA2 (:) p (many (sep *> p));
sepBy p sep = sepBy1 p sep <|> pure [];

char c = sat \x -> x == c;
between x y p = x *> (p <* y);
com = char '-' *> between (char '-') (char '\n') (many (sat \c -> not (c == '\n')));
sp = many ((itemize <$> (sat (\c -> (c == ' ') || (c == '\n')))) <|> com);
spc f = f <* sp;
spch = spc . char;
wantWith pred f inp = bind (sat' pred) (f inp);
want f s inp = wantWith (s ==) f inp;

paren = between (spch '(') (spch ')');
small = sat \x -> ((x <= 'z') && ('a' <= x)) || (x == '_');
large = sat \x -> (x <= 'Z') && ('A' <= x);
digit = sat \x -> (x <= '9') && ('0' <= x);
varLex = liftA2 (:) small (many (small <|> large <|> digit <|> char '\''));
conId = spc (liftA2 (:) large (many (small <|> large <|> digit <|> char '\'')));
keyword s = spc (want varLex s);
varId = spc (wantWith (\s -> not $ s == "of" || s == "where") varLex);
opLex = some (sat (\c -> elem c ":!#$%&*+./<=>?@\\^|-~"));
op = spc opLex <|> between (spch '`') (spch '`') varId;
var = varId <|> paren (spc opLex);

anyOne = fmap itemize (spc (sat (\c -> True)));
listify = fmap (foldr (\h t -> A (A (V ":") h) t) (V "[]"));
escChar = char '\\' *> ((sat (\c -> elem c "'\"\\")) <|> ((\c -> '\n') <$> char 'n'));
litOne delim = fmap (E . Const . ord) (escChar <|> sat (\c -> not (c == delim)));
litInt = E . Const . foldl (\n d -> 10*n + ord d - ord '0') 0 <$> spc (some digit);
litStr = listify (between (char '"') (spch '"') (many (litOne '"')));
litChar = between (char '\'') (spch '\'') (litOne '\'');
lit = litStr <|> litChar <|> litInt;
sqLst r = listify (between (spch '[') (spch ']') (sepBy r (spch ',')));
alt r = (,) <$> (conId <|> (itemize <$> paren (spch ':' <|> spch ',')) <|> ((:) <$> spch '[' <*> (itemize <$> spch ']'))) <*> (flip (foldr L) <$> many varId <*> (want op "->" *> r));
braceSep f = between (spch '{') (spch '}') (sepBy f (spch ';'));
alts r = braceSep (alt r);
cas' x as = foldl A (V (concatMap (('|':) . fst) as)) (x:map snd as);
cas r = cas' <$> between (keyword "case") (keyword "of") r <*> alts r;
lamCase r = keyword "case" *> (L "of" . cas' (V "of") <$> alts r);
lam r = spch '\\' *> (lamCase r <|> liftA2 (flip (foldr L)) (some varId) (char '-' *> (spch '>' *> r)));

thenComma r = spch ',' *> (((\x y -> A (A (V ",") y) x) <$> r) <|> pure (A (V ",")));
parenExpr r = (&) <$> r <*> (((\v a -> A (V v) a) <$> op) <|> thenComma r <|> pure id);
rightSect r = ((\v a -> A (A (ro 'C') (V v)) a) <$> (op <|> (itemize <$> spch ','))) <*> r;
section r = paren (parenExpr r <|> rightSect r);

isFree v expr = case expr of
  { E _ -> False
  ; V s -> s == v
  ; A x y -> isFree v x || isFree v y
  ; L w t -> not (v == w || not (isFree v t))
  };

maybeFix s x = ife (isFree s x) (A (ro 'Y') (L s x)) x;

def r = liftA2 (,) var (liftA2 (flip (foldr L)) (many varId) (spch '=' *> r));
addLets ls x = foldr (\p t -> fpair p (\name def -> A (L name t) $ maybeFix name def)) x ls;
letin r = addLets <$> between (keyword "let") (keyword "in") (braceSep (def r)) <*> r;

atom r = letin r <|> sqLst r <|> section r <|> cas r <|> lam r <|> (paren (spch ',') *> pure (V ",")) <|> fmap V (conId <|> var) <|> lit;
aexp r = fmap (foldl1 A) (some (atom r));
fix f = f (fix f);

data Assoc = NAssoc | LAssoc | RAssoc;
eqAssoc x y = case x of
  { NAssoc -> case y of { NAssoc -> True  ; LAssoc -> False ; RAssoc -> False }
  ; LAssoc -> case y of { NAssoc -> False ; LAssoc -> True  ; RAssoc -> False }
  ; RAssoc -> case y of { NAssoc -> False ; LAssoc -> False ; RAssoc -> True }
  };
precOf s precTab = fmaybe (lookup s precTab) 5 fst;
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

data Constr = Constr String [Type];
data Pred = Pred String Type;
data Qual = Qual [Pred] Type;

data Top = Adt Type [Constr] | Def (String, Ast) | Class String Type [(String, Type)] | Inst String Qual [(String, Ast)];

arr a b = TAp (TAp (TC "->") a) b;

bType r = foldl1 TAp <$> some r;
_type r = foldr1 arr <$> sepBy (bType r) (spc (want opLex "->"));
typeConst = (\s -> ife (s == "String") (TAp (TC "[]") (TC "Int")) (TC s)) <$> conId;
aType = paren ((&) <$> _type aType <*> ((spch ',' *> ((\a b -> TAp (TAp (TC ",") b) a) <$> _type aType)) <|> pure id)) <|>
  typeConst <|> (TV <$> varId) <|>
  (spch '[' *> (spch ']' *> pure (TC "[]") <|> TAp (TC "[]") <$> (_type aType <* spch ']')));

simpleType c vs = foldl TAp (TC c) (map TV vs);

adt = Adt <$> between (keyword "data") (spch '=') (simpleType <$> conId <*> many varId) <*> (sepBy (Constr <$> conId <*> many aType) (spch '|'));

prec = (\c -> ord c - ord '0') <$> spc digit;
fixityList a n os = map (\o -> (o, (n, a))) os;
fixityDecl kw a = between (keyword kw) (spch ';') (fixityList a <$> prec <*> sepBy op (spch ','));
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc;

noQual = Qual [];

genDecl = (,) <$> var <*> (char ':' *> spch ':' *> _type aType);
classDecl = keyword "class" *> (Class <$> conId <*> (TV <$> varId) <*> (keyword "where" *> braceSep genDecl));

inst = _type aType;
instDecl r = keyword "instance" *>
  ((\ps cl ty defs -> Inst cl (Qual ps ty) defs) <$>
  (((itemize .) . Pred <$> conId <*> (inst <* want op "=>")) <|> pure [])
    <*> conId <*> inst <*> (keyword "where" *> braceSep (def r)));

tops precTab = sepBy
  (   adt
  <|> Def <$> def (expr precTab 0)
  <|> classDecl
  <|> instDecl (expr precTab 0)
  ) (spch ';');
program' = sp *> (((":", (5, RAssoc)):) . concat <$> many fixity) >>= tops;

-- Primitives.

program = (
  [ Adt (TAp (TC "[]") (TV "a")) [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]]
  , Adt (TAp (TAp (TC ",") (TV "a")) (TV "b")) [Constr "," [TV "a", TV "b"]]] ++) <$> program';

prims = let
  { ii = arr (TC "Int") (TC "Int")
  ; iii = arr (TC "Int") ii
  ; bin s = A (A (ro 'B') (ro 'T')) (A (ro 'T') (ro s)) } in map (second (first noQual)) $
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin '='))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin 'L'))
    , ("chr", (ii, ro 'I'))
    , ("ord", (ii, ro 'I'))
    , ("succ", (ii, A (ro 'T') (A (E $ Const $ 1) (ro '+'))))
    , ("putChar", (arr (TC "Int") (TAp (TC "IO") (TV "a")), A (ro 'T') (A (ro 'F') (ro $ chr 1))))
    , ("ioBind", (arr (TAp (TC "IO") (TV "a")) (arr (arr (TV "a") (TAp (TC "IO") (TV "b"))) (TAp (TC "IO") (TV "b"))), ro 'C'))
    , ("ioPure", (arr (TV "a") (TAp (TC "IO") (TV "a")), A (A (ro 'B') (ro 'C')) (ro 'T')))
    ] ++ map (\s -> (itemize s, (iii, bin s))) "+-*/%";

ifz n = ife (0 == n);
showInt' n = ifz n id ((showInt' (n/10)) . ((:) (chr (48+(n%10)))));
showInt n s = ifz n ('0':) (showInt' n) s;

-- Conversion to De Bruijn indices.

data LC = Ze | Su LC | Pass Int | La LC | App LC LC;

debruijn m n e = case e of
  { E x -> case x of
    { Basic b -> Pass b
    ; Const c -> App (Pass $ ord '#') (Pass c)
    ; Proof _ -> undefined
    }
  ; V v -> maybe (fmaybe (mlookup v m) undefined Pass) id $
    foldr (\h found -> ife (h == v) (Just Ze) (maybe Nothing (Just . Su) found)) Nothing n
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

nolam m x = case babs $ debruijn m [] x of
  { Defer -> undefined
  ; Closed d -> d
  ; Need e -> undefined
  ; Weak e -> undefined
  };

enc mem t = case t of
  { Lf n -> (n, mem)
  ; Nd x y -> fpair (enc mem x) \p mem' -> fpair (enc mem' y) \q mem'' ->
    ife (p == ord 'C' && q == ord 'I') (ord 'T', mem) $
    ife (p == ord 'B' && q == ord 'I') (ord 'I', mem) $
    fpair mem'' \hp bs -> (hp, (hp + 2, q:p:bs))
  };

asm ds = foldl (\tabmem def -> fpair def \s t -> fpair tabmem \tab mem ->
  fpair (enc mem $ nolam tab t) \p m' -> (insert s p tab, m'))
  (Tip, (128, [])) ds;

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

maybeMap f = maybe Nothing (Just . f);

unify a b = maybe Nothing \s -> maybeMap (@@ s) (mgu unify (apply s a) (apply s b));

--instantiate' :: Type -> Int -> [(String, Type)] -> ((Type, Int), [(String, Type)])
instantiate' t n tab = case t of
  { TC s -> ((t, n), tab)
  ; TV s -> case lookup s tab of
    { Nothing -> let { va = TV (s ++ '_':showInt n "") } in ((va, n + 1), (s, va):tab)
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

--infer' :: SymTab -> Subst -> Ast -> (Maybe Subst, Int) -> ((Type, Ast), (Maybe Subst, Int))
infer' typed loc ast csn = fpair csn \cs n ->
  let
    { va = TV ('_':showInt n "")
    ; insta ty = fpair (instantiate ty n) \q n1 -> case q of { Qual preds ty -> ((ty, foldl A ast (map (E . Proof) preds)), (cs, n1)) }
    }
  in case ast of
  { E x -> case x of
    { Basic b -> ife (b == ord 'Y')
      (insta $ noQual $ arr (arr (TV "a") (TV "a")) (TV "a"))
      $ ife (b == ord 'C')
      (insta $ noQual $ arr (arr (TV "a") (arr (TV "b") (TV "c"))) (arr (TV "b") (arr (TV "a") (TV "c"))))
      undefined
    ; Const c -> ((TC "Int",  ast), csn)
    ; Proof _ -> undefined
    }
  ; V s -> fmaybe (lookup s loc)
    (fmaybe (lookup s typed) undefined $ insta . fst)
    ((, csn) . (, ast))
  ; A x y ->
    fpair (infer' typed loc x (cs, n + 1)) \tax csn1 -> fpair tax \tx ax ->
    fpair (infer' typed loc y csn1) \tay csn2 -> fpair tay \ty ay ->
      ((va, A ax ay), first (unify tx (arr ty va)) csn2)
  ; L s x -> first (\ta -> fpair ta \t a -> (arr va t, L s a)) (infer' typed ((s, va):loc) x (cs, n + 1))
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

all f = foldr (&&) True . map f;

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
    { Nothing -> undefined  -- No instances!
    ; Just insts -> findInst (findProof is) psn pred insts
    }}
  ; Just s -> (psn, V s)
  };

prove' ienv sub psn a = case a of
  { E x -> case x of
    { Basic _ -> (psn, a)
    ; Const _ -> (psn, a)
    ; Proof raw -> findProof ienv (predApply sub raw) psn
    }
  ; V _ -> (psn, a)
  ; A x y -> let { p1 = prove' ienv sub psn x } in fpair p1 \psn1 x1 ->
    second (A x1) (prove' ienv sub psn1 y)
  ; L s t -> second (L s) (prove' ienv sub psn t)
  };

--prove :: [(String, [Qual])] -> (Type, Ast) -> Subst -> (Qual, Ast)
prove ienv ta sub = fpair ta \t a ->
  fpair (prove' ienv sub ([], 0) a) \psn x -> fpair psn \ps _ ->
  (Qual (map fst ps) (apply sub t), foldr L x (map snd ps));

data Either a b = Left a | Right b;

dictVars ps n = flst ps ([], n) \p pt -> first ((p, '*':showInt n ""):) (dictVars pt $ n + 1);

-- qi = Qual of instance, e.g. Eq t => [t] -> [t] -> Bool
inferMethod ienv typed qi def = fpair def \s expr ->
  fpair (infer' typed [] expr (Just [], 0)) \ta msn ->
  case lookup s typed of
    { Nothing -> undefined -- No such method.
    -- e.g. qac = Eq a => a -> a -> Bool, some AST (product of single method)
    ; Just qac -> fpair msn \ms n -> case ms of
      { Nothing -> undefined  -- Type check fails.
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
                { Nothing -> undefined  -- Class/instance type conflict.
                ; Just subx -> snd $ prove' ienv (subx @@ sub) (dictVars ps2 0) ax
              }}}}}}}}};

genProduct ds = foldr L (L "*" $ foldl A (V "*") $ map V ds) ds;

inferInst ienv typed inst = fpair inst \cl qds -> fpair qds \q ds ->
  case q of { Qual ps t -> let { s = showPred $ Pred cl t } in
  (s, (,) (noQual $ TC "DICT") $ maybeFix s $ foldr L (foldl A (genProduct $ map fst ds) (map (inferMethod ienv typed q) ds)) (map snd $ fst $ dictVars ps 0))
  };

reverse = foldl (flip (:)) [];
inferDefs ienv defs typed = flst defs (Right $ reverse typed) \edef rest -> case edef of
  { Left def -> fpair def \s expr -> fpair (infer' typed [] (maybeFix s expr) (Just [], 0)) \ta msn ->
  fpair msn \ms _ -> case maybeMap (prove ienv ta) ms of
    { Nothing -> Left ("bad type: " ++ s)
    ; Just qa -> inferDefs ienv rest ((s, qa):typed)
    }
  ; Right inst -> inferDefs ienv rest (inferInst ienv typed inst:typed)
  };

conOf con = case con of { Constr s _ -> s };
mkCase t cs = (concatMap (('|':) . conOf) cs,
  ( noQual $ arr t $ foldr arr (TV "case") $ map (\c -> case c of { Constr _ ts -> foldr arr (TV "case") ts}) cs
  , ro 'I'));
mkStrs = snd . foldl (\p u -> fpair p (\s l -> ('*':s, s : l))) ("*", []);
index n s ss = case ss of
  { [] -> undefined
  ; (:) t ts -> ife (s == t) n $ index (n + 1) s ts
  };
length = foldr (\_ n -> n + 1) 0;
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs);
-- scottConstr t cs c = case c of { Constr s ts -> (s,
--   ( noQual $ foldr arr t ts
--   , Pick (index 0 s $ map conOf cs) (length ts) (length cs - 1)))
--   };
scottConstr t cs c = case c of { Constr s ts -> (s,
  ( noQual $ foldr arr t ts
  , scottEncode (map conOf cs) s $ mkStrs ts)) };
mkAdtDefs t cs = mkCase t cs : map (scottConstr t cs) cs;

--  * instance environment
--  * definitions, including those of instances
--  * Typed ASTs, ready for compilation, including ADTs and methods,
--    e.g. (==), (Eq a => a -> a -> Bool, select-==)
data Neat = Neat [(String, [Qual])] [Either (String, Ast) (String, (Qual, [(String, Ast)]))] [(String, (Qual, Ast))];

fneat neat f = case neat of { Neat a b c -> f a b c };

select f xs acc = flst xs (Nothing, acc) \x xt -> ife (f x) (Just x, xt ++ acc) (select f xt (x:acc));

addInstance s q is = fpair (select (\kv -> s == fst kv) is []) \m xs -> case m of
  { Nothing -> (s, [q]):xs
  ; Just sqs -> second (q:) sqs:xs
  };

mkSel ms s = L "*" $ A (V "*") $ foldr L (V $ '*':s) $ map (('*':) . fst) ms;

untangle = foldr (\top acc -> fneat acc \ienv fs typed -> case top of
  { Adt t cs -> Neat ienv fs (mkAdtDefs t cs ++ typed)
  ; Def f -> Neat ienv (Left f : fs) typed
  ; Class classId v ms -> Neat ienv fs (
    map (\st -> fpair st \s t -> (s, (Qual [Pred classId v] t, mkSel ms s))) ms
    ++ typed)
  ; Inst cl q ds -> Neat (addInstance cl q ienv) (Right (cl, (q, ds)):fs) typed
  }) (Neat [] [] prims);

infer prog = fneat (untangle prog) inferDefs;

showQual q = case q of { Qual ps t -> concatMap showPred ps ++ showType t };

dumpTypes s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> case infer prog of
  { Left err -> err
  ; Right typed -> concatMap (\p -> fpair p \s qa -> s ++ " :: " ++ showQual (fst qa) ++ "\n") typed
  };

prepAsm entry tabmem = fpair tabmem \tab mem ->
  maybe undefined id (mlookup entry tab) : reverse (snd mem);

last' x xt = flst xt x \y yt -> last' y yt;
last xs = flst xs undefined last';

compile s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> case infer prog of
  { Left err -> err
  ; Right qas -> foldr (\n s -> showInt n $ ',':s) rest $ prepAsm (fst $ last qas) $ asm $ map (second snd) qas
  }
