------------------------------------------------------------------------
-- Type inference.
--
-- `String` is hardcoded to mean `[Int]`.
------------------------------------------------------------------------
infixr 9 .;
infixr 5 : , ++;
infixl 4 <*> , <$> , <* , *>;
infixl 3 <|>;
infixr 0 $;
undefined = undefined;
($) f x = f x;
id x = x;
flip f x y = f y x;
(&) x f = f x;
data Bool = True | False;
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
lstEq xs ys = case xs of
  { [] -> flst ys True (\h t -> False)
  ; (:) x xt -> flst ys False (\y yt -> ife (x == y) (lstEq xt yt) False)
  };
(++) xs ys = flst xs ys (\x xt -> x:xt ++ ys);

foldr c n l = flst l n (\h t -> c h(foldr c n t));
foldl = \f a bs -> foldr (\b g x -> g (f x b)) (\x -> x) bs a;
foldl1 f bs = flst bs undefined (\h t -> foldl f h t);
elem k xs = foldr (\x t -> ife (x == k) True t) False xs;
find f xs = foldr (\x t -> ife (f x) (Just x) t) Nothing xs;
concat = foldr (++) [];
itemize c = c:[];
map = flip (foldr . ((:) .)) [];
concatMap = (concat .) . map;
any f xs = foldr (\x t -> ife (f x) True t) False xs;
fmaybe m n j = case m of { Nothing -> n; Just x -> j x };
lstLookup s = foldr (\h t -> fpair h (\k v -> ife (lstEq s k) (Just v) t)) Nothing;

data Ast = R String | V String | A Ast Ast | L String Ast;

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
want f s inp = wantWith (lstEq s) f inp;

paren = between (spch '(') (spch ')');
small = sat \x -> ((x <= 'z') && ('a' <= x)) || (x == '_');
large = sat \x -> (x <= 'Z') && ('A' <= x);
digit = sat \x -> (x <= '9') && ('0' <= x);
varLex = liftA2 (:) small (many (small <|> large <|> digit <|> char '\''));
conId = spc (liftA2 (:) large (many (small <|> large <|> digit <|> char '\'')));
keyword s = spc (want varLex s);
varId = spc (wantWith (not . lstEq "of") varLex);
opLex = some (sat (\c -> elem c ":!#$%&*+./<=>?@\\^|-~"));
op = spc opLex <|> between (spch '`') (spch '`') varId;
var = varId <|> paren (spc opLex);
anyOne = fmap itemize (spc (sat (\c -> True)));
lam r = spch '\\' *> liftA2 (flip (foldr L)) (some varId) (char '-' *> (spch '>' *> r));
listify = fmap (foldr (\h t -> A (A (V ":") h) t) (V "[]"));
escChar = char '\\' *> ((sat (\c -> elem c "'\"\\")) <|> ((\c -> '\n') <$> char 'n'));
litOne delim = fmap (\c -> R ('#':itemize c)) (escChar <|> sat (\c -> not (c == delim)));
litInt = R . ('(':) . (++ ")") <$> spc (some digit);
litStr = listify (between (char '"') (spch '"') (many (litOne '"')));
litChar = between (char '\'') (spch '\'') (litOne '\'');
lit = litStr <|> litChar <|> litInt;
sqLst r = listify (between (spch '[') (spch ']') (sepBy r (spch ',')));
alt r = (,) <$> (conId <|> (itemize <$> paren (spch ':' <|> spch ',')) <|> ((:) <$> spch '[' <*> (itemize <$> spch ']'))) <*> (flip (foldr L) <$> many varId <*> (want op "->" *> r));
braceSep f = between (spch '{') (spch '}') (sepBy f (spch ';'));
alts r = braceSep (alt r);
cas' x as = foldl A (V (concatMap (('|':) . fst) as)) (x:map snd as);
cas r = cas' <$> between (keyword "case") (keyword "of") r <*> alts r;

thenComma r = spch ',' *> (((\x y -> A (A (V ",") y) x) <$> r) <|> pure (A (V ",")));
parenExpr r = (&) <$> r <*> (((\v a -> A (V v) a) <$> op) <|> thenComma r <|> pure id);
rightSect r = ((\v a -> A (A (V "\\C") (V v)) a) <$> (op <|> (itemize <$> spch ','))) <*> r;
section r = paren (parenExpr r <|> rightSect r);

isFree v expr = case expr of
  { R s -> False
  ; V s -> lstEq s v
  ; A x y -> isFree v x || isFree v y
  ; L w t -> not ((lstEq v w) || not (isFree v t))
  };
maybeFix s x = (s, ife (isFree s x) (A (V "\\Y") (L s x)) x);
def r = liftA2 maybeFix var (liftA2 (flip (foldr L)) (many varId) (spch '=' *> r));
addLets ls x = foldr (\p t -> fpair p (\name def -> A (L name t) def)) x ls;
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
precOf s precTab = fmaybe (lstLookup s precTab) 5 fst;
assocOf s precTab = fmaybe (lstLookup s precTab) LAssoc snd;
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

data Type = TC String | TV String | TAp Type Type;
data Constr = Constr String [Type];
data Adt = Adt Type [Constr];

_type r = foldl1 TAp <$> some r;
typeConstant = (\s -> ife (lstEq "String" s) (TAp (TC "[]") (TC "Int")) (TC s)) <$> conId;
aType = paren ((&) <$> _type aType <*> ((spch ',' *> ((\a b -> TAp (TAp (TC ",") b) a) <$> _type aType)) <|> pure id)) <|> typeConstant <|> (TV <$> varId) <|> (TAp (TC "[]") <$> between (spch '[') (spch ']') (_type aType));

simpleType c vs = foldl TAp (TC c) (map TV vs);

adt = Adt <$> between (keyword "data") (spch '=') (simpleType <$> conId <*> many varId) <*> (sepBy (Constr <$> conId <*> many aType) (spch '|'));

prec = (\c -> ord c - ord '0') <$> spc digit;
fixityList a n os = map (\o -> (o, (n, a))) os;
fixityDecl kw a = between (keyword kw) (spch ';') (fixityList a <$> prec <*> sepBy op (spch ','));
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc;

arr a b = TAp (TAp (TC "->") a) b;

-- type Program = ([(String, (Type, Ast))], [(String, Ast)])
prims = let
  { ii = arr (TC "Int") (TC "Int")
  ; iii = arr (TC "Int") ii
  ; bin s = R $ "``BT`T" ++ s } in
    [ ("\\Y", (arr (arr (TV "a") (TV "a")) (TV "a"), R "Y"))
    , ("\\C", (arr (arr (TV "a") (arr (TV "b") (TV "c"))) (arr (TV "b") (arr (TV "a") (TV "c"))), R "C"))
    , ("==", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "="))
    , ("<=", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "L"))
    , ("chr", (ii, R "I"))
    , ("ord", (ii, R "I"))
    , ("succ", (ii, R "`T`(1)+"))
    ] ++ map (\s -> (s, (iii, bin s))) ["+", "-", "*", "/", "%"];

conOf con = case con of { Constr s _ -> s };
mkCase t cs = (concatMap (('|':) . conOf) cs,
  ( arr t $ foldr arr (TV "case") $ map (\c -> case c of { Constr _ ts -> foldr arr (TV "case") ts}) cs
  , L "x" $ V "x"));
mkStrs = snd . foldl (\p u -> fpair p (\s l -> ('*':s, s : l))) ("*", []);
-- For example, creates `Just = \x a b -> b x`.
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs);
scottConstr t cs c = case c of { Constr s ts -> (s,
  ( foldr arr t ts
  , scottEncode (map conOf cs) s $ mkStrs ts)) };
mkAdtDefs a = case a of { Adt t cs -> mkCase t cs : map (scottConstr t cs) cs };

addAdt = first . (++) . mkAdtDefs;
addDef = second . (:);

tops precTab = foldr ($) ([], []) <$> sepBy (addAdt <$> adt <|> addDef <$> def (expr precTab 0)) (spch ';');

program' = sp *> (concat <$> many fixity) >>= tops;
program = first (prims ++)
  . addAdt (Adt (TAp (TC "[]") (TV "a")) [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]])
  . addAdt (Adt (TAp (TAp (TC ",") (TV "a")) (TV "b")) [Constr "," [TV "a", TV "b"]])
  <$> program';

ifz n = ife (0 == n);
showInt' n = ifz n id ((showInt' (n/10)) . ((:) (chr (48+(n%10)))));
showInt n s = ifz n ('0':) (showInt' n) s;

rank ds v = foldr (\d t -> ife (lstEq v (fst d)) (\n -> '[':showInt n "]") (t . succ)) undefined ds 0;
show ds t = case t of
  { R s -> s
  ; V v -> rank ds v
  ; A x y -> '`':show ds x ++ show ds y
  ; L w t -> undefined
  };
data LC = Ze | Su LC | Pass Ast | La LC | App LC LC;

debruijn n e = case e of
  { R s -> Pass (R s)
  ; V v -> foldr (\h m -> ife (lstEq h v) Ze (Su m)) (Pass (V v)) n
  ; A x y -> App (debruijn n x) (debruijn n y)
  ; L s t -> La (debruijn (s:n) t)
  };

data Sem = Defer | Closed Ast | Need Sem | Weak Sem;

ldef = \r y -> case y of
  { Defer -> Need (Closed (A (A (R "S") (R "I")) (R "I")))
  ; Closed d -> Need (Closed (A (R "T") d))
  ; Need e -> Need (r (Closed (A (R "S") (R "I"))) e)
  ; Weak e -> Need (r (Closed (R "T")) e)
  };

lclo = \r d y -> case y of
  { Defer -> Need (Closed d)
  ; Closed dd -> Closed (A d dd)
  ; Need e -> Need (r (Closed (A (R "B") d)) e)
  ; Weak e -> Weak (r (Closed d) e)
  };

lnee = \r e y -> case y of
  { Defer -> Need (r (r (Closed (R "S")) e) (Closed (R "I")))
  ; Closed d -> Need (r (Closed (A (R "R") d)) e)
  ; Need ee -> Need (r (r (Closed (R "S")) e) ee)
  ; Weak ee -> Need (r (r (Closed (R "C")) e) ee)
  };

lwea = \r e y -> case y of
  { Defer -> Need e
  ; Closed d -> Weak (r e (Closed d))
  ; Need ee -> Need (r (r (Closed (R "B")) e) ee)
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
  ; Pass s -> Closed s
  ; La t -> case babs t of
    { Defer -> Closed (R "I")
    ; Closed d -> Closed (A (R "K") d)
    ; Need e -> e
    ; Weak e -> babsa (Closed (R "K")) e
    }
  ; App x y -> babsa (babs x) (babs y)
  };

nolam x = case babs (debruijn [] x) of
  { Defer -> undefined
  ; Closed d -> d
  ; Need e -> undefined
  ; Weak e -> undefined
  };

dump tab ds = flst ds "" \h t -> show tab (nolam (snd h)) ++ (';':dump tab t);
asm prog = (\ds -> dump ds ds) $
  fpair prog \typed defs -> map (second snd) typed ++ defs;

compile s = fmaybe ((asm <$> program) s) "?" fst;

apply sub t = case t of
  { TC v -> t
  ; TV v -> fmaybe (lstLookup v sub) t id
  ; TAp a b -> TAp (apply sub a) (apply sub b)
  };

(@@) s1 s2 = map (second (apply s1)) s2 ++ s1;

occurs s t = case t of
  { TC v -> False
  ; TV v -> lstEq s v
  ; TAp a b -> occurs s a || occurs s b
  };

varBind s t = case t of
  { TC v -> Just [(s, t)]
  ; TV v -> ife (lstEq v s) (Just []) (Just [(s, t)])
  ; TAp a b -> ife (occurs s t) Nothing (Just [(s, t)])
  };

mgu unify t u = case t of
  { TC a -> case u of
    { TC b -> ife (lstEq a b) (Just []) Nothing
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

maybeMap f m = fmaybe m Nothing (Just . f);

unify a b m = fmaybe m Nothing \s -> maybeMap (@@ s) (mgu unify (apply s a) (apply s b));

--instantiate' :: Type -> Int -> [(String, Type)] -> ((Type, Int), [(String, Type)])
instantiate' t n tab = case t of
  { TC s -> ((t, n), tab)
  ; TV s -> case lstLookup s tab of
    { Nothing -> let { va = TV (s ++ '_':showInt n "") } in ((va, n + 1), (s, va):tab)
    ; Just v -> ((v, n), tab)
    }
  ; TAp x y ->
    fpair (instantiate' x n tab) \tn1 tab1 ->
    fpair tn1 \t1 n1 -> fpair (instantiate' y n1 tab1) \tn2 tab2 ->
    fpair tn2 \t2 n2 -> ((TAp t1 t2, n2), tab2)
  };

--instantiate :: Type -> Int -> (Type, Int)
instantiate t n = fst (instantiate' t n []);

--type SymTab = [(String, (Type, Ast))];
--type Subst = [(String, Type)];
--infer' :: SymTab -> Subst -> Ast -> (Maybe Subst, Int) -> (Type, (Maybe Subst, Int))
infer' typed loc ast csn = fpair csn \cs n ->
  let { va = TV ('_':showInt n "") } in case ast of
  { R s -> (TC "Int", csn)
  ; V s -> fmaybe (lstLookup s loc) (fmaybe (lstLookup s typed) undefined
    (\ta -> second (cs,) (instantiate (fst ta) n))) (, csn)
  ; A x y ->
    fpair (infer' typed loc x (cs, n + 1)) \tx csn1 ->
    fpair (infer' typed loc y csn1) \ty csn2 ->
    (va, first (unify tx (arr ty va)) csn2)
  ; L s x -> first (TAp (TAp (TC "->") va)) (infer' typed ((s, va):loc) x (cs, n + 1))
  };

apSub tsn = fpair tsn \ty msn -> fpair msn \ms _ -> maybeMap (flip apply ty) ms;

data Either a b = Left a | Right b;

inferDefs typed defs = flst defs (Right typed) \def rest -> fpair def \s expr ->
  case apSub (infer' typed [] expr (Just [], 0)) of
    { Nothing -> Left ("bad type: " ++ s)
    ; Just t -> inferDefs ((s, (t, expr)) : typed) rest
    };

infer prog = fpair prog inferDefs;

showType t = case t of
  { TC s -> s
  ; TV s -> s
  ; TAp a b -> concat ["(", showType a, " ", showType b, ")"]
  };

dumpTypes s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> case infer prog of
  { Left err -> err
  ; Right typed -> concatMap (\p -> fpair p \s ta -> s ++ " :: " ++ showType (fst ta) ++ "\n") typed
  };

typedCompile s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> case infer prog of
  { Left err -> err
  ; Right _ -> asm prog
  };
