------------------------------------------------------------------------------
-- Type classes.
------------------------------------------------------------------------------
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

maybe n j m = case m of { Nothing -> n; Just x -> j x };

foldr c n l = flst l n (\h t -> c h(foldr c n t));
foldr1 c l = maybe undefined id (flst l undefined (\h t -> foldr (\x m -> Just (case m of { Nothing -> x ; Just y -> c x y })) Nothing l));
foldl = \f a bs -> foldr (\b g x -> g (f x b)) (\x -> x) bs a;
foldl1 f bs = flst bs undefined (\h t -> foldl f h t);
elem k xs = foldr (\x t -> ife (x == k) True t) False xs;
find f xs = foldr (\x t -> ife (f x) (Just x) t) Nothing xs;
concat = foldr (++) [];
itemize c = c:[];
any f xs = foldr (\x t -> ife (f x) True t) False xs;

fmaybe m n j = case m of { Nothing -> n; Just x -> j x };
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
varId = spc (wantWith (\s -> not (lstEq "of" s || lstEq "where" s)) varLex);
opLex = some (sat (\c -> elem c ":!#$%&*+./<=>?@\\^|-~"));
op = spc opLex <|> between (spch '`') (spch '`') varId;
var = varId <|> paren (spc opLex);

data Type = TC String | TV String | TAp Type Type;
data Ast = R String | V String | A Ast Ast | L String Ast | Proof Pred;

map = flip (foldr . ((:) .)) [];
concatMap = (concat .) . map;

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
  ; Proof _ -> False
  };

def r = liftA2 (,) var (liftA2 (flip (foldr L)) (many varId) (spch '=' *> r));
addLets ls x = foldr (\p t -> fpair p (\name def -> A (L name t) def)) x ls;
letin r = addLets <$> between (keyword "let") (keyword "in") (braceSep (def r)) <*> r;

atom r = letin r <|> sqLst r <|> section r <|> cas r <|> lam r <|> (paren (spch ',') *> pure (V ",")) <|> fmap V (conId <|> var) <|> lit;
aexp r = fmap (foldl1 A) (some (atom r));
fix f = f (fix f);

lookupWith eq s = foldr (\h t -> fpair h (\k v -> ife (eq s k) (Just v) t)) Nothing;
lstLookup = lookupWith lstEq;

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

data Constr = Constr String [Type];
data Pred = Pred String Type;
data Qual = Qual [Pred] Type;

data Top = Adt Type [Constr] | Def (String, Ast) | Class String Type [(String, Type)] | Inst String Qual [(String, Ast)];

arr a b = TAp (TAp (TC "->") a) b;

bType r = foldl1 TAp <$> some r;
_type r = foldr1 arr <$> sepBy (bType r) (spc (want opLex "->"));
typeConstant = (\s -> ife (lstEq "String" s) (TAp (TC "[]") (TC "Int")) (TC s)) <$> conId;
aType = paren ((&) <$> _type aType <*> ((spch ',' *> ((\a b -> TAp (TAp (TC ",") b) a) <$> _type aType)) <|> pure id)) <|>
  typeConstant <|> (TV <$> varId) <|>
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
program' = sp *> (concat <$> many fixity) >>= tops;

eqPre = fmaybe (program' $ "class Eq a where { (==) :: a -> a -> Bool };\n" ++
  "instance Eq Int where { (==) = intEq };\n") undefined fst;

program = (
  (eqPre ++
  [ Adt (TAp (TC "[]") (TV "a")) [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]]
  , Adt (TAp (TAp (TC ",") (TV "a")) (TV "b")) [Constr "," [TV "a", TV "b"]]]) ++) <$> program';

prims = let
  { ii = arr (TC "Int") (TC "Int")
  ; iii = arr (TC "Int") ii
  ; bin s = R $ "``BT`T" ++ s } in map (second (first noQual)) $
    [ ("\\Y", (arr (arr (TV "a") (TV "a")) (TV "a"), R "Y"))
    , ("\\C", (arr (arr (TV "a") (arr (TV "b") (TV "c"))) (arr (TV "b") (arr (TV "a") (TV "c"))), R "C"))
    , ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "="))
    , ("<=", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "L"))
    , ("chr", (ii, R "I"))
    , ("ord", (ii, R "I"))
    , ("succ", (ii, R "`T`(1)+"))
    ] ++ map (\s -> (s, (iii, bin s))) ["+", "-", "*", "/", "%"];

ifz n = ife (0 == n);
showInt' n = ifz n id ((showInt' (n/10)) . ((:) (chr (48+(n%10)))));
showInt n s = ifz n ('0':) (showInt' n) s;

rank ds v = foldr (\d t -> ife (lstEq v (fst d)) (\n -> '[':showInt n "]") (t . succ)) undefined ds 0;
show ds t = case t of
  { R s -> s
  ; V v -> rank ds v
  ; A x y -> '`':show ds x ++ show ds y
  ; L w t -> undefined
  ; Proof _ -> undefined
  };
data LC = Ze | Su LC | Pass Ast | La LC | App LC LC;

debruijn n e = case e of
  { R s -> Pass (R s)
  ; V v -> foldr (\h m -> ife (lstEq h v) Ze (Su m)) (Pass (V v)) n
  ; A x y -> App (debruijn n x) (debruijn n y)
  ; L s t -> La (debruijn (s:n) t)
  ; Proof _ -> undefined
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
dump tab ds = flst ds ";" \h t -> show tab (nolam (snd h)) ++ (';':dump tab t);
asm ds = dump ds ds;

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

maybeMap f = maybe Nothing (Just . f);

unify a b = maybe Nothing \s -> maybeMap (@@ s) (mgu unify (apply s a) (apply s b));

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
  let { va = TV ('_':showInt n "") } in case ast of
  { R s -> ((TC "Int", ast), csn)
  ; V s -> fmaybe (lstLookup s loc)
    (fmaybe (lstLookup s typed) undefined
      \ta -> fpair (instantiate (fst ta) n) \q n1 -> case q of { Qual preds ty -> ((ty, foldl A ast (map Proof preds)), (cs, n1)) })
    ((, csn) . (, ast))
  ; A x y ->
    fpair (infer' typed loc x (cs, n + 1)) \tax csn1 -> fpair tax \tx ax ->
    fpair (infer' typed loc y csn1) \tay csn2 -> fpair tay \ty ay ->
      ((va, A ax ay), first (unify tx (arr ty va)) csn2)
  ; L s x -> first (\ta -> fpair ta \t a -> (arr va t, L s a)) (infer' typed ((s, va):loc) x (cs, n + 1))
  ; Proof _ -> undefined
  };

onType f pred = case pred of { Pred s t -> Pred s (f t) };

typeEq t u = case t of
  { TC s -> case u of
    { TC t -> lstEq t s
    ; TV _ -> False
    ; TAp _ _ -> False
    }
  ; TV s ->  case u of
    { TC _ -> False
    ; TV t -> lstEq t s
    ; TAp _ _ -> False
    }
  ; TAp a b -> case u of
    { TC _ -> False
    ; TV _ -> False
    ; TAp c d -> typeEq a c && typeEq b d
    }
  };

predEq p q = case p of { Pred s a -> case q of { Pred t b ->
  lstEq s t && typeEq a b }};

predApply sub p = onType (apply sub) p;

all f = foldr (&&) True . map f;

filter f = foldr (\x xs ->ife (f x) (x:xs) xs) [];

intersect xs ys = filter (\x -> fmaybe (find (lstEq x) ys) False (\_ -> True)) xs;

merge s1 s2 = ife (all (\v -> typeEq (apply s1 $ TV v) (apply s2 $ TV v))
  $ map fst s1 `intersect` map fst s2) (Just $ s1 ++ s2) Nothing;

match h t = case h of
  { TC a -> case t of
    { TC b -> ife (lstEq a b) (Just []) Nothing
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

findProof is pred psn = fpair psn \ps n -> case lookupWith predEq pred ps of
  { Nothing -> case pred of { Pred s t -> case lstLookup s is of
    { Nothing -> undefined  -- No instances!
    ; Just insts -> findInst (findProof is) psn pred insts
    }}
  ; Just s -> (psn, V s)
  };

prove' ienv sub psn a = case a of
  { R _ -> (psn, a)
  ; V _ -> (psn, a)
  ; A x y -> let { p1 = prove' ienv sub psn x } in fpair p1 \psn1 x1 ->
    second (A x1) (prove' ienv sub psn1 y)
  ; L s t -> second (L s) (prove' ienv sub psn t)
  ; Proof raw -> findProof ienv (predApply sub raw) psn
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
  case lstLookup s typed of
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

maybeFix s x = ife (isFree s x) (A (V "\\Y") (L s x)) x;

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
  , L "x" $ V "x"));
mkStrs = snd . foldl (\p u -> fpair p (\s l -> ('*':s, s : l))) ("*", []);
-- For example, creates `Just = \x a b -> b x`.
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs);
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

addInstance s q is = fpair (select (\kv -> lstEq s (fst kv)) is []) \m xs -> case m of
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

compile s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> case infer prog of
  { Left err -> err
  ; Right qas -> asm $ map (second snd) qas
  };
