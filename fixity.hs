------------------------------------------------------------------------
-- Supports infix declarations.
--
-- All infix declarations must precede the definitions.
--
-- Supports let expressions.
------------------------------------------------------------------------
data Bool = True | False;
data Maybe a = Nothing | Just a;
ife a b c = case a of { True -> b ; False -> c };
not a = case a of { True -> False; False -> True };
(.) f g x = f (g x);
(||) f g = ife f True g;
(&&) f g = ife f g False;
flst xs n c = case xs of { [] -> n; (:) h t -> c h t };
lstEq xs ys = case xs of
  { [] -> flst ys True (\h t -> False)
  ; (:) x xt -> flst ys False (\y yt -> ife (x == y) (lstEq xt yt) False)
  };
id x = x;
flip f x y = f y x;
(&) x f = f x;
foldr c n l = flst l n (\h t -> c h(foldr c n t));
foldl = \f a bs -> foldr (\b g x -> g (f x b)) (\x -> x) bs a;
undefined = undefined;
foldl1 f bs = flst bs undefined (\h t -> foldl f h t);
elem k xs = foldr (\x t -> ife (x == k) True t) False xs;
find f xs = foldr (\x t -> ife (f x) (Just x) t) Nothing xs;
(++) = flip (foldr (:));
concat = foldr (++) [];
wrap c = c:[];

fst p = case p of { (,) x y -> x };
snd p = case p of { (,) x y -> y };
uncurry f p = case p of { (,) x y -> f x y };
second f = uncurry \x y -> (x, f y);
maybe n j m = case m of { Nothing -> n; Just x -> j x };

pure x = \inp -> Just (x, inp);
bind f = maybe Nothing (uncurry f);
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
between x y p = x *> (p <* y);
satHelper f = \h t -> ife (f h) (pure h t) Nothing;
sat f inp = flst inp Nothing (satHelper f);

char c = sat \x -> x == c;
com = char '-' *> char '-' <* many (sat \c -> not (c == '\n'));
sp = many ((wrap <$> (sat (\c -> (c == ' ') || (c == '\n')))) <|> com);
spc f = f <* sp;
spch = spc . char;
wantWith pred f inp = bind (satHelper pred) (f inp);
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

data Ast = R String | V String | A Ast Ast | L String Ast;

lam r = spch '\\' *> liftA2 (flip (foldr L)) (some varId) (char '-' *> (spch '>' *> r));
listify = fmap (foldr (\h t -> A (A (R ":") h) t) (R "K"));
escChar = char '\\' *> ((sat (\c -> elem c "'\"\\")) <|> ((\c -> '\n') <$> char 'n'));
litOne delim = fmap (\c -> R ('#':(wrap c))) (escChar <|> sat (\c -> not (c == delim)));
litInt = R . ('(':) . (++ ")") <$> spc (some digit);
litStr = listify (between (char '"') (spch '"') (many (litOne '"')));
litChar = between (char '\'') (spch '\'') (litOne '\'');
lit = litStr <|> litChar <|> litInt;
sqLst r = listify (between (spch '[') (spch ']') (sepBy r (spch ',')));
alt r = (conId <|> var <|> (undefined <$> sqLst r) <|> (undefined <$> paren (spch ','))) *> (flip (foldr L) <$> many varId <*> (want op "->" *> r));
braceSep f = between (spch '{') (spch '}') (sepBy f (spch ';'));
alts r = braceSep (alt r);
altize h t = foldl A h t;
cas r = altize <$> between (keyword "case") (keyword "of") r <*> alts r;

thenComma r = spch ',' *> (((\x y -> A (A (V ",") y) x) <$> r) <|> pure (A (V ",")));
parenExpr r = (&) <$> r <*> (((\v a -> A (V v) a) <$> op) <|> thenComma r <|> pure id);
rightSect r = ((\v a -> A (A (R "C") (V v)) a) <$> (op <|> (wrap <$> spch ','))) <*> r;
section r = paren (parenExpr r <|> rightSect r);

isFree v expr = case expr of
  { R s -> False
  ; V s -> lstEq s v
  ; A x y -> isFree v x || isFree v y
  ; L w t -> not ((lstEq v w) || not (isFree v t))
  };
maybeFix s x = (s, ife (isFree s x) (A (R "Y") (L s x)) x);
def r = liftA2 maybeFix var (liftA2 (flip (foldr L)) (many varId) (spch '=' *> r));
addLets ls x = foldr (\p t -> uncurry (\name def -> A (L name t) def) p) x ls;
letin r = addLets <$> between (keyword "let") (keyword "in") (braceSep (def r)) <*> r;

atom r = letin r <|> sqLst r <|> section r <|> cas r <|> lam r <|> (paren (spch ',') *> pure (V ",")) <|> fmap V (conId <|> var) <|> lit;
aexp r = fmap (foldl1 A) (some (atom r));
fix f = f (fix f);
lstLookup s = foldr (\h t -> uncurry (\k v -> ife (lstEq s k) (Just v) t) h) Nothing;

data Assoc = NAssoc | LAssoc | RAssoc;
eqAssoc x y = case x of
  { NAssoc -> case y of { NAssoc -> True  ; LAssoc -> False ; RAssoc -> False }
  ; LAssoc -> case y of { NAssoc -> False ; LAssoc -> True  ; RAssoc -> False }
  ; RAssoc -> case y of { NAssoc -> False ; LAssoc -> False ; RAssoc -> True }
  };
precOf s precTab = maybe 9 fst (lstLookup s precTab);
assocOf s precTab = maybe LAssoc snd (lstLookup s precTab);
opWithPrec precTab n = wantWith (\s -> n == precOf s precTab) op;
opFold precTab e xs = case xs of
  { [] -> e
  ; (:) x xt -> case find (\y -> not (eqAssoc (assocOf (fst x) precTab) (assocOf (fst y) precTab))) xt of
    { Nothing -> case assocOf (fst x) precTab of
      { NAssoc -> case xt of
        { [] -> uncurry (\op y -> A (A (V op) e) y) x
        ; (:) y yt -> undefined
        }
      ; LAssoc -> foldl (\a b -> uncurry (\op y -> A (A (V op) a) y) b) e xs
      ; RAssoc -> (foldr (\a b -> uncurry (\op y -> \e -> A (A (V op) e) (b y)) a) id xs) e
      }
    ; Just y -> undefined
    }
  };
expr precTab = fix \r n -> ife (n <= 9) (liftA2 (opFold precTab) (r (succ n)) (many (liftA2 (\a b -> (a,b)) (opWithPrec precTab n) (r (succ n))))) (aexp (r 0));

aType = (undefined <$> paren (some aType <* ((spch ',' *> some aType) <|> pure []) )) <|> (undefined <$> (conId <|> varId)) <|> (undefined <$> between (spch '[') (spch ']') aType);
map = flip (foldr . ((.) (:))) [];
dataDefs cs = map (\cas -> uncurry (\c as -> (c, foldr L (foldl (\a b -> A a (V b)) (V c) as) (as ++ map fst cs))) cas) cs;

dataArgs = (snd . foldl (\p u -> uncurry (\s l -> ('x':s, s : l)) p) ("x", [])) <$> many aType;
adt = between (keyword "data") (spch '=') (conId *> many varId) *> (dataDefs <$> (sepBy ((,) <$> conId <*> dataArgs) (spch '|')));

prec = (\c -> ord c - ord '0') <$> spc digit;
fixityList a n os = map (\o -> (o, (n, a))) os;
fixityDecl kw a = between (keyword kw) (spch ';') (fixityList a <$> prec <*> sepBy op (spch ','));
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc;

funs precTab = concat <$> sepBy (adt <|> (wrap <$> def (expr precTab 0))) (spch ';');
program = sp *> (((":", (5, RAssoc)):) . concat <$> many fixity) >>= funs;

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

insPrim = (map (second R) ([(":", ":"), (",", "``BCT"), ("chr", "I"), ("ord", "I"), ("succ", "`T`(1)+")] ++ map (second ("``BT`T" ++)) [("<=", "L"), ("==", "="), ("-", "-"), ("/", "/"), ("%", "%"), ("+", "+"), ("*", "*")]) ++);
rank ds v = foldr (\d t -> ife (lstEq v (fst d)) (\n -> ('@':) . (n:)) (t . (\n -> succ n))) undefined ds ' ';
shows f t = case t of
  { R s -> (s++)
  ; V v -> f v
  ; A x y -> ('`':) . shows f x . shows f y
  ; L w t -> undefined
  };
dump tab = foldr (\h t -> shows (rank tab) (nolam (snd h)) (';':t)) "" tab;
compile s = maybe "?" (dump . insPrim . fst) (program s);
