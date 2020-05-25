------------------------------------------------------------------------
-- Accepted by GHC, with a small wrapper.
--
-- Integer constants.
------------------------------------------------------------------------
data Bool = True | False;
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
(++) = flip (foldr (:));
concat = foldr (++) [];
itemize c = c:[];
data Pair x y = Pair x y;
fpair p = \f -> case p of { Pair x y -> f x y };
fst p = case p of { Pair x y -> x };
snd p = case p of { Pair x y -> y };
second f p = fpair p \x y -> Pair x (f y);
data Maybe a = Nothing | Just a;
fmaybe m n j = case m of { Nothing -> n; Just x -> j x };
lstLookup s = foldr (\h t -> fpair h (\k v -> ife (lstEq s k) (Just v) t)) Nothing;

pure x = \inp -> Just (Pair x inp);
bind f m = case m of
  { Nothing -> Nothing
  ; Just x -> fpair x f
  };
ap x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
(<*>) = ap;
fmap f x = ap (pure f) x;
(<$>) = fmap;
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

data Ast = R String | V String | A Ast Ast | L String Ast;

char c = sat \x -> x == c;
com = char '-' *> between (char '-') (char '\n') (many (sat \c -> not (c == '\n')));
sp = many ((itemize <$> (sat (\c -> (c == ' ') || (c == '\n')))) <|> com);
spc f = f <* sp;
spch = spc . char;
wantWith pred f inp = bind (satHelper pred) (f inp);
want f s inp = wantWith (lstEq s) f inp;
paren = between (spch '(') (spch ')');
letter = sat \x -> ((x <= 'z') && ('a' <= x)) || ((x <= 'Z') && ('A' <= x));
digit = sat \x -> (x <= '9') && ('0' <= x);
varLex = liftA2 (:) letter (many (letter <|> digit));
keyword s = spc (want varLex s);
varId = spc (wantWith (not . lstEq "of") varLex);
opLex = some (sat (\c -> elem c ":!#$%&*+./<=>?@\\^|-~"));
op = spc opLex <|> between (spch '`') (spch '`') varId;
var = varId <|> paren (spc opLex);
lam r = spch '\\' *> liftA2 (flip (foldr L)) (some varId) (char '-' *> (spch '>' *> r));
listify = fmap (foldr (\h t -> A (A (R ":") h) t) (R "K"));
escChar = char '\\' *> ((sat (\c -> elem c "'\"\\")) <|> ((\c -> '\n') <$> char 'n'));
litOne delim = fmap (\c -> R ('#':(itemize c))) (escChar <|> sat (\c -> not (c == delim)));
litInt = R . ('(':) . (++ ")") <$> spc (some digit);
litStr = listify (between (char '"') (spch '"') (many (litOne '"')));
litChar = between (char '\'') (spch '\'') (litOne '\'');
lit = litStr <|> litChar <|> litInt;
sqLst r = listify (between (spch '[') (spch ']') (sepBy r (spch ',')));
alt r = (var <|> (undefined <$> sqLst r) <|> (undefined <$> paren (spch ','))) *> (flip (foldr L) <$> many varId <*> (want op "->" *> r));
alts r = between (spch '{') (spch '}') (sepBy (alt r) (spch ';'));
altize h t = foldl A h t;
cas r = altize <$> between (keyword "case") (keyword "of") r <*> alts r;

thenComma r = spch ',' *> (((\x y -> A (A (V ",") y) x) <$> r) <|> pure (A (V ",")));
parenExpr r = (&) <$> r <*> (((\v a -> A (V v) a) <$> op) <|> thenComma r <|> pure id);
rightSect r = ((\v a -> A (A (R "C") (V v)) a) <$> (op <|> (itemize <$> spch ','))) <*> r;
section r = paren (parenExpr r <|> rightSect r);

atom r = sqLst r <|> section r <|> cas r <|> lam r <|> (paren (spch ',') *> pure (V ",")) <|> fmap V var <|> lit;
aexp r = fmap (foldl1 A) (some (atom r));
expr = liftA2 (foldl (&)) (aexp expr) (many (liftA2 (\f b a -> A (A (V f) a) b) op (aexp expr)));

isFree v expr = case expr of
  { R s -> False
  ; V s -> lstEq s v
  ; A x y -> isFree v x || isFree v y
  ; L w t -> not ((lstEq v w) || not (isFree v t))
  };
maybeFix s x = Pair s (ife (isFree s x) (A (R "Y") (L s x)) x);
def = liftA2 maybeFix var (liftA2 (flip (foldr L)) (many var) (spch '=' *> expr));

aType = paren (some var) <|> (undefined <$> var) <|> (undefined <$> between (spch '[') (spch ']') aType);
map = flip (foldr . ((.) (:))) [];
dataDefs cs = map (\cas -> fpair cas (\c as -> Pair c (foldr L (foldl (\a b -> A a (V b)) (V c) as) (as ++ map fst cs)))) cs;

dataArgs = (snd . foldl (\p u -> fpair p (\s l -> Pair ('x':s) (s : l))) (Pair "x" [])) <$> many aType;
adt = between (keyword "data") (spch '=') (some var) *> (dataDefs <$> (sepBy (Pair <$> var <*> dataArgs) (spch '|')));
program = sp *> (concat <$> sepBy (adt <|> (itemize <$> def)) (spch ';'));

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

primTab = Pair "," "``BCT" : (Pair "ord" "I" : (Pair "succ" "`T`(1)+" : map (second ("``BT`T" ++)) [Pair "<=" "L", Pair "==" "=", Pair "-" "-", Pair "+" "+", Pair "*" "*"]));
prim s = fmaybe (lstLookup s primTab) s id;
rank ds v = foldr (\d t -> ife (lstEq v (fst d)) (\n -> '@':(n:[])) (t . (\n -> succ n))) (\n -> prim v) ds ' ';
show ds t = case t of
  { R s -> s
  ; V v -> rank ds v
  ; A x y -> '`':(show ds x ++ show ds y)
  ; L w t -> undefined
  };
dump tab ds = flst ds "" \h t -> show tab (nolam (snd h)) ++ (';':dump tab t);
compile s = fmaybe (program s) "?" ((\ds -> dump ds ds) . fst);
