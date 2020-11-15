-- Copyright © 2019 Ben Lynn
-- This file is part of blynn-compiler.

-- blynn-compiler is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, only under version 3 of
-- the License.

-- blynn-compiler is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with blynn-compiler.  If not, see
-- <https://www.gnu.org/licenses/>.
-- Standalone compiler.
infixr 9 .;
infixl 7 *;
infixl 6 + , -;
infixr 5 ++;
infixl 4 <*> , <$> , <* , *>;
infix 4 == , <=;
infixl 3 && , <|>;
infixl 2 ||;
infixl 1 >> , >>=;
infixr 0 $;

ffi "putchar" putChar :: Int -> IO Int;
ffi "getchar" getChar :: IO Int;

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
data Either a b = Left a | Right b;
fpair p = \f -> case p of { (,) x y -> f x y };
fst p = case p of { (,) x y -> x };
snd p = case p of { (,) x y -> y };
first f p = fpair p \x y -> (f x, y);
second f p = fpair p \x y -> (x, f y);
ife a b c = case a of { True -> b ; False -> c };
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

maybe n j m = case m of { Nothing -> n; Just x -> j x };

foldr c n l = flst l n (\h t -> c h(foldr c n t));
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
elem k xs = foldr (\x t -> x == k || t) False xs;
find f xs = foldr (\x t -> ife (f x) (Just x) t) Nothing xs;
(++) = flip (foldr (:));
concat = foldr (++) [];
wrap c = c:[];
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
arr a b = TAp (TAp (TC "->") a) b;
data Extra = Basic Int | Const Int | StrCon String | Proof Pred;
data Ast = E Extra | V String | A Ast Ast | L String Ast;
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
  -- e.g.
  --   Left ("id", L "x" (V "x"))
  --   Right ("Monad", (TC "IO" , [("return", ...), (">>=", ...)]))
  [Either (String, Ast) (String, (Qual, [(String, Ast)]))]
  -- | Typed ASTs, ready for compilation, including ADTs and methods,
  -- e.g. (==), (Eq a => a -> a -> Bool, select-==)
  [(String, (Qual, Ast))]
  -- | FFI declarations.
  [(String, Type)]
  -- | Exports.
  [(String, String)]
  ;

parse p inp = case p of { Parser f -> f inp };

fneat neat f = case neat of { Neat a b c d e -> f a b c d e };

conOf con = case con of { Constr s _ -> s };
mkCase t cs = (concatMap (('|':) . conOf) cs,
  ( noQual $ arr t $ foldr arr (TV "case") $ map (\c -> case c of { Constr _ ts -> foldr arr (TV "case") ts}) cs
  , ro 'I'));
mkStrs = snd . foldl (\p u -> fpair p (\s l -> ('@':s, s : l))) ("@", []);
length = foldr (\_ n -> n + 1) 0;
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs);
scottConstr t cs c = case c of { Constr s ts -> (s,
  ( noQual $ foldr arr t ts
  , scottEncode (map conOf cs) s $ mkStrs ts)) };
mkAdtDefs t cs = mkCase t cs : map (scottConstr t cs) cs;

select f xs acc = flst xs (Nothing, acc) \x xt -> ife (f x) (Just x, xt ++ acc) (select f xt (x:acc));

addInstance s q is = fpair (select (\kv -> s == fst kv) is []) \m xs -> case m of
  { Nothing -> (s, [q]):xs
  ; Just sqs -> second (q:) sqs:xs
  };

mkSel ms s = L "*" $ A (V "*") $ foldr L (V $ '*':s) $ map (('*':) . fst) ms;

ifz n = ife (0 == n);
showInt' n = ifz n id ((showInt' (n/10)) . ((:) (chr (48+(n%10)))));
showInt n s = ifz n ('0':) (showInt' n) s;

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

addAdt t cs acc = fneat acc \ienv fs typed ffis exs -> Neat ienv fs (mkAdtDefs t cs ++ typed) ffis exs;
addClass classId v ms acc = fneat acc \ienv fs typed ffis exs -> Neat ienv fs (
    map (\st -> fpair st \s t -> (s, (Qual [Pred classId v] t, mkSel ms s))) ms
    ++ typed) ffis exs;
addInst cl q ds acc = fneat acc \ienv fs typed ffis exs -> Neat (addInstance cl q ienv) (Right (cl, (q, ds)):fs) typed ffis exs;
addFFI foreignname ourname t acc = fneat acc \ienv fs typed ffis exs -> Neat ienv fs (
    (ourname, (Qual [] t, mkFFIHelper 0 t $ A (ro 'F') (ro $ chr $ length ffis))) : typed) ((foreignname, t):ffis) exs;
addDef f acc = fneat acc \ienv fs typed ffis exs -> Neat ienv (Left f : fs) typed ffis exs;
addExport e f acc = fneat acc \ienv fs typed ffis exs -> Neat ienv fs typed ffis ((e, f):exs);

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
varId = spc $ wantWith (\s -> not $ elem s ["of", "where", "if", "then", "else"]) varLex;
opTail = many $ char ':' <|> symbo;
conSym = spc $ liftA2 (:) (char ':') opTail;
varSym = spc $ liftA2 (:) symbo opTail;
con = conId <|> paren conSym;
var = varId <|> paren varSym;
op = varSym <|> conSym <|> between (spch '`') (spch '`') (conId <|> varId);
conop = conSym <|> between (spch '`') (spch '`') conId;
listify = foldr (\h t -> A (A (V ":") h) t) (V "[]");
escChar = char '\\' *> ((sat (\c -> elem c "'\"\\")) <|> ((\c -> '\n') <$> char 'n'));
litOne delim = escChar <|> sat \c -> not (c == delim);
litInt = E . Const . foldl (\n d -> 10*n + ord d - ord '0') 0 <$> spc (some digit);
litStr = between (char '"') (spch '"') $ many (litOne '"');
litChar = E . Const . ord <$> between (char '\'') (spch '\'') (litOne '\'');
lit = E . StrCon <$> litStr <|> litChar <|> litInt;
sqLst r = between (spch '[') (spch ']') $ listify <$> sepBy r (spch ',');

gcon = conId <|> paren (conSym <|> (wrap <$> spch ',')) <|> ((:) <$> spch '[' <*> (wrap <$> spch ']'));

pat = (,) <$> gcon <*> many varId <|> (\x c y -> (c, [x, y])) <$> varId <*> conop <*> varId;

lamAlt conArgs expr = fpair conArgs \con args -> (con, foldr L expr args);

alt r = lamAlt <$> pat <*> (want varSym "->" *> r);

braceSep f = between (spch '{') (spch '}') (sepBy f (spch ';'));
alts r = braceSep (alt r);
cas' x as = foldl A (V (concatMap (('|':) . fst) as)) (x:map snd as);
cas r = cas' <$> between (keyword "case") (keyword "of") r <*> alts r;
lamCase r = keyword "case" *> (L "of" . cas' (V "of") <$> alts r);
lam r = spch '\\' *> (lamCase r <|> liftA2 (flip (foldr L)) (some varId) (char '-' *> (spch '>' *> r)));

thenComma r = spch ',' *> (((\x y -> A (A (V ",") y) x) <$> r) <|> pure (A (V ",")));
parenExpr r = (&) <$> r <*> (((\v a -> A (V v) a) <$> op) <|> thenComma r <|> pure id);
rightSect r = ((\v a -> L "@" $ A (A (V v) $ V "@") a) <$> (op <|> (wrap <$> spch ','))) <*> r;
section r = spch '(' *> (parenExpr r <* spch ')' <|> rightSect r <* spch ')' <|> spch ')' *> pure (V "()"));

isFree v expr = case expr of
  { E _ -> False
  ; V s -> s == v
  ; A x y -> isFree v x || isFree v y
  ; L w t -> not (v == w) && isFree v t
  };

maybeFix s x = ife (isFree s x) (A (ro 'Y') (L s x)) x;

opDef x f y rhs = (f, L x $ L y rhs);
def r =
  opDef <$> varId <*> varSym <*> varId <*> (spch '=' *> r)
  <|> liftA2 (,) var (liftA2 (flip (foldr L)) (many varId) (spch '=' *> r));

addLets ls x = foldr (\p t -> fpair p (\name def -> A (L name t) $ maybeFix name def)) x ls;
letin r = addLets <$> between (keyword "let") (keyword "in") (braceSep (def r)) <*> r;
ifthenelse r = (\a b c -> A (A (A (V "if") a) b) c) <$>
  (keyword "if" *> r) <*> (keyword "then" *> r) <*> (keyword "else" *> r);
atom r = ifthenelse r <|> letin r <|> sqLst r <|> section r <|> cas r <|> lam r <|> (paren (spch ',') *> pure (V ",")) <|> fmap V (con <|> var) <|> lit;
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
    <*> conId <*> inst <*> (keyword "where" *> braceSep (def r)));

ffiDecl = keyword "ffi" *>
  (addFFI <$> litStr <*> var <*> (char ':' *> spch ':' *> _type aType));

tops precTab = sepBy
  (   adt
  <|> classDecl
  <|> instDecl (expr precTab 0)
  <|> ffiDecl
  <|> addDef <$> def (expr precTab 0)
  <|> keyword "export" *> (addExport <$> litStr <*> var)
  ) (spch ';');
program' = sp *> (((":", (5, RAssoc)):) . concat <$> many fixity) >>= tops;

-- Primitives.

program = parse $ (
  [ addAdt (TAp (TC "[]") (TV "a")) [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]]
  , addAdt (TAp (TAp (TC ",") (TV "a")) (TV "b")) [Constr "," [TV "a", TV "b"]]] ++) <$> program';

prims = let
  { ii = arr (TC "Int") (TC "Int")
  ; iii = arr (TC "Int") ii
  ; bin s = A (A (ro 'B') (ro 'T')) (A (ro 'T') (ro s)) } in map (second (first noQual)) $
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin '='))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin 'L'))
    , ("if", (arr (TC "Bool") $ arr (TV "a") $ arr (TV "a") (TV "a"), ro 'I'))
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
  ; Nd x y -> let { p = optim x; q = optim y } in
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

instance Functor Maybe where { fmap f = maybe Nothing (Just . f) };
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

--infer :: SymTab -> Subst -> Ast -> (Maybe Subst, Int) -> ((Type, Ast), (Maybe Subst, Int))
infer typed loc ast csn = fpair csn \cs n ->
  let
    { va = TV (showInt n "")
    ; insta ty = fpair (instantiate ty n) \q n1 -> case q of { Qual preds ty -> ((ty, foldl A ast (map (E . Proof) preds)), (cs, n1)) }
    }
  in case ast of
  { E x -> case x of
    { Basic b -> ife (b == ord 'Y')
      (insta $ noQual $ arr (arr (TV "a") (TV "a")) (TV "a"))
      undefined
    ; Const c -> ((TC "Int",  ast), csn)
    ; StrCon _ -> ((TAp (TC "[]") (TC "Int"),  ast), csn)
    ; Proof _ -> undefined
    }
  ; V s -> fmaybe (lookup s loc)
    (fmaybe (lookup s typed) (error $ "bad symbol: " ++ s) $ insta . fst)
    ((, csn) . (, ast))
  ; A x y ->
    fpair (infer typed loc x (cs, n + 1)) \tax csn1 -> fpair tax \tx ax ->
    fpair (infer typed loc y csn1) \tay csn2 -> fpair tay \ty ay ->
      ((va, A ax ay), first (unify tx (arr ty va)) csn2)
  ; L s x -> first (\ta -> fpair ta \t a -> (arr va t, L s a)) (infer typed ((s, va):loc) x (cs, n + 1))
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
    { Nothing -> error "no instances"
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
  };

overFree s f t = case t of
  { E _ -> t
  ; V s' -> ife (s == s') (f t) t
  ; A x y -> A (overFree s f x) (overFree s f y)
  ; L s' t' -> ife (s == s') t $ L s' $ overFree s f t'
  };

--prove :: [(String, [Qual])] -> String -> (Type, Ast) -> Subst -> (Qual, Ast)
prove ienv s ta sub = fpair ta \t a ->
  fpair (prove' ienv sub ([], 0) a) \psn x -> fpair psn \ps _ ->
  let { applyDicts expr = foldl A expr $ map (V . snd) ps }
  in (Qual (map fst ps) (apply sub t), foldr L (overFree s applyDicts x) $ map snd ps);

dictVars ps n = flst ps ([], n) \p pt -> first ((p, '*':showInt n ""):) (dictVars pt $ n + 1);

-- qi = Qual of instance, e.g. Eq t => [t] -> [t] -> Bool
inferMethod ienv typed qi def = fpair def \s expr ->
  fpair (infer typed [] expr (Just [], 0)) \ta msn ->
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

inferInst ienv typed inst = fpair inst \cl qds -> fpair qds \q ds ->
  case q of { Qual ps t -> let { s = showPred $ Pred cl t } in
  (s, (,) (noQual $ TC "DICT") $ foldr L (L "@" $ foldl A (V "@") (map (inferMethod ienv typed q) ds)) (map snd $ fst $ dictVars ps 0))
  };

reverse = foldl (flip (:)) [];
inferDefs ienv defs typed = flst defs (Right $ reverse typed) \edef rest -> case edef of
  { Left def -> fpair def \s expr ->
    fpair (infer typed [(s, TV "self!")] expr (Just [], 0)) \ta msn ->
      fpair msn \ms _ -> case prove ienv s ta <$> (unify (TV "self!") (fst ta) ms) of
    { Nothing -> Left ("bad type: " ++ s)
    ; Just qa -> inferDefs ienv rest ((s, qa):typed)
    }
  ; Right inst -> inferDefs ienv rest (inferInst ienv typed inst:typed)
  };

showQual q = case q of { Qual ps t -> concatMap showPred ps ++ showType t };

untangle = foldr ($) $ Neat [] [] prims [] [];

dumpTypes s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> fneat (untangle prog) \ienv fs typed ffis exs -> case inferDefs ienv fs typed of
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
  , intercalate "," $ map cTypeName $ init tys
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

upFrom n = n : upFrom (n + 1);
zipWith f xs ys = flst xs [] $ \x xt -> flst ys [] $ \y yt -> f x y : zipWith f xt yt;

compile s = fmaybe (program s) "parse error" \progRest ->
  fpair progRest \prog rest -> fneat (untangle prog) \ienv fs typed ffis exs -> case inferDefs ienv fs typed of
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
