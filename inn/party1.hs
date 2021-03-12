-- Modules.
module Main where
import Base
import Map
import Ast
import Lexer
import Unify
import RTS

-- Parser.
data ParseState = ParseState Ell (Map String (Int, Assoc))
data Parser a = Parser (ParseState -> Either String (a, ParseState))
getPrecs = Parser \st@(ParseState _ precs) -> Right (precs, st)
putPrecs precs = Parser \(ParseState s _) -> Right ((), ParseState s precs)
parse (Parser f) inp = f inp
instance Functor Parser where fmap f x = pure f <*> x
instance Applicative Parser where
  pure x = Parser \inp -> Right (x, inp)
  x <*> y = Parser \inp -> case parse x inp of
    Left e -> Left e
    Right (fun, t) -> case parse y t of
      Left e -> Left e
      Right (arg, u) -> Right (fun arg, u)
instance Monad Parser where
  return = pure
  (>>=) x f = Parser \inp -> case parse x inp of
    Left e -> Left e
    Right (a, t) -> parse (f a) t
instance Alternative Parser where
  empty = Parser \_ -> Left ""
  x <|> y = Parser \inp -> either (const $ parse y inp) Right $ parse x inp

ro = E . Basic . comEnum
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
  TAp (TAp (TC "->") x) y -> L (showInt n "") $ mkFFIHelper (n + 1) y $ A (V $ showInt n "") acc

updateDcs cs dcs = foldr (\(Constr s _) m -> insert s cs m) dcs cs
addAdt t cs (Neat tycl fs typed dcs ffis ffes ims) =
  Neat tycl fs (mkAdtDefs t cs ++ typed) (updateDcs cs dcs) ffis ffes ims

emptyTycl = Tycl [] []
addClass classId v (sigs, defs) (Neat tycl fs typed dcs ffis ffes ims) = let
  vars = take (size sigs) $ (`showInt` "") <$> [0..]
  selectors = zipWith (\var (s, t) -> (s, (Qual [Pred classId v] t,
    L "@" $ A (V "@") $ foldr L (V var) vars))) vars $ toAscList sigs
  methods = map (\s -> (s, mlookup s defs)) $ fst <$> toAscList sigs
  Tycl _ is = maybe emptyTycl id $ mlookup classId tycl
  tycl' = insert classId (Tycl methods is) tycl
  in Neat tycl' fs (selectors ++ typed) dcs ffis ffes ims

addInstance classId ps ty ds (Neat tycl fs typed dcs ffis ffes ims) = let
  Tycl ms is = maybe emptyTycl id $ mlookup classId tycl
  tycl' = insert classId (Tycl ms $ Instance ty name ps (fromList ds):is) tycl
  name = '{':classId ++ (' ':showType ty "") ++ "}"
  in Neat tycl' fs typed dcs ffis ffes ims

addFFI foreignname ourname t (Neat tycl fs typed dcs ffis ffes ims) =
  Neat tycl fs ((ourname, (Qual [] t, mkFFIHelper 0 t $ A (ro "F") (E $ Basic $ length ffis))) : typed) dcs ((foreignname, t):ffis) ffes ims
addDefs ds (Neat tycl fs typed dcs ffis ffes ims) = Neat tycl (ds ++ fs) typed dcs ffis ffes ims
addImport im (Neat tycl fs typed dcs ffis exs ims) = Neat tycl fs typed dcs ffis exs (im:ims)
addExport e f (Neat tycl fs typed dcs ffis ffes ims) = Neat tycl fs typed dcs ffis ((e, f):ffes) ims

want f = Parser \(ParseState inp precs) -> case ell inp of
  Right ((_, x), inp') -> (, ParseState inp' precs) <$> f x
  Left e -> Left e

braceYourself = Parser \(ParseState inp precs) -> case ell inp of
  Right ((_, Reserved "}"), inp') -> Right ((), ParseState inp' precs)
  _ -> case parseErrorRule inp of
    Left e -> Left e
    Right inp' -> Right ((), ParseState inp' precs)

res w = want \case
  Reserved s | s == w -> Right s
  _ -> Left $ "want \"" ++ w ++ "\""
wantInt = want \case
  Lit (Const i) -> Right i
  _ -> Left "want integer"
wantString = want \case
  Lit (StrCon s) -> Right s
  _ -> Left "want string"
wantConId = want \case
  ConId s -> Right s
  _ -> Left "want conid"
wantVarId = want \case
  VarId s -> Right s
  _ -> Left "want varid"
wantVarSym = want \case
  VarSym s -> Right s
  _ -> Left "want VarSym"
wantLit = want \case
  Lit x -> Right x
  _ -> Left "want literal"

paren = between (res "(") (res ")")
braceSep f = between (res "{") braceYourself $ foldr ($) [] <$> sepBy ((:) <$> f <|> pure id) (res ";")

maybeFix s x = if elem s $ fvPro [] x then A (V "fix") (L s x) else x

coalesce ds = flst ds [] \h@(s, x) t -> flst t [h] \(s', x') t' -> let
  f (Pa vsts) (Pa vsts') = Pa $ vsts ++ vsts'
  f _ _ = error "bad multidef"
  in if s == s' then coalesce $ (s, f x x'):t' else h:coalesce t

nonemptyTails [] = []
nonemptyTails xs@(x:xt) = xs : nonemptyTails xt

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
    in foldr (\(x:xt) t -> A (L x t) $ maybeFix x $ insLams xt $ suball $ maybe undefined id $ lookup x ls) (suball expr) tnames

data Assoc = NAssoc | LAssoc | RAssoc
instance Eq Assoc where
  NAssoc == NAssoc = True
  LAssoc == LAssoc = True
  RAssoc == RAssoc = True
  _ == _ = False
precOf s precTab = maybe 5 fst $ mlookup s precTab
assocOf s precTab = maybe LAssoc snd $ mlookup s precTab

parseErr s = Parser $ const $ Left s

opFold precTab f x xs = case xs of
  [] -> pure x
  (op, y):xt -> case find (\(op', _) -> assocOf op precTab /= assocOf op' precTab) xt of
    Nothing -> case assocOf op precTab of
      NAssoc -> case xt of
        [] -> pure $ f op x y
        y:yt -> parseErr "NAssoc repeat"
      LAssoc -> pure $ foldl (\a (op, y) -> f op a y) x xs
      RAssoc -> pure $ foldr (\(op, y) b -> \e -> f op e (b y)) id xs $ x
    Just y -> parseErr "Assoc clash"

qconop = want f <|> between (res "`") (res "`") (want g) where
  f (ConSym s) = Right s
  f (Reserved ":") = Right ":"
  f _ = Left ""
  g (ConId s) = Right s
  g _ = Left "want qconop"

wantqconsym = want \case
  ConSym s -> Right s
  Reserved ":" -> Right ":"
  _ -> Left "want qconsym"

op = wantqconsym <|> want f <|> between (res "`") (res "`") (want g) where
  f (VarSym s) = Right s
  f _ = Left ""
  g (VarId s) = Right s
  g (ConId s) = Right s
  g _ = Left "want op"

con = wantConId <|> paren wantqconsym
var = wantVarId <|> paren wantVarSym

tycon = want \case
  ConId s -> Right $ if s == "String" then TAp (TC "[]") (TC "Char") else TC s
  _ -> Left "want type constructor"

aType =
  res "(" *>
    (   res ")" *> pure (TC "()")
    <|> (foldr1 (TAp . TAp (TC ",")) <$> sepBy1 _type (res ",")) <* res ")")
  <|> tycon
  <|> TV <$> wantVarId
  <|> (res "[" *> (res "]" *> pure (TC "[]") <|> TAp (TC "[]") <$> (_type <* res "]")))
bType = foldl1 TAp <$> some aType
_type = foldr1 arr <$> sepBy bType (res "->")

fixityDecl w a = do
  res w
  n <- wantInt
  os <- sepBy op (res ",")
  precs <- getPrecs
  putPrecs $ foldr (\o m -> insert o (n, a) m) precs os
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc

cDecls = first fromList . second (fromList . coalesce) . foldr ($) ([], []) <$> braceSep cDecl
cDecl = first . (:) <$> genDecl <|> second . (++) <$> def

genDecl = (,) <$> var <*> (res "::" *> _type)

classDecl = res "class" *> (addClass <$> wantConId <*> (TV <$> wantVarId) <*> (res "where" *> cDecls))

simpleClass = Pred <$> wantConId <*> _type
scontext = (:[]) <$> simpleClass <|> paren (sepBy simpleClass $ res ",")

instDecl = res "instance" *>
  ((\ps cl ty defs -> addInstance cl ps ty defs) <$>
  (scontext <* res "=>" <|> pure [])
    <*> wantConId <*> _type <*> (res "where" *> (coalesce . concat <$> braceSep def)))

letin = addLets <$> between (res "let") (res "in") (coalesce . concat <$> braceSep def) <*> expr
ifthenelse = (\a b c -> A (A (A (V "if") a) b) c) <$>
  (res "if" *> expr) <*> (res "then" *> expr) <*> (res "else" *> expr)
listify = foldr (\h t -> A (A (V ":") h) t) (V "[]")

alts = braceSep $ (,) <$> pat <*> guards "->"
cas = Ca <$> between (res "case") (res "of") expr <*> alts
lamCase = res "case" *> (L "\\case" . Ca (V "\\case") <$> alts)
lam = res "\\" *> (lamCase <|> liftA2 onePat (some apat) (res "->" *> expr))

flipPairize y x = A (A (V ",") x) y
moreCommas = foldr1 (A . A (V ",")) <$> sepBy1 expr (res ",")
thenComma = res "," *> ((flipPairize <$> moreCommas) <|> pure (A (V ",")))
parenExpr = (&) <$> expr <*> (((\v a -> A (V v) a) <$> op) <|> thenComma <|> pure id)
rightSect = ((\v a -> L "@" $ A (A (V v) $ V "@") a) <$> (op <|> res ",")) <*> expr
section = res "(" *> (parenExpr <* res ")" <|> rightSect <* res ")" <|> res ")" *> pure (V "()"))

maybePureUnit = maybe (V "pure" `A` V "()") id
stmt = (\p x -> Just . A (V ">>=" `A` x) . onePat [p] . maybePureUnit) <$> pat <*> (res "<-" *> expr)
  <|> (\x -> Just . maybe x (\y -> (V ">>=" `A` x) `A` (L "_" y))) <$> expr
  <|> (\ds -> Just . addLets ds . maybePureUnit) <$> (res "let" *> (coalesce . concat <$> braceSep def))
doblock = res "do" *> (maybePureUnit . foldr ($) Nothing <$> braceSep stmt)

compQual =
  (\p xs e -> A (A (V "concatMap") $ onePat [p] e) xs)
    <$> pat <*> (res "<-" *> expr)
  <|> (\b e -> A (A (A (V "if") b) e) $ V "[]") <$> expr
  <|> addLets <$> (res "let" *> (coalesce . concat <$> braceSep def))

sqExpr = between (res "[") (res "]") $
  ((&) <$> expr <*>
    (   res ".." *>
      (   (\hi lo -> (A (A (V "enumFromTo") lo) hi)) <$> expr
      <|> pure (A (V "enumFrom"))
      )
    <|> res "|" *>
      ((. A (V "pure")) . foldr (.) id <$> sepBy1 compQual (res ","))
    <|> (\t h -> listify (h:t)) <$> many (res "," *> expr)
    )
  )
  <|> pure (V "[]")

atom = ifthenelse <|> doblock <|> letin <|> sqExpr <|> section
  <|> cas <|> lam <|> (paren (res ",") *> pure (V ","))
  <|> fmap V (con <|> var) <|> E <$> wantLit

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

gcon = wantConId <|> paren (wantqconsym <|> res ",") <|> ((++) <$> res "[" <*> (res "]"))

apat = PatVar <$> var <*> (res "@" *> (Just <$> apat) <|> pure Nothing)
  <|> flip PatVar Nothing <$> (res "_" *> pure "_")
  <|> flip PatCon [] <$> gcon
  <|> PatLit <$> wantLit
  <|> foldr (\h t -> PatCon ":" [h, t]) (PatCon "[]" [])
    <$> between (res "[") (res "]") (sepBy pat $ res ",")
  <|> paren (foldr1 pairPat <$> sepBy1 pat (res ",") <|> pure (PatCon "()" []))
  where pairPat x y = PatCon "," [x, y]

binPat f x y = PatCon f [x, y]
patP n = if n <= 9
  then getPrecs >>= \precTab
    -> patP (succ n) >>= \a
    -> many ((,) <$> withPrec precTab n qconop <*> patP (succ n)) >>= \as
    -> opFold precTab binPat a as
  else PatCon <$> gcon <*> many apat <|> apat
pat = patP 0

maybeWhere p = (&) <$> p <*> (res "where" *> (addLets . coalesce . concat <$> braceSep def) <|> pure id)

guards s = maybeWhere $ res s *> expr <|> foldr ($) (V "pjoin#") <$> some ((\x y -> case x of
  V "True" -> \_ -> y
  _ -> A (A (A (V "if") x) y)
  ) <$> (res "|" *> expr) <*> (res s *> expr))

onePat vs x = Pa [(vs, x)]
opDef x f y rhs = [(f, onePat [x, y] rhs)]
leftyPat p expr = case pvars of
  [] -> []
  (h:t) -> let gen = '@':h in
    (gen, expr):map (\v -> (v, Ca (V gen) [(p, V v)])) pvars
  where
  pvars = filter (/= "_") $ patVars p
def = liftA2 (\l r -> [(l, r)]) var (liftA2 onePat (many apat) $ guards "=")
  <|> (pat >>= \x -> opDef x <$> wantVarSym <*> pat <*> guards "=" <|> leftyPat x <$> guards "=")

simpleType c vs = foldl TAp (TC c) (map TV vs)
conop = want f <|> between (res "`") (res "`") (want g) where
  f (ConSym s) = Right s
  f _ = Left ""
  g (ConId s) = Right s
  g _ = Left "want conop"
constr = (\x c y -> Constr c [x, y]) <$> aType <*> conop <*> aType
  <|> Constr <$> wantConId <*> many aType
adt = addAdt <$> between (res "data") (res "=") (simpleType <$> wantConId <*> many wantVarId) <*> sepBy constr (res "|")

impDecl = addImport <$> (res "import" *> wantConId)

topdecls = braceSep
  (   adt
  <|> classDecl
  <|> instDecl
  <|> res "ffi" *> (addFFI <$> wantString <*> var <*> (res "::" *> _type))
  <|> res "export" *> (addExport <$> wantString <*> var)
  <|> addDefs <$> def
  <|> fixity *> pure id
  <|> impDecl
  )

haskell = some $ (,) <$> (res "module" *> wantConId <* res "where" <|> pure "Main") <*> topdecls

offside xs = Ell (landin xs) []
program s = case lex posLexemes $ LexState s (1, 1) of
  Left e -> Left e
  Right (xs, LexState [] _) -> parse haskell $ ParseState (offside xs) $ insert ":" (5, RAssoc) Tip
  Right (_, st) -> Left "unlexable"

-- Primitives.
primAdts =
  [ (TC "()", [Constr "()" []])
  , (TC "Bool", [Constr "True" [], Constr "False" []])
  , (TAp (TC "[]") (TV "a"), [Constr "[]" [], Constr ":" [TV "a", TAp (TC "[]") (TV "a")]])
  , (TAp (TAp (TC ",") (TV "a")) (TV "b"), [Constr "," [TV "a", TV "b"]])
  ]

prims = let
  dyad s = TC s `arr` (TC s `arr` TC s)
  wordy = foldr arr (TAp (TAp (TC ",") (TC "Word")) (TC "Word")) [TC "Word", TC "Word", TC "Word", TC "Word"]
  bin s = A (ro "Q") (ro s)
  in map (second (first noQual)) $
    [ ("intEq", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "EQ"))
    , ("intLE", (arr (TC "Int") (arr (TC "Int") (TC "Bool")), bin "LE"))
    , ("wordLE", (arr (TC "Word") (arr (TC "Word") (TC "Bool")), bin "U_LE"))
    , ("wordEq", (arr (TC "Word") (arr (TC "Word") (TC "Bool")), bin "EQ"))

    , ("charEq", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "EQ"))
    , ("charLE", (arr (TC "Char") (arr (TC "Char") (TC "Bool")), bin "LE"))
    , ("fix", (arr (arr (TV "a") (TV "a")) (TV "a"), ro "Y"))
    , ("if", (arr (TC "Bool") $ arr (TV "a") $ arr (TV "a") (TV "a"), ro "I"))
    , ("wordFromInt", (arr (TC "Int") (TC "Word"), ro "I"))
    , ("chr", (arr (TC "Int") (TC "Char"), ro "I"))
    , ("ord", (arr (TC "Char") (TC "Int"), ro "I"))
    , ("ioBind", (arr (TAp (TC "IO") (TV "a")) (arr (arr (TV "a") (TAp (TC "IO") (TV "b"))) (TAp (TC "IO") (TV "b"))), ro "C"))
    , ("ioPure", (arr (TV "a") (TAp (TC "IO") (TV "a")), A (A (ro "B") (ro "C")) (ro "T")))
    , ("newIORef", (arr (TV "a") (TAp (TC "IO") (TAp (TC "IORef") (TV "a"))),
      A (A (ro "B") (ro "C")) (A (A (ro "B") (ro "T")) (ro "REF"))))
    , ("readIORef", (arr (TAp (TC "IORef") (TV "a")) (TAp (TC "IO") (TV "a")),
      A (ro "T") (ro "READREF")))
    , ("writeIORef", (arr (TAp (TC "IORef") (TV "a")) (arr (TV "a") (TAp (TC "IO") (TC "()"))),
      A (A (ro "R") (ro "WRITEREF")) (ro "B")))
    , ("exitSuccess", (TAp (TC "IO") (TV "a"), ro "END"))
    , ("unsafePerformIO", (arr (TAp (TC "IO") (TV "a")) (TV "a"), A (A (ro "C") (A (ro "T") (ro "END"))) (ro "K")))
    , ("fail#", (TV "a", A (V "unsafePerformIO") (V "exitSuccess")))
    , ("word64Add", (wordy, A (ro "QQ") (ro "DADD")))
    , ("word64Sub", (wordy, A (ro "QQ") (ro "DSUB")))
    , ("word64Mul", (wordy, A (ro "QQ") (ro "DMUL")))
    , ("word64Div", (wordy, A (ro "QQ") (ro "DDIV")))
    , ("word64Mod", (wordy, A (ro "QQ") (ro "DMOD")))
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
    ++ map (\(s, v) -> (s, (dyad "Word", bin v)))
      [ ("wordAdd", "ADD")
      , ("wordSub", "SUB")
      , ("wordMul", "MUL")
      , ("wordDiv", "U_DIV")
      , ("wordMod", "U_MOD")
      , ("wordQuot", "U_DIV")
      , ("wordRem", "U_MOD")
      ]

-- Conversion to De Bruijn indices.
data LC = Ze | Su LC | Pass Extra | PassVar String | La LC | App LC LC

debruijn n e = case e of
  E x -> Pass x
  V v -> maybe (PassVar v) id $
    foldr (\h found -> if h == v then Just Ze else Su <$> found) Nothing n
  A x y -> App (debruijn n x) (debruijn n y)
  L s t -> La (debruijn (s:n) t)

-- Kiselyov bracket abstraction.
data IntTree = Lf Extra | LfVar String | Nd IntTree IntTree
data Sem = Defer | Closed IntTree | Need Sem | Weak Sem

lf = Lf . Basic . comEnum

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
  Pass x -> Closed (Lf x)
  PassVar s -> Closed (LfVar s)
  La t -> case babs t of
    Defer -> Closed (lf "I")
    Closed d -> Closed (Nd (lf "K") d)
    Need e -> e
    Weak e -> Closed (lf "K") ## e
  App x y -> babs x ## babs y

nolam x = (\(Closed d) -> d) $ babs $ debruijn [] x

isLeaf (Lf (Basic n)) c = n == comEnum c
isLeaf _ _ = False

optim t = case t of
  Nd x y -> let p = optim x ; q = optim y in
    if isLeaf p "I" then q else
    if isLeaf q "I" then case p of
      Lf (Basic c)
        | c == comEnum "C" -> lf "T"
        | c == comEnum "B" -> lf "I"
      Nd p1 p2 -> case p1 of
        Lf (Basic c)
          | c == comEnum "B" -> p2
          | c == comEnum "R" -> Nd (lf "T") p2
        _ -> Nd (Nd p1 p2) q
      _ -> Nd p q
    else if isLeaf q "T" then case p of
      Nd x y -> if isLeaf x "B" && isLeaf y "C" then lf "V" else Nd p q
      _ -> Nd p q
    else Nd p q
  _ -> t

freeCount v expr = case expr of
  E _ -> 0
  V s -> if s == v then 1 else 0
  A x y -> freeCount v x + freeCount v y
  L w t -> if v == w then 0 else freeCount v t
app01 s x = case freeCount s x of
  0 -> const x
  1 -> flip (beta s) x
  _ -> A $ L s x
optiApp t = case t of
  A (L s x) y -> app01 s (optiApp x) (optiApp y)
  A x y -> A (optiApp x) (optiApp y)
  L s x -> L s (optiApp x)
  _ -> t

-- Pattern compiler.
singleOut s cs = \scrutinee x ->
  foldl A (A (V $ specialCase cs) scrutinee) $ map (\(Constr s' ts) ->
    if s == s' then x else foldr L (V "pjoin#") $ map (const "_") ts) cs

patEq lit b x y = A (A (A (V "if") (A (A (V "==") (E lit)) b)) x) y

unpat dcs as t = case as of
  [] -> pure t
  a:at -> get >>= \n -> put (n + 1) >> let freshv = showInt n "#" in L freshv <$> let
    go p x = case p of
      PatLit lit -> unpat dcs at $ patEq lit (V freshv) x $ V "pjoin#"
      PatVar s m -> maybe (unpat dcs at) (\p1 x1 -> go p1 x1) m $ beta s (V freshv) x
      PatCon con args -> case dcs con of
        Nothing -> error "bad data constructor"
        Just cons -> unpat dcs args x >>= \y -> unpat dcs at $ singleOut con cons (V freshv) y
    in go a t

unpatTop dcs als x = case als of
  [] -> pure x
  (a, l):alt -> let
    go p t = case p of
      PatLit lit -> unpatTop dcs alt $ patEq lit (V l) t $ V "pjoin#"
      PatVar s m -> maybe (unpatTop dcs alt) go m $ beta s (V l) t
      PatCon con args -> case dcs con of
        Nothing -> error "bad data constructor"
        Just cons -> unpat dcs args t >>= \y -> unpatTop dcs alt $ singleOut con cons (V l) y
    in go a x

rewritePats' dcs asxs ls = case asxs of
  [] -> pure $ V "fail#"
  (as, t):asxt -> unpatTop dcs (zip as ls) t >>=
    \y -> A (L "pjoin#" y) <$> rewritePats' dcs asxt ls

rewritePats dcs vsxs@((vs0, _):_) = get >>= \n -> let
  ls = map (flip showInt "#") $ take (length vs0) [n..]
  in put (n + length ls) >> flip (foldr L) ls <$> rewritePats' dcs vsxs ls

classifyAlt v x = case v of
  PatLit lit -> Left $ patEq lit (V "of") x
  PatVar s m -> maybe (Left . A . L "pjoin#") classifyAlt m $ A (L s x) $ V "of"
  PatCon s ps -> Right (insertWith (flip (.)) s ((ps, x):))

genCase dcs tab = if size tab == 0 then id else A . L "cjoin#" $ let
  firstC = flst (toAscList tab) undefined (\h _ -> fst h)
  cs = maybe (error $ "bad constructor: " ++ firstC) id $ dcs firstC
  in foldl A (A (V $ specialCase cs) (V "of"))
    $ map (\(Constr s ts) -> case mlookup s tab of
      Nothing -> foldr L (V "cjoin#") $ const "_" <$> ts
      Just f -> Pa $ f [(const (PatVar "_" Nothing) <$> ts, V "cjoin#")]
    ) cs

updateCaseSt dcs (acc, tab) alt = case alt of
  Left f -> (acc . genCase dcs tab . f, Tip)
  Right upd -> (acc, upd tab)

rewriteCase dcs as = fpair (foldl (updateCaseSt dcs) (id, Tip)
  $ uncurry classifyAlt <$> as) \acc tab -> acc . genCase dcs tab $ V "fail#"

secondM f (a, b) = (a,) <$> f b
patternCompile dcs t = optiApp $ evalState (go t) 0 where
  go t = case t of
    E _ -> pure t
    V _ -> pure t
    A x y -> liftA2 A (go x) (go y)
    L s x -> L s <$> go x
    Pa vsxs -> mapM (secondM go) vsxs >>= rewritePats dcs
    Ca x as -> liftA2 A (L "of" . rewriteCase dcs <$> mapM (secondM go) as >>= go) (go x)

-- Type inference.
instantiate' t n tab = case t of
  TC s -> ((t, n), tab)
  TV s -> case lookup s tab of
    Nothing -> let va = TV (showInt n "") in ((va, n + 1), (s, va):tab)
    Just v -> ((v, n), tab)
  TAp x y ->
    fpair (instantiate' x n tab) \(t1, n1) tab1 ->
    fpair (instantiate' y n1 tab1) \(t2, n2) tab2 ->
    ((TAp t1 t2, n2), tab2)

instantiatePred (Pred s t) ((out, n), tab) = first (first ((:out) . Pred s)) (instantiate' t n tab)

instantiate (Qual ps t) n =
  fpair (foldr instantiatePred (([], n), []) ps) \(ps1, n1) tab ->
  first (Qual ps1) (fst (instantiate' t n1 tab))

proofApply sub a = case a of
  Proof (Pred cl ty) -> Proof (Pred cl $ apply sub ty)
  A x y -> A (proofApply sub x) (proofApply sub y)
  L s t -> L s $ proofApply sub t
  _ -> a

typeAstSub sub (t, a) = (apply sub t, proofApply sub a)

infer typed loc ast csn = fpair csn \cs n ->
  let
    va = TV (showInt n "")
    insta ty = fpair (instantiate ty n) \(Qual preds ty) n1 -> ((ty, foldl A ast (map Proof preds)), (cs, n1))
  in case ast of
    E x -> Right $ case x of
      Const _ -> ((TC "Int", ast), csn)
      ChrCon _ -> ((TC "Char", ast), csn)
      StrCon _ -> ((TAp (TC "[]") (TC "Char"), ast), csn)
      Link im s q -> insta q
    V s -> maybe (Left $ "undefined: " ++ s) Right
      $ (\t -> ((t, ast), csn)) <$> lookup s loc
      <|> insta <$> mlookup s typed
    A x y -> infer typed loc x (cs, n + 1) >>=
      \((tx, ax), csn1) -> infer typed loc y csn1 >>=
      \((ty, ay), (cs2, n2)) -> unify tx (arr ty va) cs2 >>=
      \cs -> Right ((va, A ax ay), (cs, n2))
    L s x -> first (\(t, a) -> (arr va t, L s a)) <$> infer typed ((s, va):loc) x (cs, n + 1)

findInstance tycl qn@(q, n) p@(Pred cl ty) insts = case insts of
  [] -> let v = '*':showInt n "" in Right (((p, v):q, n + 1), V v)
  (modName, Instance h name ps _):rest -> case match h ty of
    Nothing -> findInstance tycl qn p rest
    Just subs -> foldM (\(qn1, t) (Pred cl1 ty1) -> second (A t)
      <$> findProof tycl (Pred cl1 $ apply subs ty1) qn1) (qn, if modName == "" then V name else E $ Link modName name undefined) ps

findProof tycl pred@(Pred classId t) psn@(ps, n) = case lookup pred ps of
  Nothing -> case tycl classId of
    [] -> Left $ "no instance: " ++ showPred pred ""
    insts -> findInstance tycl psn pred insts
  Just s -> Right (psn, V s)

prove' tycl psn a = case a of
  Proof pred -> findProof tycl pred psn
  A x y -> prove' tycl psn x >>= \(psn1, x1) ->
    second (A x1) <$> prove' tycl psn1 y
  L s t -> second (L s) <$> prove' tycl psn t
  _ -> Right (psn, a)

data Dep a = Dep ([String] -> Either String ([String], a))
instance Functor Dep where
  fmap f = \(Dep mf) -> Dep \g -> do
    (g', x) <- mf g
    pure (g', f x)
instance Applicative Dep where
  pure x = Dep \g -> Right (g, x)
  (Dep mf) <*> (Dep mx) = Dep \g -> do
    (g', f) <- mf g
    (g'', x) <- mx g'
    pure (g'', f x)
addDep s = Dep \deps -> Right (if s `elem` deps then deps else s : deps, ())
badDep s = Dep $ const $ Left s
runDep (Dep f) = f []

astLink typed locals imps mods ast = runDep $ go [] ast where
  go bound ast = case ast of
    V s
      | elem s bound -> pure ast
      | elem s $ fst <$> typedAsts neatNew -> pure ast
      | member s locals -> case findImportSym imps mods s of
        [] -> (if member s typed then pure () else addDep s) *> pure ast
        _ -> badDep $ "ambiguous: " ++ s
      | True -> case findImportSym imps mods s of
        [] -> badDep $ "missing: " ++ s
        [(im, t)] -> pure $ E $ Link im s t
        _ -> badDep $ "ambiguous: " ++ s
    A x y -> A <$> go bound x <*> go bound y
    L s t -> L s <$> go (s:bound) t
    _ -> pure ast

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
findImportSym imps mods s = concat [maybe [] (\t -> [(im, t)]) $ mlookup s qs | im <- imps, let qs = fst $ fst $ mods ! im]

inferDefs tycl defs typed = do
  let
    insertUnique m (s, (_, t)) = case mlookup s m of
      Nothing -> case mlookup s typed of
        Nothing -> Right $ insert s t m
        _ -> Left $ "reserved: " ++ s
      _ -> Left $ "duplicate: " ++ s
    addEdges (sym, (deps, _)) (ins, outs) = (foldr (\dep es -> insertWith union dep [sym] es) ins deps, insertWith union sym deps outs)
    graph = foldr addEdges (Tip, Tip) defs
  defmap <- foldM insertUnique Tip defs
  let
    ins k = maybe [] id $ mlookup k $ fst graph
    outs k = maybe [] id $ mlookup k $ snd graph
    typeTab = fst <$> typed
    lambs = second snd <$> toAscList typed
  foldM (inferDefs' tycl defmap) (typeTab, (lambs++)) $ scc ins outs $ keys defmap

dictVars ps n = (zip ps $ map (('*':) . flip showInt "") [n..], n + length ps)

inferTypeclasses tycl typed dcs linker ienv = concat <$> mapM perClass (toAscList ienv) where
  perClass (classId, Tycl sigs insts) = do
    let
      checkDefault (s, Just rawExpr) = do
        expr <- snd <$> linker (patternCompile dcs rawExpr)
        (ta, (sub, _)) <- either (Left . (s++) . (" (class): "++)) Right
          $ infer typed [] expr ([], 0)
        (_, (Qual ps t, a)) <- prove tycl s $ typeAstSub sub ta
        case ps of
          [Pred cl _] | cl == classId -> Right ()
          _ -> Left $ "bad method: " ++ s
        Qual ps0 t0 <- maybe (Left "parse bug!") Right $ mlookup s typed
        case match t t0 of
          Nothing -> Left $ "bad method type: " ++ s
          _ -> Right ()
      checkDefault (s, Nothing) = pure ()
    mapM_ checkDefault sigs
    let
      perInstance (Instance ty name ps idefs) = do
        let
          dvs = map snd $ fst $ dictVars ps 0
          perMethod (s, mayDefault) = do
            let Just rawExpr = mlookup s idefs <|> mayDefault <|> pure (V "fail#")
            expr <- snd <$> linker (patternCompile dcs rawExpr)
            (ta, (sub, n)) <- either (Left . (name++) . (" "++) . (s++) . (": "++)) Right
              $ infer typed [] expr ([], 0)
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
                  then Left $ ("want context: "++) . (foldr (.) id $ showPred . fst <$> ps3) $ name
                  else pure tr
        ms <- mapM perMethod sigs
        pure (name, flip (foldr L) dvs $ L "@" $ foldl A (V "@") ms)
    mapM perInstance insts

neatNew = foldr (uncurry addAdt) (Neat Tip [] prims Tip [] [] []) primAdts

tabulateModules mods = foldM ins Tip $ go <$> mods where
  go (name, prog) = (name, foldr ($) neatNew prog)
  ins tab (k, v) = case mlookup k tab of
    Nothing -> Right $ insert k v tab
    Just _ -> Left $ "duplicate module: " ++ k

inferModule tab acc name = case mlookup name acc of
  Nothing -> do
    let
      Neat ienv rawDefs typed adtTab ffis ffes imps = tab ! name
      defs = coalesce rawDefs
      locals = fromList $ map (, ()) $ (fst <$> typed) ++ (fst <$> defs)
      insts im (Tycl _ is) = (im,) <$> is
      classes im = if im == "" then ienv else typeclasses $ tab ! im
      tycl classId = concat [maybe [] (insts im) $ mlookup classId $ classes im | im <- "":imps]
      dcs s = foldr (<|>) (mlookup s adtTab) $ map (\im -> mlookup s $ dataCons $ tab ! im) imps
    acc' <- foldM (inferModule tab) acc imps
    let linker = astLink (fromList typed) locals imps acc'
    depdefs <- mapM (\(s, t) -> (s,) <$> linker (patternCompile dcs t)) defs
    (qs, lambF) <- inferDefs tycl depdefs (fromList typed)
    mets <- inferTypeclasses tycl qs dcs linker ienv
    Right $ insert name ((qs, lambF mets), (ffis, ffes)) acc'
  Just _ -> Right acc

untangle s = case program s of
  Left e -> Left $ "parse error: " ++ e
  Right (mods, ParseState s _) -> case s of
    Ell [] [] -> do
      tab <- tabulateModules mods
      foldM (inferModule tab) Tip $ keys tab
    _ -> Left $ "parse error: " ++ case ell s of
      Left e -> e
      Right (((r, c), _), _) -> ("row "++) . showInt r . (" col "++) . showInt c $ ""

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

showVar s@(h:_) = showParen (elem h ":!#$%&*+./<=>?@\\^|-~") (s++)

showExtra = \case
  Basic i -> (comName i++)
  Const i -> showInt i
  ChrCon c -> ('\'':) . (c:) . ('\'':)
  StrCon s -> ('"':) . (s++) . ('"':)
  Link im s _ -> (im++) . ('.':) . (s++)

showPat = \case
  PatLit e -> showExtra e
  PatVar s mp -> (s++) . maybe id ((('@':) .) . showPat) mp
  PatCon s ps -> (s++) . ("TODO"++)

showAst prec t = case t of
  E e -> showExtra e
  V s -> showVar s
  A (E (Basic f)) (E (Basic c)) | f == comEnum "F" -> ("FFI_"++) . showInt c
  A x y -> showParen prec $ showAst False x . (' ':) . showAst True y
  L s t -> par $ ('\\':) . (s++) . (" -> "++) . showAst prec t
  Pa vsts -> ('\\':) . par (foldr (.) id $ intersperse (';':) $ map (\(vs, t) -> foldr (.) id (intersperse (' ':) $ map (par . showPat) vs) . (" -> "++) . showAst False t) vsts)
  Ca x as -> ("case "++) . showAst False x . ("of {"++) . foldr (.) id (intersperse (',':) $ map (\(p, a) -> showPat p . (" -> "++) . showAst False a) as)
  Proof p -> ("{Proof "++) . showPred p . ("}"++)

showTree prec t = case t of
  LfVar s -> showVar s
  Lf extra -> showExtra extra
  Nd (Lf (Basic f)) (Lf (Basic c)) | f == comEnum "F" -> ("FFI_"++) . showInt c
  Nd x y -> showParen prec $ showTree False x . (' ':) . showTree True y
disasm (s, t) = (s++) . (" = "++) . showTree False t . (";\n"++)

dumpWith dumper s = case untangle s of
  Left err -> err
  Right tab -> foldr ($) [] $ map (\(name, mod) -> ("module "++) . (name++) . ('\n':) . (foldr (.) id $ dumper mod)) $ toAscList tab

dumpCombs ((_, lambs), _) = map disasm $ optiComb lambs

dumpLambs ((_, lambs), _) = map (\(s, t) -> (s++) . (" = "++) . showAst False t . ('\n':)) lambs

showQual (Qual ps t) = foldr (.) id (map showPred ps) . showType t

dumpTypes ((typed, _), _) = map (\(s, q) -> (s++) . (" :: "++) . showQual q . ('\n':)) $ toAscList typed

-- Hash consing.
data Obj = Local String | Global String String | Code Int

instance Eq Obj where
  Local a == Local b = a == b
  Global m a == Global n b = m == n && a == b
  Code a == Code b = a == b
  _ == _ = False

instance Ord Obj where
  x <= y = case x of
    Local a -> case y of
      Local b -> a <= b
      _ -> True
    Global m a -> case y of
      Local _ -> False
      Global n b -> if m == n then a <= b else m <= n
      _ -> True
    Code a -> case y of
      Code b -> a <= b
      _ -> False

instance (Eq a, Eq b) => Eq (a, b) where
  (a1, b1) == (a2, b2) = a1 == a2 && b1 == b2
instance (Ord a, Ord b) => Ord (a, b) where
  (a1, b1) <= (a2, b2) = a1 <= a2 && (not (a2 <= a1) || b1 <= b2)

memget k@(a, b) = get >>= \(tab, (hp, f)) -> case mlookup k tab of
  Nothing -> put (insert k hp tab, (hp + 2, f . (a:) . (b:))) >> pure hp
  Just v -> pure v

enc t = case t of
  Lf n -> case n of
    Basic c -> pure $ Code c
    Const c -> Code <$> memget (Code $ comEnum "NUM", Code c)
    ChrCon c -> enc $ Lf $ Const $ ord c
    StrCon s -> enc $ foldr (\h t -> Nd (Nd (lf "CONS") (Lf $ ChrCon h)) t) (lf "K") s
    Link m s _ -> pure $ Global m s
  LfVar s -> pure $ Local s
  Nd x y -> enc x >>= \hx -> enc y >>= \hy -> Code <$> memget (hx, hy)

encTop t = enc t >>= \case
  Code n -> pure n
  other -> memget (Code $ comEnum "I", other)

asm combs = foldM
  (\symtab (s, t) -> (flip (insert s) symtab) <$> encTop t)
  Tip combs

hashcons hp combs = fpair (runState (asm combs) (Tip, (hp, id)))
  \symtab (_, (hp, f)) -> let
    mem = (\case
        Code n -> Right n
        Local s -> Right $ symtab ! s
        Global m s -> Left (m, s)
      ) <$> f []
    in (symtab, (hp, mem))

-- Code generation.
codegenLocal (name, ((_, lambs), _)) (bigmap, (hp, f)) =
  (insert name localmap bigmap, (hp', f . (mem'++)))
  where
  (localmap, (hp', mem')) = hashcons hp $ optiComb lambs

ffcat (name, (_, (ffis, ffes))) (xs, ys) = (ffis ++ xs, ((name,) <$> ffes) ++ ys)

compile s = either id id do
  mods <- untangle s
  let
    (bigmap, (_, memF)) = foldr codegenLocal (Tip, (128, id)) $ toAscList mods
    (ffis, ffes) = foldr ffcat ([], []) $ toAscList mods
    mem = either (\(m, s) -> (bigmap ! m) ! s ) id <$> memF []
    getIOType (Qual [] (TAp (TC "IO") t)) = Right t
    getIOType q = Left $ "main : " ++ showQual q ""
    mustType modName s = case mlookup s $ fst $ fst $ mods ! modName of
      Just (Qual [] t) -> t
      _ -> error "TODO: report bad exports"
    mayMain = do
        tab <- mlookup "Main" bigmap
        mainAddr <- mlookup "main" tab
        mainType <- mlookup "main" $ fst $ fst $ mods ! "Main"
        pure (mainAddr, mainType)
  mainStr <- case mayMain of
    Nothing -> pure ""
    Just (a, q) -> do
      getIOType q
      pure $ genMain a

  pure
    $ ("#include<stdio.h>\n"++)
    . ("typedef unsigned u;\n"++)
    . ("enum{_UNDEFINED=0,"++)
    . foldr (.) id (map (\(s, _) -> ('_':) . (s++) . (',':)) comdefs)
    . ("};\n"++)
    . ("static const u prog[]={" ++)
    . foldr (.) id (map (\n -> showInt n . (',':)) mem)
    . ("};\nstatic const u prog_size="++) . showInt (length mem) . (";\n"++)
    . ("static u root[]={" ++)
    . foldr (\(x, (modName, funName)) f -> maybe undefined showInt (mlookup funName $ bigmap ! modName) . (", " ++) . f) id ffes
    . ("0};\n" ++)
    . (libc++)
    . (preamble++)
    . (concatMap ffiDeclare ffis ++)
    . ("static void foreign(u n) {\n  switch(n) {\n" ++)
    . ffiDefine (length ffis - 1) ffis
    . ("\n  }\n}\n" ++)
    . runFun
    . foldr (.) id (zipWith (\(modName, (expName, ourName))  n -> ("EXPORT(f"++) . showInt n . (", \""++) . (expName++) . ("\")\n"++)
      . genExport (arrCount $ mustType modName ourName) n) ffes [0..])
    $ mainStr

comb = (,) <$> wantConId <*> ((,) <$> many wantVarId <*> (res "=" *> combExpr))
combExpr = foldl1 A <$> some
  (V <$> wantVarId <|> E . StrCon <$> wantString <|> paren combExpr)
comdefs = case lex posLexemes $ LexState comdefsrc (1, 1) of
  Left e -> error e
  Right (xs, _) -> case parse (braceSep comb) $ ParseState (offside xs) Tip of
    Left e -> error e
    Right (cs, _) -> cs
comEnum s = maybe (error s) id $ lookup s $ zip (fst <$> comdefs) [1..]
comName i = maybe undefined id $ lookup i $ zip [1..] (fst <$> comdefs)

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
  static u ready;if (!ready){ready=1;rts_init();}
  *(sp = spTop) = app(app(n, _UNDEFINED), _END);
  run();
}
|]++)

main = getArgs >>= \case
  "comb":_ -> interact $ dumpWith dumpCombs
  "lamb":_ -> interact $ dumpWith dumpLambs
  "type":_ -> interact $ dumpWith dumpTypes
  _ -> interact compile
  where
  getArg' k n = getArgChar n k >>= \c -> if ord c == 0 then pure [] else (c:) <$> getArg' (k + 1) n
  getArgs = getArgCount >>= \n -> mapM (getArg' 0) [1..n-1]
