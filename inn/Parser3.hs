-- Separate fixity phase.
-- Export lists.
module Parser where
import Base
import Ast
import Map

-- Lexer.
data LexState = LexState String (Int, Int)
data Lexer a = Lexer (LexState -> Either String (a, LexState))
instance Functor Lexer where fmap f (Lexer x) = Lexer $ fmap (first f) . x
instance Applicative Lexer where
  pure x = Lexer \inp -> Right (x, inp)
  f <*> x = Lexer \inp -> case lexer f inp of
    Left e -> Left e
    Right (fun, t) -> case lexer x t of
      Left e -> Left e
      Right (arg, u) -> Right (fun arg, u)
instance Monad Lexer where
  return = pure
  x >>= f = Lexer \inp -> case lexer x inp of
    Left e -> Left e
    Right (a, t) -> lexer (f a) t
instance Alternative Lexer where
  empty = Lexer \_ -> Left ""
  (<|>) x y = Lexer \inp -> either (const $ lexer y inp) Right $ lexer x inp

lexer (Lexer f) inp = f inp
advanceRC x (r, c)
  | n `elem` [10, 11, 12, 13] = (r + 1, 1)
  | n == 9 = (r, (c + 8)`mod`8)
  | True = (r, c + 1)
  where n = ord x
pos = Lexer \inp@(LexState _ rc) -> Right (rc, inp)
sat f = Lexer \(LexState inp rc) -> case inp of
  [] -> Left "EOF"
  h:t -> if f h then Right (h, LexState t $ advanceRC h rc) else Left "unsat"
char c = sat (c ==)

data Token = Reserved String
  | VarId String | VarSym String | ConId String | ConSym String
  | Lit Extra

hexValue d
  | d <= '9' = ord d - ord '0'
  | d <= 'F' = 10 + ord d - ord 'A'
  | d <= 'f' = 10 + ord d - ord 'a'
isSpace c = elem (ord c) [32, 9, 10, 11, 12, 13, 160]
isNewline c = ord c `elem` [10, 11, 12, 13]
isSymbol = (`elem` "!#$%&*+./<=>?@\\^|-~:")
dashes = char '-' *> some (char '-')
comment = dashes *> (sat isNewline <|> sat (not . isSymbol) *> many (sat $ not . isNewline) *> sat isNewline)
small = sat \x -> ((x <= 'z') && ('a' <= x)) || (x == '_')
large = sat \x -> (x <= 'Z') && ('A' <= x)
hexit = sat \x -> (x <= '9') && ('0' <= x)
  || (x <= 'F') && ('A' <= x)
  || (x <= 'f') && ('a' <= x)
digit = sat \x -> (x <= '9') && ('0' <= x)
decimal = foldl (\n d -> 10*n + ord d - ord '0') 0 <$> some digit
hexadecimal = foldl (\n d -> 16*n + hexValue d) 0 <$> some hexit

escape = char '\\' *> (sat (`elem` "'\"\\") <|> char 'n' *> pure '\n' <|> char '0' *> pure '\0' <|> char 'x' *> (chr <$> hexadecimal))
tokOne delim = escape <|> sat (delim /=)

tokChar = between (char '\'') (char '\'') (tokOne '\'')
tokStr = between (char '"') (char '"') $ many $ many (char '\\' *> char '&') *> tokOne '"'
integer = char '0' *> (char 'x' <|> char 'X') *> hexadecimal <|> decimal
literal = Lit . Const <$> integer <|> Lit . ChrCon <$> tokChar <|> Lit . StrCon <$> tokStr
varId = fmap ck $ liftA2 (:) small $ many (small <|> large <|> digit <|> char '\'') where
  ck s = (if elem s
    ["export", "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"]
    then Reserved else VarId) s
varSym = fmap ck $ (:) <$> sat (\c -> isSymbol c && c /= ':') <*> many (sat isSymbol) where
  ck s = (if elem s ["..", "=", "\\", "|", "<-", "->", "@", "~", "=>"] then Reserved else VarSym) s

conId = fmap ConId $ liftA2 (:) large $ many (small <|> large <|> digit <|> char '\'')
conSym = fmap ck $ liftA2 (:) (char ':') $ many $ sat isSymbol where
  ck s = (if elem s [":", "::"] then Reserved else ConSym) s
special = Reserved . (:"") <$> asum (char <$> "(),;[]`{}")

rawBody = (char '|' *> char ']' *> pure []) <|> (:) <$> sat (const True) <*> rawBody
rawQQ = char '[' *> char 'r' *> char '|' *> (Lit . StrCon <$> rawBody)
lexeme = rawQQ <|> varId <|> varSym <|> conId <|> conSym
  <|> special <|> literal

whitespace = many (sat isSpace <|> comment)
lexemes = whitespace *> many (lexeme <* whitespace)

getPos = Lexer \st@(LexState _ rc) -> Right (rc, st)
posLexemes = whitespace *> many (liftA2 (,) getPos lexeme <* whitespace)

-- Layout.
data Landin = Curly Int | Angle Int | PL ((Int, Int), Token)
beginLayout xs = case xs of
  [] -> [Curly 0]
  ((r', _), Reserved "{"):_ -> margin r' xs
  ((r', c'), _):_ -> Curly c' : margin r' xs

landin ls@((_, Reserved "module"):_) = embrace ls
landin ls@(((r, _), Reserved "{"):_) = margin r ls
landin ls@(((r, c), _):_) = Curly c : margin r ls
landin [] = []

margin r ls@(((r', c), _):_) | r /= r' = Angle c : embrace ls
margin r ls = embrace ls

embrace ls@(x@(_, Reserved w):rest) | elem w ["let", "where", "do", "of"] =
  PL x : beginLayout rest
embrace ls@(x@(_, Reserved "\\"):y@(_, Reserved "case"):rest) =
  PL x : PL y : beginLayout rest
embrace (x@((r,_),_):xt) = PL x : margin r xt
embrace [] = []

data Ell = Ell [Landin] [Int]
insPos x ts ms = Right (x, Ell ts ms)
ins w = insPos ((0, 0), Reserved w)

ell (Ell toks cols) = case toks of
  t:ts -> case t of
    Angle n -> case cols of
      m:ms | m == n -> ins ";" ts (m:ms)
           | n + 1 <= m -> ins "}" (Angle n:ts) ms
      _ -> ell $ Ell ts cols
    Curly n -> case cols of
      m:ms | m + 1 <= n -> ins "{" ts (n:m:ms)
      [] | 1 <= n -> ins "{" ts [n]
      _ -> ell $ Ell (PL ((0,0),Reserved "{"): PL ((0,0),Reserved "}"):Angle n:ts) cols
    PL x -> case snd x of
      Reserved "}" -> case cols of
        0:ms -> ins "}" ts ms
        _ -> Left "unmatched }"
      Reserved "{" -> insPos x ts (0:cols)
      _ -> insPos x ts cols
  [] -> case cols of
    [] -> Left "EOF"
    m:ms | m /= 0 -> ins "}" [] ms
    _ -> Left "missing }"

parseErrorRule (Ell toks cols) = case cols of
  m:ms | m /= 0 -> Right $ Ell toks ms
  _ -> Left "missing }"

-- Parser.
data Parser a = Parser (Ell -> Either String (a, Ell))
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

ro = E . Basic
conOf (Constr s _) = s
specialCase (h:_) = '{':conOf h
mkCase t cs = insert (specialCase cs)
  ( Qual [] $ arr t $ foldr arr (TV "case") $ map (\(Constr _ sts) -> foldr arr (TV "case") $ snd <$> sts) cs
  , ro "I")
mkStrs = snd . foldl (\(s, l) u -> ('@':s, s:l)) ("@", [])
scottEncode _ ":" _ = ro "CONS"
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs)
scottConstr t cs (Constr s sts) = foldr (.) (insertWith (error $ "constructor conflict: " ++ s) s
  (Qual [] $ foldr arr t ts , scottEncode (map conOf cs) s $ mkStrs ts)
  ) [insertWith (error $ "field conflict: " ++ field) field (Qual [] $ t `arr` ft, L s $ foldl A (V s) $ inj $ proj field) | (field, ft) <- sts, field /= ""]
  where
  ts = snd <$> sts
  proj fd = foldr L (V fd) $ fst <$> sts
  inj x = map (\(Constr s' _) -> if s' == s then x else V "undefined") cs
mkAdtDefs t cs = foldr (.) (mkCase t cs) $ scottConstr t cs <$> cs

mkFFIHelper n t acc = case t of
  TC s -> acc
  TAp (TC "IO") _ -> acc
  TAp (TAp (TC "->") x) y -> L (show n) $ mkFFIHelper (n + 1) y $ A (V $ show n) acc

updateDcs cs dcs = foldr (\(Constr s _) m -> insert s cs m) dcs cs
addAdt t cs ders neat = foldr derive neat' ders where
  neat' = neat
    { typedAsts = mkAdtDefs t cs $ typedAsts neat
    , dataCons = updateDcs cs $ dataCons neat
    , type2Cons = insert (typeName t) (concatMap cnames cs) $ type2Cons neat
    }
  typeName = \case
    TAp x _ -> typeName x
    TC c -> c
  cnames (Constr s sts) = s : concatMap (\(s, _) -> if s == "" then [] else [s]) sts
  derive "Eq" = addInstance "Eq" (mkPreds "Eq") t
    [("==", L "lhs" $ L "rhs" $ Ca (V "lhs") $ map eqCase cs
    )]
  derive "Show" = addInstance "Show" (mkPreds "Show") t
    [("showsPrec", L "prec" $ L "x" $ Ca (V "x") $ map showCase cs
    )]
  derive der = error $ "bad deriving: " ++ der
  showCase (Constr con args) = let as = show <$> [1..length args]
    in (PatCon con (mkPatVar "" <$> as), case args of
      [] -> L "s" $ A (A (V "++") (E $ StrCon con)) (V "s")
      _ -> case con of
        ':':_ -> A (A (V "showParen") $ V "True") $ foldr1
          (\f g -> A (A (V ".") f) g)
          [ A (A (V "showsPrec") (E $ Const 11)) (V "1")
          , L "s" $ A (A (V "++") (E $ StrCon $ ' ':con++" ")) (V "s")
          , A (A (V "showsPrec") (E $ Const 11)) (V "2")
          ]
        _ -> A (A (V "showParen") $ A (A (V "<=") (E $ Const 11)) $ V "prec")
          $ A (A (V ".") $ A (V "++") (E $ StrCon con))
          $ foldr (\f g -> A (A (V ".") f) g) (L "x" $ V "x")
          $ map (\a -> A (A (V ".") (A (V ":") (E $ ChrCon ' '))) $ A (A (V "showsPrec") (E $ Const 11)) (V a)) as
      )
  mkPreds classId = Pred classId . TV <$> typeVars t
  mkPatVar pre s = PatVar (pre ++ s) Nothing
  eqCase (Constr con args) = let as = show <$> [1..length args]
    in (PatCon con (mkPatVar "l" <$> as), Ca (V "rhs")
      [ (PatCon con (mkPatVar "r" <$> as), foldr (\x y -> (A (A (V "&&") x) y)) (V "True")
         $ map (\n -> A (A (V "==") (V $ "l" ++ n)) (V $ "r" ++ n)) as)
      , (PatVar "_" Nothing, V "False")])

emptyTycl = Tycl [] []
addClass classId v (sigs, defs) neat = if null ms then neat
  { typeclasses = insert classId (Tycl (keys sigs) is) tycl
  , typedAsts = selectors $ typedAsts neat
  , topDefs = defaults ++ topDefs neat
  } else error $ "duplicate class: " ++ classId
  where
  vars = take (size sigs) $ show <$> [0..]
  selectors = foldr (.) id $ zipWith (\var (s, Qual ps t) -> insertWith (error $ "method conflict: " ++ s) s (Qual (Pred classId v:ps) t,
    L "@" $ A (V "@") $ foldr L (V var) vars)) vars $ toAscList sigs
  defaults = map (\(s, t) -> if member s sigs then ("{default}" ++ s, t) else error $ "bad default method: " ++ s) $ toAscList defs
  tycl = typeclasses neat
  Tycl ms is = maybe emptyTycl id $ mlookup classId tycl

addInstance classId ps ty ds neat = neat
  { typeclasses = insert classId (Tycl ms $ Instance ty name ps (fromList ds):is) tycl
  } where
  tycl = typeclasses neat
  Tycl ms is = maybe emptyTycl id $ mlookup classId tycl
  name = '{':classId ++ (' ':shows ty "}")

addTopDecl (s, t) neat = neat { topDecls = insert s t $ topDecls neat }

addForeignImport foreignname ourname t neat = neat
  { typedAsts = insertWith (error $ "import conflict: " ++ ourname) ourname (Qual [] t, mkFFIHelper 0 t $ A (E $ Basic "F") $ E $ Link "{foreign}" foreignname $ Qual [] t) $ typedAsts neat
  , ffiImports = insertWith (error $ "duplicate import: " ++ foreignname) foreignname t $ ffiImports neat
  }
addForeignExport e f neat = neat { ffiExports = insertWith (error $ "duplicate export: " ++ e) e f $ ffiExports neat }
addDefs ds neat = neat { topDefs = ds ++ topDefs neat }
addImport im neat = neat { moduleImports = im:moduleImports neat }
addFixities os prec neat = neat { opFixity = foldr (\o tab -> insert o prec tab) (opFixity neat) os }

want f = Parser \inp -> case ell inp of
  Right ((_, x), inp') -> (, inp') <$> f x
  Left e -> Left e

braceYourself = Parser \inp -> case ell inp of
  Right ((_, Reserved "}"), inp') -> Right ((), inp')
  _ -> ((), ) <$> parseErrorRule inp

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
    insLams vs t = foldr L t vs
    appem vs = foldl1 A $ V <$> vs
    suball expr = foldl A (foldr L expr $ init names) $ appem <$> init tnames
    redef tns expr = foldr L (suball expr) tns
    in foldr (\(x:xt) t -> A (L x t) $ maybeFix x $ redef xt $ maybe undefined id $ lookup x ls) (suball expr) tnames

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
  pure $ addFixities os (n, a)

fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc

cDecls = first fromList . second fromList . foldr ($) ([], []) <$> braceSep cDecl
cDecl = first . (:) <$> genDecl <|> second . (++) <$> defSemi

genDecl = (,) <$> var <* res "::" <*> (Qual <$> (scontext <* res "=>" <|> pure []) <*> _type)

classDecl = res "class" *> (addClass <$> wantConId <*> (TV <$> wantVarId) <*> (res "where" *> cDecls))

simpleClass = Pred <$> wantConId <*> _type
scontext = (:[]) <$> simpleClass <|> paren (sepBy simpleClass $ res ",")

instDecl = res "instance" *>
  ((\ps cl ty defs -> addInstance cl ps ty defs) <$>
  (scontext <* res "=>" <|> pure [])
    <*> wantConId <*> _type <*> (res "where" *> braceDef))

letin = addLets <$> between (res "let") (res "in") braceDef <*> expr
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
  <|> (\ds -> Just . addLets ds . maybePureUnit) <$> (res "let" *> braceDef)
doblock = res "do" *> (maybePureUnit . foldr ($) Nothing <$> braceSep stmt)

compQual =
  (\p xs e -> A (A (V "concatMap") $ onePat [p] e) xs)
    <$> pat <*> (res "<-" *> expr)
  <|> (\b e -> A (A (A (V "if") b) e) $ V "[]") <$> expr
  <|> addLets <$> (res "let" *> braceDef)

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

fbind = A <$> (E . StrCon <$> var) <*> (res "=" *> expr)

mayUpdate v = (do
    fbs <- between (res "{") (res "}") $ sepBy1 fbind (res ",")
    pure $ A (E $ Basic "{=") $ foldr A (E $ Basic "=}") $ v:fbs
  ) <|> pure v

atom = ifthenelse <|> doblock <|> letin <|> sqExpr <|> section
  <|> cas <|> lam <|> (paren (res ",") *> pure (V ","))
  <|> ((V <$> (con <|> var)) >>= mayUpdate) <|> E <$> wantLit

aexp = foldl1 A <$> some atom

chain a = \case
  [] -> a
  A f b:rest -> case rest of
    [] -> A (A f a) b
    _ -> A (E $ Basic "{+") $ A (A (A f a) b) $ foldr A (E $ Basic "+}") rest
  _ -> error "unreachable"
expr = chain <$> aexp <*> many (A <$> (V <$> op) <*> aexp)

gcon = wantConId <|> paren (wantqconsym <|> res ",") <|> ((++) <$> res "[" <*> (res "]"))

apat = PatVar <$> var <*> (res "@" *> (Just <$> apat) <|> pure Nothing)
  <|> flip PatVar Nothing <$> (res "_" *> pure "_")
  <|> flip PatCon [] <$> gcon
  <|> PatLit <$> wantLit
  <|> foldr (\h t -> PatCon ":" [h, t]) (PatCon "[]" [])
    <$> between (res "[") (res "]") (sepBy pat $ res ",")
  <|> paren (foldr1 pairPat <$> sepBy1 pat (res ",") <|> pure (PatCon "()" []))
  where pairPat x y = PatCon "," [x, y]

patChain a = \case
  [] -> a
  PatCon f [b]:rest -> case rest of
    [] -> PatCon f [a, b]
    _ -> PatCon "{+" $ PatCon f [a, b] : rest
  _ -> error "unreachable"
patAtom = PatCon <$> gcon <*> many apat <|> apat
pat = patChain <$> patAtom <*> many (PatCon <$> qconop <*> ((:[]) <$> patAtom))

maybeWhere p = (&) <$> p <*> (res "where" *> (addLets <$> braceDef) <|> pure id)

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
coalesce = \case
  [] -> []
  h@(s, x):t -> case t of
    [] -> [h]
    (s', x'):t' -> let
      f (Pa vsts) (Pa vsts') = Pa $ vsts ++ vsts'
      f _ _ = error "bad multidef"
      in if s == s' then coalesce $ (s, f x x'):t' else h:coalesce t
defSemi = coalesce . concat <$> sepBy1 def (res ";")
braceDef = concat <$> braceSep defSemi

simpleType c vs = foldl TAp (TC c) (map TV vs)
conop = want f <|> between (res "`") (res "`") (want g) where
  f (ConSym s) = Right s
  f _ = Left ""
  g (ConId s) = Right s
  g _ = Left "want conop"
commaVars = sepBy1 var $ res ","
fieldDecl = (\vs t -> map (, t) vs) <$> commaVars <*> (res "::" *> _type)
constr = (\x c y -> Constr c [("", x), ("", y)]) <$> aType <*> conop <*> aType
  <|> Constr <$> wantConId <*>
    (   concat <$> between (res "{") (res "}") (fieldDecl `sepBy` res ",")
    <|> map ("",) <$> many aType)
dclass = wantConId
_deriving = (res "deriving" *> ((:[]) <$> dclass <|> paren (dclass `sepBy` res ","))) <|> pure []
adt = addAdt <$> between (res "data") (res "=") (simpleType <$> wantConId <*> many wantVarId) <*> sepBy constr (res "|") <*> _deriving

impDecl = addImport <$> (res "import" *> wantConId)

topdecls = braceSep
  (   adt
  <|> classDecl
  <|> addTopDecl <$> genDecl
  <|> instDecl
  <|> res "foreign" *>
    (   res "import" *> var *> (addForeignImport <$> wantString <*> var <*> (res "::" *> _type))
    <|> res "export" *> var *> (addForeignExport <$> wantString <*> var)
    )
  <|> addDefs <$> defSemi
  <|> fixity
  <|> impDecl
  )

export_ = ExportVar <$> wantVarId <|> ExportCon <$> wantConId <*>
  (   paren ((:[]) <$> res ".." <|> sepBy (var <|> con) (res ","))
  <|> pure []
  )
exports = Just <$> paren (export_ `sepBy` res ",")
  <|> pure Nothing

haskell = some do
  (moduleName, exs) <- mayModule
  (moduleName,) . (exs,) <$> topdecls

mayModule = res "module" *> ((,) <$> wantConId <*> exports <* res "where")
  <|> pure ("Main", Nothing)

offside xs = Ell (landin xs) []
program s = case lexer posLexemes $ LexState s (1, 1) of
  Left e -> Left e
  Right (xs, LexState [] _) -> parse haskell $ offside xs
  Right (_, st) -> Left "unlexable"
