module Parser where
import Base
import Ast
import Map

-- Parser.
data ParserState = ParserState
  [(Char, (Int, Int))]
  String
  [Int]
  (Map String (Int, Assoc))

readme  (ParserState x _ _ _) = x
landin  (ParserState _ x _ _) = x
indents (ParserState _ _ x _) = x
precs   (ParserState _ _ _ x) = x
putReadme  x (ParserState _ a b c) = ParserState x a b c
putLandin  x (ParserState a _ b c) = ParserState a x b c
modIndents f (ParserState a b x c) = ParserState a b (f x) c
data Parser a = Parser (ParserState -> Either String (a, ParserState))
getParser (Parser p) = p
instance Functor Parser where fmap f x = pure f <*> x
instance Applicative Parser where
  pure x = Parser \inp -> Right (x, inp)
  (Parser f) <*> (Parser x) = Parser \inp -> do
    (fun, t) <- f inp
    (arg, u) <- x t
    pure (fun arg, u)
instance Monad Parser where
  return = pure
  (Parser x) >>= f = Parser \inp -> do
    (a, t) <- x inp
    getParser (f a) t
instance Alternative Parser where
  empty = bad ""
  x <|> y = Parser \inp -> either (const $ getParser y inp) Right $ getParser x inp
getPrecs = Parser \st -> Right (precs st, st)
putPrecs ps = Parser \(ParserState a b c _) -> Right ((), ParserState a b c ps)

notFollowedBy p = do
  saved <- Parser \pasta -> Right (pasta, pasta)
  ret <- p *> pure (bad "") <|> pure (pure ())
  Parser \_ -> Right ((), saved)
  ret

parse f str = getParser f $ ParserState (rowcol str (1, 1)) [] [] $ singleton ":" (5, RAssoc) where
  rowcol s rc = case s of
    [] -> []
    h:t -> (h, rc) : rowcol t (advanceRC (ord h) rc)
  advanceRC n (r, c)
    | n `elem` [10, 11, 12, 13] = (r + 1, 1)
    | n == 9 = (r, (c + 8)`mod`8)
    | True = (r, c + 1)

indentOf pasta = case readme pasta of
  [] -> 1
  (_, (_, c)):_ -> c

ins c pasta = putLandin (c:landin pasta) pasta

angle n pasta = case indents pasta of
  m:ms | m == n -> ins ';' pasta
       | n + 1 <= m -> ins '}' $ angle n $ modIndents tail pasta
  _ -> pasta

curly n pasta = case indents pasta of
  m:ms | m + 1 <= n -> ins '{' $ modIndents (n:) pasta
  [] | 1 <= n -> ins '{' $ modIndents (n:) pasta
  _ -> ins '{' . ins '}' $ angle n pasta

sat f = Parser \pasta -> case landin pasta of
  c:t -> if f c then Right (c, putLandin t pasta) else Left "unsat"
  [] -> case readme pasta of
    [] -> case indents pasta of
      [] -> Left "EOF"
      m:ms | m /= 0 && f '}' -> Right ('}', modIndents tail pasta)
      _ -> Left "unsat"
    (h, _):t | f h -> let
      p' = putReadme t pasta
      in case h of
        '}' -> case indents pasta of
          0:ms -> Right (h, modIndents tail p')
          _ -> Left "unsat"
        '{' -> Right (h, modIndents (0:) p')
        _ -> Right (h, p')
    _ -> Left "unsat"

char c = sat (c ==)

rawSat f = Parser \pasta -> case readme pasta of
  [] -> Left "EOF"
  (h, _):t -> if f h then Right (h, putReadme t pasta) else Left "unsat"

eof = Parser \pasta -> case pasta of
  ParserState [] [] _ _ -> Right ((), pasta)
  _ -> badpos pasta "want eof"

comment = rawSat ('-' ==) *> some (rawSat ('-' ==)) *>
  (rawSat isNewline <|> rawSat (not . isSymbol) *> many (rawSat $ not . isNewline) *> rawSat isNewline) *> pure True
spaces = isNewline <$> rawSat isSpace
whitespace = do
  offside <- or <$> many (spaces <|> comment)
  Parser \pasta -> Right ((), if offside then angle (indentOf pasta) pasta else pasta)

hexValue d
  | d <= '9' = ord d - ord '0'
  | d <= 'F' = 10 + ord d - ord 'A'
  | d <= 'f' = 10 + ord d - ord 'a'
isNewline c = ord c `elem` [10, 11, 12, 13]
isSymbol = (`elem` "!#$%&*+./<=>?@\\^|-~:")
isSmall c = c <= 'z' && 'a' <= c || c == '_'
small = sat isSmall
large = sat \x -> (x <= 'Z') && ('A' <= x)
hexit = sat \x -> (x <= '9') && ('0' <= x)
  || (x <= 'F') && ('A' <= x)
  || (x <= 'f') && ('a' <= x)
digit = sat \x -> (x <= '9') && ('0' <= x)
decimal = foldl (\n d -> 10*n + ord d - ord '0') 0 <$> some digit
hexadecimal = foldl (\n d -> 16*n + hexValue d) 0 <$> some hexit
nameTailChar = small <|> large <|> digit <|> char '\''
nameTailed p = liftA2 (:) p $ many nameTailChar

escape = char '\\' *> (sat (`elem` "'\"\\") <|> char 'n' *> pure '\n' <|> char '0' *> pure (chr 0) <|> char 'x' *> (chr <$> hexadecimal))
tokOne delim = escape <|> rawSat (delim /=)

charSeq = mapM char
tokChar = between (char '\'') (char '\'') (tokOne '\'')
quoteStr = between (char '"') (char '"') $ many $ many (charSeq "\\&") *> tokOne '"'
quasiquoteStr = charSeq "[r|" *> quasiquoteBody
quasiquoteBody = charSeq "|]" *> pure [] <|> (:) <$> rawSat (const True) <*> quasiquoteBody
tokStr = quoteStr <|> quasiquoteStr
integer = char '0' *> (char 'x' <|> char 'X') *> hexadecimal <|> decimal
literal = lexeme . fmap E $ Const <$> integer <|> ChrCon <$> tokChar <|> StrCon <$> tokStr
varish = lexeme $ nameTailed small
bad s = Parser \pasta -> badpos pasta s
badpos pasta s = Left $ loc $ ": " ++ s where
  loc = case readme pasta of
    [] -> ("EOF"++)
    (_, (r, c)):_ -> ("row "++) . shows r . (" col "++) . shows c
varId = do
  s <- varish
  if elem s
    ["export", "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"]
    then bad $ "reserved: " ++ s else pure s
varSymish = lexeme $ (:) <$> sat (\c -> isSymbol c && c /= ':') <*> many (sat isSymbol)
varSym = lexeme $ do
  s <- varSymish
  if elem s ["..", "=", "\\", "|", "<-", "->", "@", "~", "=>"] then bad $ "reserved: " ++ s else pure s

conId = lexeme $ nameTailed large
conSymish = lexeme $ liftA2 (:) (char ':') $ many $ sat isSymbol
conSym = do
  s <- conSymish
  if elem s [":", "::"] then bad $ "reserved: " ++ s else pure s
special c = lexeme $ sat (c ==)
comma = special ','
semicolon = special ';'
lParen = special '('
rParen = special ')'
lBrace = special '{'
rBrace = special '}'
lSquare = special '['
rSquare = special ']'
backquote = special '`'

lexeme f = f <* whitespace

lexemePrelude = whitespace *>
  Parser \pasta -> case getParser (res "module" <|> (:[]) <$> char '{') pasta of
    Left _ -> Right ((), curly (indentOf pasta) pasta)
    Right _ -> Right ((), pasta)

curlyCheck f = do
  Parser \pasta -> Right ((), modIndents (0:) pasta)
  r <- f
  Parser \pasta -> let pasta' = modIndents tail pasta in case readme pasta of
    []              -> Right ((), curly 0 pasta')
    ('{', _):_      -> Right ((), pasta')
    (_, (_, col)):_ -> Right ((), curly col pasta')
  pure r

conOf (Constr s _) = s
specialCase (h:_) = '|':conOf h
mkCase t cs = (specialCase cs,
  ( Qual [] $ arr t $ foldr arr (TV "case") $ map (\(Constr _ ts) -> foldr arr (TV "case") ts) cs
  , E $ Basic "I"))
mkStrs = snd . foldl (\(s, l) u -> ('@':s, s:l)) ("@", [])
scottEncode _ ":" _ = E $ Basic "CONS"
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs)
scottConstr t cs (Constr s ts) = (s,
  (Qual [] $ foldr arr t ts , scottEncode (map conOf cs) s $ mkStrs ts))
mkAdtDefs t cs = mkCase t cs : map (scottConstr t cs) cs

mkFFIHelper n t acc = case t of
  TC s -> acc
  TAp (TC "IO") _ -> acc
  TAp (TAp (TC "->") x) y -> L (show n) $ mkFFIHelper (n + 1) y $ A (V $ show n) acc

updateDcs cs dcs = foldr (\(Constr s _) m -> insert s cs m) dcs cs
addAdt t cs (Neat tycl fs typed dcs ffis ffes ims) =
  Neat tycl fs (mkAdtDefs t cs ++ typed) (updateDcs cs dcs) ffis ffes ims

emptyTycl = Tycl [] []
addClass classId v (sigs, defs) (Neat tycl fs typed dcs ffis ffes ims) = let
  vars = take (size sigs) $ show <$> [0..]
  selectors = zipWith (\var (s, t) -> (s, (Qual [Pred classId v] t,
    L "@" $ A (V "@") $ foldr L (V var) vars))) vars $ toAscList sigs
  defaults = map (\(s, t) -> if member s sigs then ("{default}" ++ s, t) else error $ "bad default method: " ++ s) $ toAscList defs
  Tycl ms is = maybe emptyTycl id $ mlookup classId tycl
  tycl' = insert classId (Tycl (keys sigs) is) tycl
  in if null ms then Neat tycl' (defaults ++ fs) (selectors ++ typed) dcs ffis ffes ims
    else error $ "duplicate class: " ++ classId

addInstance classId ps ty ds (Neat tycl fs typed dcs ffis ffes ims) = let
  Tycl ms is = maybe emptyTycl id $ mlookup classId tycl
  tycl' = insert classId (Tycl ms $ Instance ty name ps (fromList ds):is) tycl
  name = '{':classId ++ (' ':shows ty "}")
  in Neat tycl' fs typed dcs ffis ffes ims

addFFI foreignname ourname t (Neat tycl fs typed dcs ffis ffes ims) = let
  fn = A (E $ Basic "F") $ E $ Const $ length ffis
  in Neat tycl fs ((ourname, (Qual [] t, mkFFIHelper 0 t fn)) : typed) dcs ((foreignname, t):ffis) ffes ims
addDefs ds (Neat tycl fs typed dcs ffis ffes ims) = Neat tycl (ds ++ fs) typed dcs ffis ffes ims
addImport im (Neat tycl fs typed dcs ffis exs ims) = Neat tycl fs typed dcs ffis exs (im:ims)
addExport e f (Neat tycl fs typed dcs ffis ffes ims) = Neat tycl fs typed dcs ffis ((e, f):ffes) ims

parseErrorRule = Parser \pasta -> case indents pasta of
  m:ms | m /= 0 -> Right ('}', modIndents tail pasta)
  _ -> badpos pasta "missing }"

res w@(h:_) = reservedSeq *> pure w <|> bad ("want \"" ++ w ++ "\"") where
  reservedSeq = if elem w ["let", "where", "do", "of"]
    then curlyCheck $ lexeme $ charSeq w *> notFollowedBy nameTailChar
    else lexeme $ charSeq w *> notFollowedBy (if isSmall h then nameTailChar else sat isSymbol)

paren = between lParen rParen
braceSep f = between lBrace (rBrace <|> parseErrorRule) $ foldr ($) [] <$> sepBy ((:) <$> f <|> pure id) semicolon

maybeFix s x = if elem s $ fvPro [] x then A (V "fix") (L s x) else x

nonemptyTails [] = []
nonemptyTails xs@(x:xt) = xs : nonemptyTails xt

joinIsFail t = A (L "join#" t) (V "fail#")

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
    in foldr (\(x:xt) t -> A (L x t) $ maybeFix x $ insLams xt $ suball $ maybe undefined joinIsFail $ lookup x ls) (suball expr) tnames

data Assoc = NAssoc | LAssoc | RAssoc
instance Eq Assoc where
  NAssoc == NAssoc = True
  LAssoc == LAssoc = True
  RAssoc == RAssoc = True
  _ == _ = False
precOf s precTab = maybe 9 fst $ mlookup s precTab
assocOf s precTab = maybe LAssoc snd $ mlookup s precTab

opFold precTab f x xs = case xs of
  [] -> pure x
  (op, y):xt -> case find (\(op', _) -> assocOf op precTab /= assocOf op' precTab) xt of
    Nothing -> case assocOf op precTab of
      NAssoc -> case xt of
        [] -> pure $ f op x y
        y:yt -> bad "NAssoc repeat"
      LAssoc -> pure $ foldl (\a (op, y) -> f op a y) x xs
      RAssoc -> pure $ foldr (\(op, y) b -> \e -> f op e (b y)) id xs $ x
    Just y -> bad "Assoc clash"

qconop = conSym <|> res ":" <|> between backquote backquote conId

qconsym = conSym <|> res ":"

op = qconsym <|> varSym <|> between backquote backquote (conId <|> varId)
con = conId <|> paren qconsym
var = varId <|> paren varSym

tycon = do
  s <- conId
  pure $ if s == "String" then TAp (TC "[]") (TC "Char") else TC s

aType =
  lParen *>
    (   rParen *> pure (TC "()")
    <|> (foldr1 (TAp . TAp (TC ",")) <$> sepBy1 _type comma) <* rParen)
  <|> tycon
  <|> TV <$> varId
  <|> (lSquare *> (rSquare *> pure (TC "[]") <|> TAp (TC "[]") <$> (_type <* rSquare)))
bType = foldl1 TAp <$> some aType
_type = foldr1 arr <$> sepBy bType (res "->")

fixityDecl w a = do
  res w
  n <- lexeme integer
  os <- sepBy1 op comma
  precs <- getPrecs
  putPrecs $ foldr (\o m -> insert o (n, a) m) precs os
fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc

cDecls = first fromList . second fromList . foldr ($) ([], []) <$> braceSep cDecl
cDecl = first . (:) <$> genDecl <|> second . (++) <$> defSemi

genDecl = (,) <$> var <*> (res "::" *> _type)

classDecl = res "class" *> (addClass <$> conId <*> (TV <$> varId) <*> (res "where" *> cDecls))

simpleClass = Pred <$> conId <*> _type
scontext = (:[]) <$> simpleClass <|> paren (sepBy simpleClass comma)

instDecl = res "instance" *>
  ((\ps cl ty defs -> addInstance cl ps ty defs) <$>
  (scontext <* res "=>" <|> pure [])
    <*> conId <*> _type <*> (res "where" *> braceDef))

letin = addLets <$> between (res "let") (res "in") braceDef <*> expr
ifthenelse = (\a b c -> A (A (A (V "if") a) b) c) <$>
  (res "if" *> expr) <*> (res "then" *> expr) <*> (res "else" *> expr)
listify = foldr (\h t -> A (A (V ":") h) t) (V "[]")

alts = joinIsFail . Pa <$> braceSep ((\x y -> ([x], y)) <$> pat <*> guards "->")
cas = flip A <$> between (res "case") (res "of") expr <*> alts
lamCase = curlyCheck (res "case") *> alts
lam = res "\\" *> (lamCase <|> liftA2 onePat (some apat) (res "->" *> expr))

flipPairize y x = A (A (V ",") x) y
moreCommas = foldr1 (A . A (V ",")) <$> sepBy1 expr comma
thenComma = comma *> ((flipPairize <$> moreCommas) <|> pure (A (V ",")))
parenExpr = (&) <$> expr <*> (((\v a -> A (V v) a) <$> op) <|> thenComma <|> pure id)
rightSect = ((\v a -> L "@" $ A (A (V v) $ V "@") a) <$> (op <|> (:"") <$> comma)) <*> expr
section = lParen *> (parenExpr <* rParen <|> rightSect <* rParen <|> rParen *> pure (V "()"))

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

sqExpr = between lSquare rSquare $
  ((&) <$> expr <*>
    (   res ".." *>
      (   (\hi lo -> (A (A (V "enumFromTo") lo) hi)) <$> expr
      <|> pure (A (V "enumFrom"))
      )
    <|> res "|" *>
      ((. A (V "pure")) . foldr (.) id <$> sepBy1 compQual comma)
    <|> (\t h -> listify (h:t)) <$> many (comma *> expr)
    )
  )
  <|> pure (V "[]")

atom = ifthenelse <|> doblock <|> letin <|> sqExpr <|> section
  <|> cas <|> lam <|> (paren comma *> pure (V ","))
  <|> V <$> (con <|> var) <|> literal

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

gcon = conId <|> paren (qconsym <|> (:"") <$> comma) <|> (lSquare *> rSquare *> pure "[]")

apat = PatVar <$> var <*> (res "@" *> (Just <$> apat) <|> pure Nothing)
  <|> flip PatVar Nothing <$> (res "_" *> pure "_")
  <|> flip PatCon [] <$> gcon
  <|> PatLit <$> literal
  <|> foldr (\h t -> PatCon ":" [h, t]) (PatCon "[]" [])
    <$> between lSquare rSquare (sepBy pat comma)
  <|> paren (foldr1 pairPat <$> sepBy1 pat comma <|> pure (PatCon "()" []))
  where pairPat x y = PatCon "," [x, y]

binPat f x y = PatCon f [x, y]
patP n = if n <= 9
  then getPrecs >>= \precTab
    -> patP (succ n) >>= \a
    -> many ((,) <$> withPrec precTab n qconop <*> patP (succ n)) >>= \as
    -> opFold precTab binPat a as
  else PatCon <$> gcon <*> many apat <|> apat
pat = patP 0

maybeWhere p = (&) <$> p <*> (res "where" *> (addLets <$> braceDef) <|> pure id)

guards s = maybeWhere $ res s *> expr <|> foldr ($) (V "join#") <$> some ((\x y -> case x of
  V "True" -> \_ -> y
  _ -> A (A (A (V "if") x) y)
  ) <$> (res "|" *> expr) <*> (res s *> expr))

onePat vs x = joinIsFail $ Pa [(vs, x)]
defOnePat vs x = Pa [(vs, x)]
opDef x f y rhs = [(f, defOnePat [x, y] rhs)]
leftyPat p expr = case pvars of
  [] -> []
  (h:t) -> let gen = '@':h in
    (gen, expr):map (\v -> (v, A (Pa [([p], V v)]) $ V gen)) pvars
  where
  pvars = filter (/= "_") $ patVars p
def = liftA2 (\l r -> [(l, r)]) var (liftA2 defOnePat (many apat) $ guards "=")
  <|> (pat >>= \x -> opDef x <$> varSym <*> pat <*> guards "=" <|> leftyPat x <$> guards "=")
coalesce = \case
  [] -> []
  h@(s, x):t -> case t of
    [] -> [h]
    (s', x'):t' -> let
      f (Pa vsts) (Pa vsts') = Pa $ vsts ++ vsts'
      f _ _ = error "bad multidef"
      in if s == s' then coalesce $ (s, f x x'):t' else h:coalesce t
defSemi = coalesce . concat <$> sepBy1 def (some semicolon)
braceDef = concat <$> braceSep defSemi

simpleType c vs = foldl TAp (TC c) (map TV vs)
conop = conSym <|> between backquote backquote conId
constr = (\x c y -> Constr c [x, y]) <$> aType <*> conop <*> aType
  <|> Constr <$> conId <*> many aType
adt = addAdt <$> between (res "data") (res "=") (simpleType <$> conId <*> many varId) <*> sepBy constr (res "|")

impDecl = addImport <$> (res "import" *> conId)

topdecls = braceSep
  $   adt
  <|> classDecl
  <|> instDecl
  <|> res "foreign" *>
    (   res "import" *> var *> (addFFI <$> lexeme tokStr <*> var <*> (res "::" *> _type))
    <|> res "export" *> var *> (addExport <$> lexeme tokStr <*> var)
    )
  <|> addDefs <$> defSemi
  <|> fixity *> pure id
  <|> impDecl

haskell = between lexemePrelude eof $ some $ (,) <$> (res "module" *> conId <* res "where" <|> pure "Main") <*> topdecls

parseProgram s = fst <$> parse haskell s
