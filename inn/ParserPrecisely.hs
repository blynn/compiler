module Parser where
import Base
import Ast
import Map

-- Parser.
data ParserState = ParserState
  { readme :: [(Char, (Int, Int))]
  , landin :: String
  , indents :: [Int]
  , openClose :: (Bool, Bool)
  } deriving Show
data Parser a = Parser { getParser :: ParserState -> Either (String, ParserState) (a, ParserState) }
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
  x <|> y = Parser \inp -> case getParser x inp of
    Left (err, errInp) -> if samePos inp errInp then getParser y inp else Left (err, errInp)
    Right x -> Right x

putOC o c = Parser \pasta -> Right ((), pasta { openClose = (o, c) })
getOC = Parser \pasta -> Right (openClose pasta, pasta)
setOC = putOC True True
clearOC = putOC False False

lookahead p = do
  saved <- Parser \pasta -> Right (pasta, pasta)
  ret <- p *> pure (pure ()) <|> pure (bad "lookahead")
  Parser \_ -> Right ((), saved)
  ret

notFollowedBy p = do
  saved <- Parser \pasta -> Right (pasta, pasta)
  ret <- p *> pure (bad "notFollowedBy") <|> pure (pure ())
  Parser \_ -> Right ((), saved)
  ret

try p = Parser \inp -> case getParser p inp of
  Left (err, foo) -> Left (err, inp)
  Right x -> Right x

samePos p q = case readme p of
  [] -> null $ readme q
  (_, (r0, c0)):_ -> case readme q of
    (_, (r1, c1)):_ -> r0 == r1 && c0 == c1
    _ -> False

parse f str = either badpos Right $ getParser f initState where
  initState = ParserState (rowcol str (1, 1)) [] [] (False, False)
  rowcol s rc = case s of
    [] -> []
    h:t -> (h, rc) : rowcol t (advanceRC (ord h) rc)
  advanceRC n (r, c)
    | n `elem` [10, 11, 12, 13] = (r + 1, 1)
    | n == 9 = (r, (c + 8)`mod`8)
    | True = (r, c + 1)

badpos (s, pasta) = Left $ loc $ ": " ++ s where
  loc = case readme pasta of
    [] -> ("EOF"++)
    (_, (r, c)):_ -> ("row "++) . shows r . (" col "++) . shows c

indentOf pasta = case readme pasta of
  [] -> 1
  (_, (_, c)):_ -> c

ins c pasta = pasta { landin = c:landin pasta }

curlyCheck f = do
  Parser \pasta -> Right ((), pasta { indents = 0:indents pasta })
  r <- f
  Parser \pasta -> let pasta' = pasta { indents = tail $ indents pasta } in case readme pasta of
    []              -> Right ((), curly 0 pasta')
    ('{', _):_      -> Right ((), pasta')
    (_, (_, col)):_ -> Right ((), curly col pasta')
  pure r

angle n pasta = case indents pasta of
  m:ms | m == n -> ins ';' pasta
       | n < m -> ins '}' $ angle n pasta { indents = ms }
  _ -> pasta

curly n pasta = let err s = Left (s, pasta) in case indents pasta of
  m:ms | m < n -> ins '{' pasta { indents = n:m:ms }
  [] | 1 <= n -> ins '{' pasta { indents = [n] }
  _ -> ins '{' . ins '}' $ angle n pasta

sat f = Parser \pasta -> let err s = Left (s, pasta) in case landin pasta of
  c:t -> if f c then Right (c, pasta { landin = t }) else err "unsat"
  [] -> case readme pasta of
    [] -> case indents pasta of
      [] -> err "EOF"
      m:ms | m /= 0 && f '}' -> Right ('}', pasta { indents = ms })
      _ -> err "unsat"
    (h, _):t | f h -> let
      p' = pasta { readme = t }
      in case h of
        '}' -> case indents pasta of
          0:ms -> Right (h, p' { indents = ms })
          _ -> err "unsat"
        '{' -> Right (h, p' { indents = 0:indents p' })
        _ -> Right (h, p')
    _ -> err "unsat"

char = sat . (==)
oneOf s = sat (`elem` s)
rawSat f = Parser \pasta -> let err s = Left (s, pasta) in case readme pasta of
  [] -> err "EOF"
  (h, _):t -> if f h then Right (h, pasta { readme = t }) else err "unsat"

eof = Parser \pasta -> case pasta of
  ParserState [] [] _ _ -> Right ((), pasta)
  _ -> Left ("want eof", pasta)

blockComment = try (rawSat ('{' ==) *> rawSat ('-' ==)) *> blockCommentBody
blockCommentBody = try (rawSat ('-' ==) *> rawSat ('}' ==)) *> pure False <|>
  (||) <$> (blockComment <|> pure False) <*> ((||) <$> (isNewline <$> rawSat (const True)) <*> blockCommentBody)
comment = try $ rawSat ('-' ==) *> some (rawSat ('-' ==)) *>
  (rawSat isNewline <|> rawSat (not . isSymbol) *> many (rawSat $ not . isNewline) *> rawSat isNewline) *> pure True
spaces = isNewline <$> rawSat isSpace
singleWhitespace = spaces <|> comment <|> blockComment
whitespace = (<|> pure ()) do
  offside <- or <$> some singleWhitespace
  when offside $ Parser \pasta -> Right ((), angle (indentOf pasta) pasta)
  clearOC

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
nameTailChar = small <|> large <|> digit <|> char '\''
nameTailed p = liftA2 (:) p $ many nameTailChar

nat = readNatural <$> some digit
hexInt = foldl (\n d -> 16*n + (hexValue d)) 0 <$> some hexit
escape = char '\\' *> (oneOf "'\"\\" <|> char 'n' *> pure '\n' <|> (chr . fromInteger <$> nat) <|> char 'x' *> (chr <$> hexInt))
tokOne delim = escape <|> rawSat (delim /=)

charSeq = try . mapM char
tokChar = between (char '\'') (char '\'') (tokOne '\'')
quoteStr = between (char '"') (char '"') $ many $ many (charSeq "\\&") *> tokOne '"'
quasiquoteStr = charSeq "[r|" *> quasiquoteBody
quasiquoteBody = charSeq "|]" *> pure [] <|> (:) <$> rawSat (const True) <*> quasiquoteBody
tokStr = quoteStr <|> quasiquoteStr
hexconstant = do
  sgn <- try unaryMinus *> pure ('-':) <|> pure id
  try $ char '0' *> (char 'x' <|> char 'X')
  litinteger . sgn . ('x':) <$> some hexit
decimal = do
  ds <- (try unaryMinus *> pure ('-':) <|> pure id) <*> some digit
  litdouble . (ds++) . ('.':) <$> (try $ char '.' *> some digit) <|> pure (litinteger ds)
litdouble s = E $ Lit (TC "Double", s)
litchar c = E $ Lit (TC "Char", [c])
litstr s = E $ Lit (TAp (TC "[]") $ TC "Char", s)
litinteger s = A (V "fromInteger") $ E $ Lit (TC "Integer", s)
literal = lexeme $ hexconstant <|> decimal <|> litchar <$> tokChar <|> litstr <$> tokStr
varish = lexeme $ nameTailed small
bad s = Parser \pasta -> Left (s, pasta)

reservedId s = elem s
  ["export", "case", "cases", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"]

varId = try do
  s <- varish
  if reservedId s then bad $ "reserved: " ++ s else pure s

reservedSym s = elem s ["..", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

varSymish = (:) <$> sat (\c -> isSymbol c && c /= ':') <*> many (sat isSymbol)
varSym = do
  clo <- snd <$> getOC
  s <- varSymish
  when (reservedSym s) $ bad $ "reserved: " ++ s
  when (s == "-" && not clo) $ lookahead nonOpener <|> bad "unary minus"
  whitespace
  pure s

unaryMinus = do
  clo <- snd <$> getOC
  when clo $ bad "want non-closing lexeme"
  char '-'
  notFollowedBy $ sat isSymbol
  notFollowedBy nonOpener *> pure "-" <|> bad "want opening lexeme"

conId = lexeme $ nameTailed large
conSymish = lexeme $ liftA2 (:) (char ':') $ many $ sat isSymbol
conSym = do
  s <- conSymish
  if elem s [":", "::"] then bad $ "reserved: " ++ s else pure s

lexeme f = f <* setOC <* whitespace
charNeither c = char c <* clearOC <* whitespace
charOpener c = char c <* putOC True False <* whitespace
charCloser c = char c <* putOC False True <* whitespace

nonOpener = singleWhitespace *> pure () <|> oneOf ")}],;" *> pure ()
nonCloser = singleWhitespace *> pure () <|> oneOf "({[,;" *> pure ()

comma = charNeither ','
semicolon = charNeither ';'
lParen = charOpener '('
rParen = charCloser ')'
lBrace = charOpener '{'
rBrace = charCloser '}'
lSquare = charOpener '['
rSquare = charCloser ']'

backquoted = between (char '`' *> whitespace) (char '`' *> whitespace)

lexemePrelude = whitespace *>
  Parser \pasta -> case getParser (res "module" <|> (:[]) <$> char '{') pasta of
    Left _ -> Right ((), curly (indentOf pasta) pasta)
    Right _ -> Right ((), pasta)

conOf (Constr s _) = s
mkStrs = snd . foldl (\(s, l) u -> ('@':s, s:l)) ("@", [])
scottEncode _ ":" _ = E $ Basic "CONS"
scottEncode vs s ts = foldr L (foldl (\a b -> A a (V b)) (V s) ts) (ts ++ vs)
scottConstr t cs (Constr s sts) = foldr (.) (insertWith (error $ "constructor conflict: " ++ s) s
  (Qual [] $ foldr arr t ts , scottEncode (map conOf cs) s $ mkStrs ts)
  ) [insertWith (error $ "field conflict: " ++ field) field (Qual [] $ t `arr` ft, L s $ foldl A (V s) $ inj $ proj field) | (field, ft) <- sts, field /= ""]
  where
  ts = snd <$> sts
  proj fd = foldr L (V fd) $ fst <$> sts
  inj x = map (\(Constr s' _) -> if s' == s then x else V "undefined") cs
mkAdtDefs t cs = foldr (.) id $ scottConstr t cs <$> cs

mkFFIHelper n t acc = case t of
  TC s -> acc
  TAp (TC "IO") _ -> acc
  TAp (TAp (TC "->") x) y -> case x of
    TC c | elem c ["Char", "Int", "Word"] -> L (show n) $ mkFFIHelper (n + 1) y $ A (V $ show n) acc
    _ -> L (show n) $ mkFFIHelper (n + 1) y $ A acc $ V $ show n

updateDcs t cs dcs = foldr (\(Constr s _) m -> insert s (q, cs) m) dcs cs where
  q = Qual [] $ arr t $ foldr arr (TV "case") $ map (\(Constr _ sts) -> foldr arr (TV "case") $ snd <$> sts) cs

addAdt t cs ders neat = foldr derive neat' ders where
  neat' = neat
    { typedAsts = mkAdtDefs t cs $ typedAsts neat
    , dataCons = updateDcs t cs $ dataCons neat
    , type2Cons = insert (typeName t) (concatMap cnames cs) $ type2Cons neat
    }
  typeName = \case
    TAp x _ -> typeName x
    TC c -> c
  cnames (Constr s sts) = s : concatMap (\(s, _) -> if s == "" then [] else [s]) sts
  derive "Eq" = addInstance "Eq" (mkPreds "Eq") t
    [("==", Pa $ map eqCase cs
    )]
  derive "Show" = addInstance "Show" (mkPreds "Show") t
    [("showsPrec", L "prec" $ Pa $ map showCase cs
    )]
  derive der = error $ "bad deriving: " ++ der
  prec0 = A (V "ord") (litchar '\0')
  showCase (Constr con args) = let as = show <$> [1..length args]
    in ([PatCon con $ mkPatVar "" <$> as], case args of
      [] -> A (V "++") (litstr con)
      _ -> case con of
        ':':_ -> A (A (V "showParen") $ V "True") $ foldr1
          (\f g -> A (A (V ".") f) g)
          [ A (A (V "showsPrec") prec0) (V "1")
          , A (V "++") (litstr $ ' ':con++" ")
          , A (A (V "showsPrec") prec0) (V "2")
          ]
        _ -> A (A (V "showParen") $ A (A (V "<=") prec0) $ V "prec")
          $ A (A (V ".") $ A (V "++") (litstr con))
          $ foldr (\f g -> A (A (V ".") f) g) (L "x" $ V "x")
          $ map (\a -> A (A (V ".") (A (V ":") (litchar ' '))) $ A (A (V "showsPrec") prec0) (V a)) as
      )
  mkPreds classId = Pred classId . TV <$> typeVars t
  mkPatVar pre s = PatVar (pre ++ s) Nothing
  eqCase (Constr con args) = let as = show <$> [1..length args]
    in ([PatCon con $ mkPatVar "l" <$> as], Pa
      [ ([PatCon con $ mkPatVar "r" <$> as], foldr (\x y -> (A (A (V "&&") x) y)) (V "True")
         $ map (\n -> A (A (V "==") (V $ "l" ++ n)) (V $ "r" ++ n)) as)
      , ([PatVar "_" Nothing], V "False")])

addClass classId v (sigs, defs) neat = if not $ member classId $ typeclasses neat then addDefs defaults neat
  { typeclasses = insert classId (keys sigs) $ typeclasses neat
  , typedAsts = selectors $ typedAsts neat
  } else error $ "duplicate class: " ++ classId
  where
  vars = take (size sigs) $ show <$> [0..]
  selectors = foldr (.) id $ zipWith (\var (s, Qual ps t) -> insertWith (error $ "method conflict: " ++ s) s (Qual (Pred classId v:ps) t,
    L "@" $ A (V "@") $ foldr L (V var) vars)) vars $ toAscList sigs
  defaults = map (\(s, t) -> if member s sigs then ("{default}" ++ s, t) else error $ "bad default method: " ++ s) $ toAscList defs

addInstance classId ps ty ds neat = neat
  { instances = insertWith (++) classId [Instance ty name ps (fromList ds)] $ instances neat
  } where
  name = '{':classId ++ (' ':shows ty "}")

addTopDecl (s, t) neat = neat { topDecls = insert s t $ topDecls neat }

addForeignImport foreignname ourname t neat = neat
  { typedAsts = insertWith (error $ "import conflict: " ++ ourname) ourname (Qual [] t, mkFFIHelper 0 t $ A (E $ Basic "F") $ A (E $ Basic "NUM") $ E $ Link "{foreign}" foreignname) $ typedAsts neat
  , ffiImports = insertWith (error $ "duplicate import: " ++ foreignname) foreignname t $ ffiImports neat
  }
addForeignExport e f neat = neat { ffiExports = insertWith (error $ "duplicate export: " ++ e) e f $ ffiExports neat }
addDefs ds neat = neat { topDefs = foldr (uncurry insert) (topDefs neat) ds }
addImport isQual im mayAs f neat = neat { moduleImports = mergeImport (maybe im id mayAs)
  $ (if isQual then id else mergeImport "") $ moduleImports neat }
  where
  mergeImport k m = insert k post m
    where
    post = case lookup im pre of
      Nothing -> (im, f):pre
      Just _ -> pre  -- TODO: Merge rather than drop new imports.
    pre = maybe [] id $ mlookup k m

addFixities os prec neat = neat { opFixity = foldr (\o tab -> insert o prec tab) (opFixity neat) os }

parseErrorRule = Parser \pasta -> case indents pasta of
  m:ms | m /= 0 -> Right ('}', pasta { indents = ms })
  _ -> Left ("missing }", pasta)

res w@(h:_) = reservedSeq *> pure w <|> bad ("want \"" ++ w ++ "\"") where
  reservedSeq = try $ if elem w ["let", "where", "do", "of"]
    then curlyCheck $ lexeme $ charSeq w *> notFollowedBy nameTailChar
    else lexeme $ charSeq w *> notFollowedBy (if isSmall h then nameTailChar else sat isSymbol)

paren = between lParen rParen
braceSep f = between lBrace (rBrace <|> parseErrorRule) $ foldr ($) [] <$> sepBy ((:) <$> f <|> pure id) semicolon

joinIsFail t = A (L "join#" t) (V "fail#")

addLets ls x = L "let" $ foldr encodeVar (L "in" bodies) ls where
  encodeVar (v, (_, m)) rest = L v case m of
    Nothing -> rest
    Just q -> A (E $ XQual q) rest
  bodies = foldr A x $ joinIsFail . fst . snd <$> ls

op = conSym <|> varSym <|> backquoted (conId <|> varId)

qop = V <$> res ":" <|> modded (varSym <|> conSym) <|> backquoted (modded $ conId <|> varId)
con = conId <|> try (paren conSym)
var = varId <|> try (paren varSym)

modded parser = do
  mods <- many $ try $ nameTailed large <* char '.'
  s <- parser
  pure $ case mods of
    [] -> V s
    _ -> E $ Link (intercalate "." mods) s

tycon = do
  s <- conId
  pure $ if s == "String" then TAp (TC "[]") (TC "Char") else TC s

aType =
  lParen *>
    (   rParen *> pure (TC "()")
    <|> (foldr1 (TAp . TAp (TC ",")) <$> sepBy1 _type comma) <* rParen
    <|> comma *> rParen *> pure (TC ",")
    <|> TC <$> res "->" <* rParen
    )
  <|> tycon
  <|> TV <$> varId
  <|> (lSquare *> (rSquare *> pure (TC "[]") <|> TAp (TC "[]") <$> (_type <* rSquare)))
bType = foldl1 TAp <$> some aType
_type = foldr1 arr <$> sepBy bType (res "->")

fixityDecl w a = do
  res w
  n <- lexeme nat
  os <- sepBy op comma
  pure $ addFixities os (fromInteger n, a)

fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc

cDecls = first fromList . second fromList . foldr ($) ([], []) <$> braceSep cDecl
cDecl = first . (:) <$> genDecl <|> second . (++) <$> defSemi

genDecl = (,) <$> try (var <* res "::") <*> (Qual <$> fatArrows <*> _type)

classDecl = res "class" *> (addClass <$> conId <*> (TV <$> varId) <*> (res "where" *> cDecls))

simpleClass = Pred <$> conId <*> _type
scontext = (:[]) <$> simpleClass <|> paren (sepBy simpleClass comma)
fatArrows = concat <$> many (try $ scontext <* res "=>")

instDecl = res "instance" *>
  ((\ps cl ty defs -> addInstance cl ps ty defs) <$> fatArrows
    <*> conId <*> _type <*> (res "where" *> fmap (map $ second fst) braceDef))

letin = addLets <$> between (res "let") (res "in") braceDef <*> expr
ifthenelse = (\a b c -> A (A (A (V "if") a) b) c) <$>
  (res "if" *> expr) <*> (res "then" *> expr) <*> (res "else" *> expr)
listify = foldr (\h t -> A (A (V ":") h) t) (V "[]")

alts = joinIsFail . Pa <$> braceSep (gateGuard (\x y -> ([x], y)) pat "->")
cas = flip A <$> between (res "case") (res "of") expr <*> alts
lamCase = curlyCheck (res "case") *> alts

nalts = joinIsFail . Pa <$> braceSep (gateGuard (,) (many apat) "->")
lamCases = curlyCheck (res "cases") *> nalts

lam = res "\\" *> (lamCase <|> lamCases <|> liftA2 onePat (some apat) (res "->" *> expr))

flipPairize y x = A (A (V ",") x) y
moreCommas = foldr1 (A . A (V ",")) <$> sepBy1 expr comma
thenComma = comma *> ((flipPairize <$> moreCommas) <|> pure (A (V ",")))
parenExpr = (&) <$> expr <*> (((\o a -> A o a) <$> qop) <|> thenComma <|> pure id)
-- "(- x)" and "(-x)" differ, thus the `try`.
rightSect = ((\o a -> L "@" $ A (A o $ V "@") a) <$> (try qop <|> V . (:"") <$> comma)) <*> expr
section = rightSect <|> parenExpr

maybePureUnit = maybe (V "pure" `A` V "()") id
stmt = letStmt
  <|> (\p x -> Just . A (V ">>=" `A` x) . onePat [p] . maybePureUnit) <$> try (pat <* res "<-") <*> expr
  <|> constBind <$> expr
  where
  constBind x = Just . maybe x (\y -> (V ">>=" `A` x) `A` (L "_" y))
  letStmt = do
    ds <- res "let" *> braceDef
    constBind . addLets ds <$> (res "in" *> expr) <|> pure (Just . addLets ds . maybePureUnit)

doblock = res "do" *> (maybePureUnit . foldr ($) Nothing <$> braceSep stmt)

compQual =
  (\p xs e -> A (A (V "concatMap") $ onePat [p] e) xs)
    <$> (try $ pat <* res "<-") <*> expr
  <|> letComp
  <|> cond <$> expr
  where
  cond b e = A (A (A (V "if") b) e) $ V "[]"
  letComp = do
    ds <- res "let" *> braceDef
    cond <$> (res "in" *> expr) <|> pure (addLets ds)

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

fbind = A <$> (V <$> var) <*> (res "=" *> expr)

fBinds v = (do
    fbs <- between lBrace rBrace $ sepBy1 fbind comma
    pure $ A (E $ Basic "{=") $ foldr A (E $ Basic "=}") $ v:fbs
  ) <|> pure v

atom = ifthenelse <|> doblock <|> letin
  <|> cas <|> lam <|> literal <|> sqExpr
  <|> modded (conId <|> varId)
  <|> lParen *> parenAtom
  >>= fBinds

parenAtom = rParen *> pure (V "()")
  <|> try ((comma *> pure (V ",") <|> V <$> res ":" <|> modded (conSym <|> varSym)) <* rParen)
  <|> section <* rParen

aexp = foldl1 A <$> some unexp
unexp = try unaryMinus *> (A (V "negate") <$> unexp) <|> atom

chain a = \case
  [] -> a
  A f b:rest -> case rest of
    [] -> A (A f a) b
    _ -> A (E $ Basic "{+") $ A (A (A f a) b) $ foldr A (E $ Basic "+}") rest
  _ -> error "unreachable"
expr = do
  x <- chain <$> aexp <*> many (try $ A <$> qop <*> aexp)
  res "::" *> annotate x <|> pure x
annotate x = do
  q <- Qual <$> fatArrows <*> _type
  pure $ L "::" $ A x $ E $ XQual q

gcon = conId <|> try (paren $ res ":" <|> (:"") <$> comma <|> conSym)
qconop = res ":" <|> conSym <|> backquoted conId

apat = PatVar <$> var <*> (res "@" *> (Just <$> apat) <|> pure Nothing)
  <|> flip PatVar Nothing <$> (res "_" *> pure "_")
  <|> flip PatCon [] <$> gcon
  <|> paren (foldr1 pairPat <$> sepBy1 pat comma <|> pure (PatCon "()" []))
  <|> PatLit <$> literal
  <|> foldr (\h t -> PatCon ":" [h, t]) (PatCon "[]" [])
    <$> between lSquare rSquare (sepBy pat comma)
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

gateGuard :: (a -> Ast -> b) -> Parser a -> String -> Parser b
gateGuard f lhsParser s = do
  x <- try $ lhsParser <* lookahead (res s <|> res "|")
  y <- maybeWhere $ res s *> expr <|> foldr ($) (V "join#") <$> some (between (res "|") (res s) guards <*> expr)
  pure $ f x y

guards = foldr1 (\f g -> \yes no -> f (g yes no) no) <$> sepBy1 guard comma
guard = try (guardPat <$> pat <*> (res "<-" *> expr))
  <|> guardLets <$> (res "let" *> braceDef)
  <|> guardExpr <$> expr
guardExpr x yes no = case x of
  V "True" -> yes
  _ -> A (A (A (V "if") x) yes) no
guardPat p x yes no = A (Pa [([p], yes), ([PatVar "_" Nothing], no)]) x
guardLets defs yes no = addLets defs yes

onePat vs x = joinIsFail $ Pa [(vs, x)]
leftyPat p expr = case pvars of
  [] -> []
  (h:t) -> let gen = '@':h in
    (gen, expr):map (\v -> (v, A (Pa [([p], V v)]) $ V gen)) pvars
  where
  pvars = filter (/= "_") $ patVars p
funlhs = try ((\x o y -> (o, [x, y])) <$> pat <*> varSym <*> pat)
  <|> liftA2 (,) var (many apat)
  <|> (\(s, vs) vs' -> (s, vs ++ vs')) <$> paren funlhs <*> some apat
def = gateGuard (\(s, vs) x -> (s, Pa [(vs, x)])) funlhs "="
coalesce = \case
  [] -> []
  h@(s, x):t -> case t of
    [] -> [h]
    (s', x'):t' -> let
      f (Pa vsts) (Pa vsts') = Pa $ vsts ++ vsts'
      f _ _ = error "bad multidef"
      in if s == s' then coalesce $ (s, f x x'):t' else h:coalesce t
defSemi = coalesce <$> liftA2 (:) def (many $ try $ some semicolon *> def) <|> gateGuard leftyPat pat "="
braceDef = do
  (defs, annos) <- foldr (\(f, g) (x, y) -> (f x, g y)) ([], []) <$> braceSep ((,id) . (++) <$> defSemi <|> (id,) . (:) <$> genDecl)
  let
    tab = fromList $ second (,Nothing) <$> defs
    go tab (s, t) = case mlookup s tab of
      Nothing -> Left $ "missing definition: " ++ s
      Just (a, m) -> case m of
        Nothing -> Right $ insert s (a, Just t) tab
        _ -> Left $ "duplicate annotation: " ++ s
  case foldM go tab annos of
    Left e -> bad e
    Right tab -> pure $ toAscList tab

simpleType c vs = foldl TAp (TC c) (map TV vs)
conop = conSym <|> backquoted conId
fieldDecl = (\vs t -> map (, t) vs) <$> sepBy1 var comma <*> (res "::" *> _type)
constr = try ((\x c y -> Constr c [("", x), ("", y)]) <$> bType <*> conop <*> bType)
  <|> Constr <$> conId <*>
    (   concat <$> between lBrace rBrace (fieldDecl `sepBy` comma)
    <|> map ("",) <$> many aType)
dclass = conId
_deriving = (res "deriving" *> ((:[]) <$> dclass <|> paren (dclass `sepBy` comma))) <|> pure []
adt = addAdt <$> between (res "data") (res "=") (simpleType <$> conId <*> many varId) <*> sepBy1 constr (res "|") <*> _deriving

impDecl = do
  res "import"
  preQual <- const True <$> res "qualified" <|> pure False
  q <- conId
  postQual <- const True <$> res "qualified" <|> pure False
  when (preQual && postQual) $ bad "overqualified"
  addImport (preQual || postQual) q <$>
    (res "as" *> (Just <$> conId) <|> pure Nothing) <*>
    ( paren (flip elem <$> sepBy var comma)
    <|> res "hiding" *> paren ((not .) . flip elem <$> sepBy var comma)
    <|> pure (const True)
    )

typeDecl = addTypeAlias <$> between (res "type") (res "=") conId <*> _type

addTypeAlias s t neat = neat { typeAliases = insertWith (error $ "duplicate: " ++ s) s t $ typeAliases neat }

tops = fmap (foldr (.) id) $ braceSep
  $   adt
  <|> classDecl
  <|> instDecl
  <|> res "foreign" *>
    (   res "import" *> var *> (addForeignImport <$> lexeme tokStr <*> var <*> (res "::" *> _type))
    <|> res "export" *> var *> (addForeignExport <$> lexeme tokStr <*> var)
    )
  <|> fixity
  <|> impDecl
  <|> typeDecl
  <|> addTopDecl <$> genDecl
  <|> addDefs <$> defSemi

export_ = ExportVar <$> varId <|> ExportCon <$> conId <*>
  (   paren ((:[]) <$> res ".." <|> sepBy (var <|> con) comma)
  <|> pure []
  )
exports = Just <$> paren (export_ `sepBy` comma)
  <|> pure Nothing

haskell = between lexemePrelude eof $ some $ liftA2 (,) mayModule tops

mayModule = res "module" *> ((,) <$> conId <*> exports <* res "where")
  <|> pure ("Main", Nothing)

parseProgram s = fmap fst $ parse haskell s
