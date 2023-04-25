-- Separate fixity phase.
-- Export lists.
module Parser where
import Base
import Ast
import Map

-- Parser.
data ParserState = ParserState
  { readme :: [(Char, (Int, Int))]
  , landin :: String
  , indents :: [Int]
  }
data Parser a = Parser { getParser :: ParserState -> Either String (a, ParserState) }
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

notFollowedBy p = do
  saved <- Parser \pasta -> Right (pasta, pasta)
  ret <- p *> pure (bad "") <|> pure (pure ())
  Parser \_ -> Right ((), saved)
  ret

parse f str = getParser f $ ParserState (rowcol str (1, 1)) [] [] where
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

ins c pasta = pasta { landin = c:landin pasta }

angle n pasta = case indents pasta of
  m:ms | m == n -> ins ';' pasta
       | n + 1 <= m -> ins '}' $ angle n pasta { indents = ms }
  _ -> pasta

curly n pasta = case indents pasta of
  m:ms | m + 1 <= n -> ins '{' pasta { indents = n:m:ms }
  [] | 1 <= n -> ins '{' pasta { indents = [n] }
  _ -> ins '{' . ins '}' $ angle n pasta

sat f = Parser \pasta -> case landin pasta of
  c:t -> if f c then Right (c, pasta { landin = t }) else Left "unsat"
  [] -> case readme pasta of
    [] -> case indents pasta of
      [] -> Left "EOF"
      m:ms | m /= 0 && f '}' -> Right ('}', pasta { indents = ms })
      _ -> Left "unsat"
    (h, _):t | f h -> let
      p' = pasta { readme = t }
      in case h of
        '}' -> case indents pasta of
          0:ms -> Right (h, p' { indents = ms })
          _ -> Left "unsat"
        '{' -> Right (h, p' { indents = 0:indents p' })
        _ -> Right (h, p')
    _ -> Left "unsat"

char c = sat (c ==)

rawSat f = Parser \pasta -> case readme pasta of
  [] -> Left "EOF"
  (h, _):t -> if f h then Right (h, pasta { readme = t }) else Left "unsat"

eof = Parser \pasta -> case pasta of
  ParserState [] [] _ -> Right ((), pasta)
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

escape = char '\\' *> (sat (`elem` "'\"\\") <|> char 'n' *> pure '\n' <|> char '0' *> pure '\0' <|> char 'x' *> (chr <$> hexadecimal))
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
  Parser \pasta -> Right ((), pasta { indents = 0:indents pasta })
  r <- f
  Parser \pasta -> let pasta' = pasta { indents = tail $ indents pasta } in case readme pasta of
    []              -> Right ((), curly 0 pasta')
    ('{', _):_      -> Right ((), pasta')
    (_, (_, col)):_ -> Right ((), curly col pasta')
  pure r

conOf (Constr s _) = s
specialCase (h:_) = '|':conOf h
mkCase t cs = insert (specialCase cs)
  ( Qual [] $ arr t $ foldr arr (TV "case") $ map (\(Constr _ sts) -> foldr arr (TV "case") $ snd <$> sts) cs
  , E $ Basic "I")
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
    [("==", Pa $ map eqCase cs
    )]
  derive "Show" = addInstance "Show" (mkPreds "Show") t
    [("showsPrec", L "prec" $ Pa $ map showCase cs
    )]
  derive der = error $ "bad deriving: " ++ der
  prec0 = A (V "ord") (E $ ChrCon '\0')
  showCase (Constr con args) = let as = show <$> [1..length args]
    in ([PatCon con $ mkPatVar "" <$> as], case args of
      [] -> A (V "++") (E $ StrCon con)
      _ -> case con of
        ':':_ -> A (A (V "showParen") $ V "True") $ foldr1
          (\f g -> A (A (V ".") f) g)
          [ A (A (V "showsPrec") prec0) (V "1")
          , A (V "++") (E $ StrCon $ ' ':con++" ")
          , A (A (V "showsPrec") prec0) (V "2")
          ]
        _ -> A (A (V "showParen") $ A (A (V "<=") prec0) $ V "prec")
          $ A (A (V ".") $ A (V "++") (E $ StrCon con))
          $ foldr (\f g -> A (A (V ".") f) g) (L "x" $ V "x")
          $ map (\a -> A (A (V ".") (A (V ":") (E $ ChrCon ' '))) $ A (A (V "showsPrec") prec0) (V a)) as
      )
  mkPreds classId = Pred classId . TV <$> typeVars t
  mkPatVar pre s = PatVar (pre ++ s) Nothing
  eqCase (Constr con args) = let as = show <$> [1..length args]
    in ([PatCon con $ mkPatVar "l" <$> as], Pa
      [ ([PatCon con $ mkPatVar "r" <$> as], foldr (\x y -> (A (A (V "&&") x) y)) (V "True")
         $ map (\n -> A (A (V "==") (V $ "l" ++ n)) (V $ "r" ++ n)) as)
      , ([PatVar "_" Nothing], V "False")])

addClass classId v (sigs, defs) neat = if not $ member classId $ typeclasses neat then neat
  { typeclasses = insert classId (keys sigs) $ typeclasses neat
  , typedAsts = selectors $ typedAsts neat
  , topDefs = defaults ++ topDefs neat
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
  { typedAsts = insertWith (error $ "import conflict: " ++ ourname) ourname (Qual [] t, mkFFIHelper 0 t $ A (E $ Basic "F") $ A (E $ Basic "NUM") $ E $ Link "{foreign}" foreignname $ Qual [] t) $ typedAsts neat
  , ffiImports = insertWith (error $ "duplicate import: " ++ foreignname) foreignname t $ ffiImports neat
  }
addForeignExport e f neat = neat { ffiExports = insertWith (error $ "duplicate export: " ++ e) e f $ ffiExports neat }
addDefs ds neat = neat { topDefs = ds ++ topDefs neat }
addImport im neat = neat { moduleImports = im:moduleImports neat }
addFixities os prec neat = neat { opFixity = foldr (\o tab -> insert o prec tab) (opFixity neat) os }

parseErrorRule = Parser \pasta -> case indents pasta of
  m:ms | m /= 0 -> Right ('}', pasta { indents = ms })
  _ -> badpos pasta "missing }"

res w@(h:_) = reservedSeq *> pure w <|> bad ("want \"" ++ w ++ "\"") where
  reservedSeq = if elem w ["let", "where", "do", "of"]
    then curlyCheck $ lexeme $ charSeq w *> notFollowedBy nameTailChar
    else lexeme $ charSeq w *> notFollowedBy (if isSmall h then nameTailChar else sat isSymbol)

paren = between lParen rParen
braceSep f = between lBrace (rBrace <|> parseErrorRule) $ foldr ($) [] <$> sepBy ((:) <$> f <|> pure id) semicolon

joinIsFail t = A (L "join#" t) (V "fail#")

addLets ls x = L "let" $ foldr L (L "in" bodies) $ fst <$> ls where
  bodies = foldr A x $ joinIsFail . snd <$> ls

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
  os <- sepBy op comma
  pure $ addFixities os (n, a)

fixity = fixityDecl "infix" NAssoc <|> fixityDecl "infixl" LAssoc <|> fixityDecl "infixr" RAssoc

cDecls = first fromList . second fromList . foldr ($) ([], []) <$> braceSep cDecl
cDecl = first . (:) <$> genDecl <|> second . (++) <$> defSemi

genDecl = (,) <$> var <* res "::" <*> (Qual <$> (scontext <* res "=>" <|> pure []) <*> _type)

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

alts = joinIsFail . Pa <$> braceSep ((\x y -> ([x], y)) <$> pat <*> gdSep "->")
cas = flip A <$> between (res "case") (res "of") expr <*> alts
lamCase = curlyCheck (res "case") *> alts

nalts = joinIsFail . Pa <$> braceSep ((,) <$> many apat <*> gdSep "->")
lamCases = curlyCheck (res "cases") *> nalts

lam = res "\\" *> (lamCase <|> lamCases <|> liftA2 onePat (some apat) (res "->" *> expr))

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

fbind = A <$> (V <$> var) <*> (res "=" *> expr)

fBinds v = (do
    fbs <- between lBrace rBrace $ sepBy1 fbind comma
    pure $ A (E $ Basic "{=") $ foldr A (E $ Basic "=}") $ v:fbs
  ) <|> pure v

atom = ifthenelse <|> doblock <|> letin <|> sqExpr <|> section
  <|> cas <|> lam <|> (paren comma *> pure (V ","))
  <|> V <$> (con <|> var) <|> literal
  >>= fBinds

aexp = foldl1 A <$> some atom

chain a = \case
  [] -> a
  A f b:rest -> case rest of
    [] -> A (A f a) b
    _ -> A (E $ Basic "{+") $ A (A (A f a) b) $ foldr A (E $ Basic "+}") rest
  _ -> error "unreachable"
expr = chain <$> aexp <*> many (A <$> (V <$> op) <*> aexp)

gcon = conId <|> paren (qconsym <|> (:"") <$> comma) <|> lSquare *> rSquare *> pure "[]"

apat = PatVar <$> var <*> (res "@" *> (Just <$> apat) <|> pure Nothing)
  <|> flip PatVar Nothing <$> (res "_" *> pure "_")
  <|> flip PatCon [] <$> gcon
  <|> PatLit <$> literal
  <|> foldr (\h t -> PatCon ":" [h, t]) (PatCon "[]" [])
    <$> between lSquare rSquare (sepBy pat comma)
  <|> paren (foldr1 pairPat <$> sepBy1 pat comma <|> pure (PatCon "()" []))
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

gdSep s = maybeWhere $ res s *> expr <|> foldr ($) (V "join#") <$> some (between (res "|") (res s) guards <*> expr)
guards = foldr1 (\f g -> \yes no -> f (g yes no) no) <$> sepBy1 guard comma
guard = guardPat <$> pat <*> (res "<-" *> expr) <|> guardExpr <$> expr
  <|> guardLets <$> (res "let" *> braceDef)
guardExpr x yes no = case x of
  V "True" -> yes
  _ -> A (A (A (V "if") x) yes) no
guardPat p x yes no = A (Pa [([p], yes), ([PatVar "_" Nothing], no)]) x
guardLets defs yes no = addLets defs yes

onePat vs x = joinIsFail $ Pa [(vs, x)]
defOnePat vs x = Pa [(vs, x)]
opDef x f y rhs = [(f, defOnePat [x, y] rhs)]
leftyPat p expr = case pvars of
  [] -> []
  (h:t) -> let gen = '@':h in
    (gen, expr):map (\v -> (v, A (Pa [([p], V v)]) $ V gen)) pvars
  where
  pvars = filter (/= "_") $ patVars p
def = liftA2 (\l r -> [(l, r)]) var (liftA2 defOnePat (many apat) $ gdSep "=")
  <|> (pat >>= \x -> opDef x <$> varSym <*> pat <*> gdSep "=" <|> leftyPat x <$> gdSep "=")
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
fieldDecl = (\vs t -> map (, t) vs) <$> sepBy1 var comma <*> (res "::" *> _type)
constr = (\x c y -> Constr c [("", x), ("", y)]) <$> bType <*> conop <*> bType
  <|> Constr <$> conId <*>
    (   concat <$> between lBrace rBrace (fieldDecl `sepBy` comma)
    <|> map ("",) <$> many aType)
dclass = conId
_deriving = (res "deriving" *> ((:[]) <$> dclass <|> paren (dclass `sepBy` comma))) <|> pure []
adt = addAdt <$> between (res "data") (res "=") (simpleType <$> conId <*> many varId) <*> sepBy constr (res "|") <*> _deriving

impDecl = addImport <$> (res "import" *> conId)

topdecls = braceSep
  $   adt
  <|> classDecl
  <|> addTopDecl <$> genDecl
  <|> instDecl
  <|> res "foreign" *>
    (   res "import" *> var *> (addForeignImport <$> lexeme tokStr <*> var <*> (res "::" *> _type))
    <|> res "export" *> var *> (addForeignExport <$> lexeme tokStr <*> var)
    )
  <|> addDefs <$> defSemi
  <|> fixity
  <|> impDecl

export_ = ExportVar <$> varId <|> ExportCon <$> conId <*>
  (   paren ((:[]) <$> res ".." <|> sepBy (var <|> con) comma)
  <|> pure []
  )
exports = Just <$> paren (export_ `sepBy` comma)
  <|> pure Nothing

haskell = between lexemePrelude eof $ some do
  (moduleName, exs) <- mayModule
  (moduleName,) . (exs,) <$> topdecls

mayModule = res "module" *> ((,) <$> conId <*> exports <* res "where")
  <|> pure ("Main", Nothing)

parseProgram s = fmap fst $ parse haskell s
