module Lexer where
import Base
import Ast

data LexState = LexState String (Int, Int)
data Lexer a = Lexer (LexState -> Either String (a, LexState))
instance Functor Lexer where fmap f (Lexer x) = Lexer $ fmap (first f) . x
instance Applicative Lexer where
  pure x = Lexer \inp -> Right (x, inp)
  f <*> x = Lexer \inp -> case lex f inp of
    Left e -> Left e
    Right (fun, t) -> case lex x t of
      Left e -> Left e
      Right (arg, u) -> Right (fun arg, u)
instance Monad Lexer where
  return = pure
  x >>= f = Lexer \inp -> case lex x inp of
    Left e -> Left e
    Right (a, t) -> lex (f a) t
instance Alternative Lexer where
  empty = Lexer \_ -> Left ""
  (<|>) x y = Lexer \inp -> either (const $ lex y inp) Right $ lex x inp

lex (Lexer f) inp = f inp
advanceRC x (r, c)
  | n `elem` [10, 11, 12, 13] = (r + 1, 1)
  | n == 9 = (r, (c + 8)`mod`8)
  | True = (r, c + 1)
  where n = ord x
pos = Lexer \inp@(LexState _ rc) -> Right (rc, inp)
sat f = Lexer \(LexState inp rc) -> flst inp (Left "EOF") \h t ->
  if f h then Right (h, LexState t $ advanceRC h rc) else Left "unsat"
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

escape = char '\\' *> (sat (`elem` "'\"\\") <|> char 'n' *> pure '\n')
tokOne delim = escape <|> sat (delim /=)

tokChar = between (char '\'') (char '\'') (tokOne '\'')
tokStr = between (char '"') (char '"') $ many (tokOne '"')
integer = char '0' *> (char 'x' <|> char 'X') *> hexadecimal <|> decimal
literal = Lit . Const <$> integer <|> Lit . ChrCon <$> tokChar <|> Lit . StrCon <$> tokStr
varId = fmap ck $ liftA2 (:) small $ many (small <|> large <|> digit <|> char '\'') where
  ck s = (if elem s
    ["ffi", "export", "case", "class", "data", "default", "deriving", "do", "else", "foreign", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_"]
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
