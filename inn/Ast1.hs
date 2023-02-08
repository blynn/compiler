-- Record fields.
-- Remove `overFreePro`.
module Ast where
import Base
import Map

data Type = TC String | TV String | TAp Type Type
arr a b = TAp (TAp (TC "->") a) b
data Extra = Basic String | Const Int | ChrCon Char | StrCon String | Link String String Qual
data Pat = PatLit Extra | PatVar String (Maybe Pat) | PatCon String [Pat]
data Ast = E Extra | V String | A Ast Ast | L String Ast | Pa [([Pat], Ast)] | Proof Pred
data Constr = Constr String [(String, Type)]
data Pred = Pred String Type
data Qual = Qual [Pred] Type

instance Eq Type where
  (TC s) == (TC t) = s == t
  (TV s) == (TV t) = s == t
  (TAp a b) == (TAp c d) = a == c && b == d
  _ == _ = False

instance Eq Pred where (Pred s a) == (Pred t b) = s == t && a == b

data Instance = Instance
  -- Type, e.g. Int for Eq Int.
  Type
  -- Dictionary name, e.g. "{Eq Int}"
  String
  -- Context.
  [Pred]
  -- Method definitions
  (Map String Ast)

data Tycl = Tycl [String] [Instance]

data Neat = Neat
  (Map String Tycl)
  -- | Top-level definitions
  [(String, Ast)]
  -- | Typed ASTs, ready for compilation, including ADTs and methods,
  -- e.g. (==), (Eq a => a -> a -> Bool, select-==)
  [(String, (Qual, Ast))]
  -- | Data constructor table.
  (Map String [Constr])
  -- | FFI declarations.
  [(String, Type)]
  -- | Exports.
  [(String, String)]
  -- | Module imports.
  [String]

patVars = \case
  PatLit _ -> []
  PatVar s m -> s : maybe [] patVars m
  PatCon _ args -> concat $ patVars <$> args

fvPro bound expr = case expr of
  V s | not (elem s bound) -> [s]
  A x y -> fvPro bound x `union` fvPro bound y
  L s t -> fvPro (s:bound) t
  Pa vsts -> foldr union [] $ map (\(vs, t) -> fvPro (concatMap patVars vs ++ bound) t) vsts
  _ -> []

beta s a t = case t of
  E _ -> t
  V v -> if s == v then a else t
  A x y -> A (beta s a x) (beta s a y)
  L v u -> if s == v then t else L v $ beta s a u

showInt' n = if 0 == n then id else (showInt' $ n`div`10) . ((:) (chr $ 48+n`mod`10))
showInt n = if 0 == n then ('0':) else showInt' n
par = showParen True
showType t = case t of
  TC s -> (s++)
  TV s -> (s++)
  TAp (TAp (TC "->") a) b -> par $ showType a . (" -> "++) . showType b
  TAp a b -> par $ showType a . (' ':) . showType b
showPred (Pred s t) = (s++) . (' ':) . showType t . (" => "++)

showQual (Qual ps t) = foldr (.) id (map showPred ps) . showType t

showVar s@(h:_) = showParen (elem h ":!#$%&*+./<=>?@\\^|-~") (s++)

showExtra = \case
  Basic s -> (s++)
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
  A x y -> showParen prec $ showAst False x . (' ':) . showAst True y
  L s t -> par $ ('\\':) . (s++) . (" -> "++) . showAst prec t
  Pa vsts -> ('\\':) . par (foldr (.) id $ intersperse (';':) $ map (\(vs, t) -> foldr (.) id (intersperse (' ':) $ map (par . showPat) vs) . (" -> "++) . showAst False t) vsts)
  Proof p -> ("{Proof "++) . showPred p . ("}"++)

typedAsts (Neat _ _ tas _ _ _ _) = tas
typeclasses (Neat tcs _ _ _ _ _ _) = tcs
dataCons (Neat _ _ _ dcs _ _ _) = dcs

typeVars = \case
  TC _ -> []
  TV v -> [v]
  TAp x y -> typeVars x `union` typeVars y

depthFirstSearch = (foldl .) \relation st@(visited, sequence) vertex ->
  if vertex `elem` visited then st else second (vertex:)
    $ depthFirstSearch relation (vertex:visited, sequence) (relation vertex)

spanningSearch   = (foldl .) \relation st@(visited, setSequence) vertex ->
  if vertex `elem` visited then st else second ((:setSequence) . (vertex:))
    $ depthFirstSearch relation (vertex:visited, []) (relation vertex)

scc ins outs = spanning . depthFirst where
  depthFirst = snd . depthFirstSearch outs ([], [])
  spanning   = snd . spanningSearch   ins  ([], [])

encodeCase x alts = A (E $ Basic "case") $ A x $ Pa $ first (:[]) <$> alts
decodeCaseArg (A x (Pa pas)) = (x, first (\(h:_) -> h) <$> pas)
