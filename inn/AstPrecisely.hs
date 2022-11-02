-- Integer.
module Ast where
import Base
import Map

data Type = TC String | TV String | TAp Type Type deriving Eq
arr a b = TAp (TAp (TC "->") a) b
data Extra = Basic String | Const Integer | ChrCon Char | StrCon String | Link String String | XQual Qual
data Pat = PatLit Extra | PatVar String (Maybe Pat) | PatCon String [Pat]
data Ast = E Extra | V String | A Ast Ast | L String Ast | Pa [([Pat], Ast)] | Proof Pred
data Constr = Constr String [(String, Type)]
data ModExport = ExportVar String | ExportCon String [String]
data Pred = Pred String Type deriving Eq
data Qual = Qual [Pred] Type

instance Show Type where
  showsPrec p = \case
    TC s -> (s++)
    TV s -> (s++)
    TAp (TAp (TC "->") a) b -> showParen (p >= 1) $ showsPrec 1 a . (" -> "++) . shows b
    TAp (TAp (TC ",") a) b -> showParen True $ shows a . (", "++) . shows b
    TAp (TC "[]") a -> ('[':) . shows a . (']':)
    TAp a b -> showParen (p >= 2) $ shows a . (' ':) . showsPrec 2 b
instance Show Pred where
  showsPrec _ (Pred s t) = (s++) . (' ':) . shows t . (" => "++)
instance Show Qual where
  showsPrec _ (Qual ps t) = foldr (.) id (map shows ps) . shows t
instance Show Extra where
  showsPrec _ = \case
    Basic s -> (s++)
    Const i -> shows i
    ChrCon c -> shows c
    StrCon s -> shows s
    Link im s -> (im++) . ('.':) . (s++)
    XQual q -> shows q
instance Show Pat where
  showsPrec _ = \case
    PatLit e -> shows e
    PatVar s mp -> (s++) . maybe id ((('@':) .) . shows) mp
    PatCon s ps -> (s++) . foldr (.) id (((' ':) .) . shows <$> ps)

showVar s@(h:_) = showParen (elem h ":!#$%&*+./<=>?@\\^|-~") (s++)

instance Show Ast where
  showsPrec prec = \case
    E e -> shows e
    V s -> showVar s
    A x y -> showParen (1 <= prec) $ shows x . (' ':) . showsPrec 1 y
    L s t -> showParen True $ ('\\':) . (s++) . (" -> "++) . shows t
    Pa vsts -> ('\\':) . showParen True (foldr (.) id $ intersperse (';':) $ map (\(vs, t) -> foldr (.) id (intersperse (' ':) $ map (showParen True . shows) vs) . (" -> "++) . shows t) vsts)
    Proof p -> ("{Proof "++) . shows p . ("}"++)

showType = shows  -- for Unify.

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

data Assoc = NAssoc | LAssoc | RAssoc deriving Eq

data Neat = Neat
  { typeclasses :: Map String Tycl
  , topDefs :: [(String, Ast)]
  , topDecls :: Map String Qual
  -- | Typed ASTs, ready for compilation, including ADTs and methods,
  -- e.g. (==), (Eq a => a -> a -> Bool, select-==)
  , typedAsts :: Map String (Qual, Ast)
  , dataCons :: Map String (Qual, [Constr])
  , type2Cons :: Map String [String]
  , ffiImports :: Map String Type
  , ffiExports :: Map String String
  , moduleImports :: Map String [(String, String -> Bool)]
  , moduleExports :: Maybe [String]
  , opFixity :: Map String (Int, Assoc)
  , typeAliases :: Map String Type
  }

neatEmpty = Neat Tip [] Tip Tip Tip Tip Tip Tip (singleton "" []) Nothing Tip Tip

patVars = \case
  PatLit _ -> []
  PatVar s m -> s : maybe [] patVars m
  PatCon _ args -> concat $ patVars <$> args

beta s a t = case t of
  E _ -> t
  V v -> if s == v then a else t
  A x y -> A (beta s a x) (beta s a y)
  L v u -> if s == v then t else L v $ beta s a u

typeVars = \case
  TC _ -> []
  TV v -> [v]
  TAp x y -> typeVars x `union` typeVars y

dependentModules neat = map fst $ concat $ elems $ moduleImports neat

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
decodeCaseArg (A x (Pa pas)) = (x, first head <$> pas)
