module Map where

import Base

data Map k a = Tip | Bin Int k a (Map k a) (Map k a)
instance (Show k, Show a) => Show (Map k a) where
  showsPrec n m = ("fromList ["++) . showsPrec n (toAscList m) . (']':)
instance Functor (Map k) where
  fmap f m = case m of
    Tip -> Tip
    Bin sz k x l r -> Bin sz k (f x) (fmap f l) (fmap f r)
instance Ord k => Monoid (Map k a) where
  mempty = Tip
  x <> y = foldr (\(k, v) m -> insertWith const k v m) y $ assocs x
size m = case m of Tip -> 0 ; Bin sz _ _ _ _ -> sz
node k x l r = Bin (1 + size l + size r) k x l r
singleton k x = Bin 1 k x Tip Tip
singleL k x l (Bin _ rk rkx rl rr) = node rk rkx (node k x l rl) rr
doubleL k x l (Bin _ rk rkx (Bin _ rlk rlkx rll rlr) rr) =
  node rlk rlkx (node k x l rll) (node rk rkx rlr rr)
singleR k x (Bin _ lk lkx ll lr) r = node lk lkx ll (node k x lr r)
doubleR k x (Bin _ lk lkx ll (Bin _ lrk lrkx lrl lrr)) r =
  node lrk lrkx (node lk lkx ll lrl) (node k x lrr r)
balance k x l r = f k x l r where
  f | size l + size r <= 1 = node
    | 5 * size l + 3 <= 2 * size r = case r of
      Tip -> node
      Bin sz _ _ rl rr -> if 2 * size rl + 1 <= 3 * size rr
        then singleL
        else doubleL
    | 5 * size r + 3 <= 2 * size l = case l of
      Tip -> node
      Bin sz _ _ ll lr -> if 2 * size lr + 1 <= 3 * size ll
        then singleR
        else doubleR
    | True = node
insert kx x t = case t of
  Tip -> singleton kx x
  Bin sz ky y l r -> case compare kx ky of
    LT -> balance ky y (insert kx x l) r
    GT -> balance ky y l (insert kx x r)
    EQ -> Bin sz kx x l r
insertWith f kx x t = case t of
  Tip -> singleton kx x
  Bin sy ky y l r -> case compare kx ky of
    LT -> balance ky y (insertWith f kx x l) r
    GT -> balance ky y l (insertWith f kx x r)
    EQ -> Bin sy kx (f x y) l r
delete = go where
  go _ Tip = Tip
  go k t@(Bin _ kx x l r) = case compare k kx of
    LT -> balance kx x (go k l) r
    GT -> balance kx x l (go k r)
    EQ -> glue l r
  glue Tip r = r
  glue l Tip = l
  glue l@(Bin sl kl xl ll lr) r@(Bin sr kr xr rl rr)
    | sl <= sr = let ((km, m), r') = minViewSure kr xr rl rr in balance km m l r'
    | True = let ((km, m), l') = maxViewSure kl xl ll lr in balance km m l' r
  minViewSure k x Tip r = ((k, x), r)
  minViewSure k x (Bin _ kl xl ll lr) r = case minViewSure kl xl ll lr of
    ((km, xm), l') -> ((km, xm), balance k x l' r)
  maxViewSure k x l Tip = ((k, x), l)
  maxViewSure k x l (Bin _ kr xr rl rr) = case maxViewSure kr xr rl rr of
    ((km, xm), r') -> ((km, xm), balance k x l r')

mlookup kx t = case t of
  Tip -> Nothing
  Bin _ ky y l r -> case compare kx ky of
    LT -> mlookup kx l
    GT -> mlookup kx r
    EQ -> Just y
fromList = foldl (\t (k, x) -> insert k x t) Tip
fromListWith f = foldl (\t (k, x) -> insertWith f k x t) Tip
member k t = maybe False (const True) $ mlookup k t
t ! k = maybe undefined id $ mlookup k t
foldrWithKey f = go where
  go z t = case t of
    Tip -> z
    Bin _ kx x l r -> go (f kx x (go z r)) l
toAscList = foldrWithKey (\k x xs -> (k,x):xs) []
keys = map fst . toAscList
elems = map snd . toAscList
assocs = toAscList
