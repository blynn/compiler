module Map(Map(..), (!), size, singleton, insert, insertWith, mlookup, delete,
  member, assocs, keys, elems, fromList, fromListWith, toAscList, mapWithKey,
  foldrWithKey, mfilter, filterWithKey) where

import Base

infixl 9 !

data Map k a = Tip | Bin Int k a (Map k a) (Map k a)
instance (Show k, Show a) => Show (Map k a) where
  showsPrec n m = ("fromList "++) . showsPrec n (toAscList m)
instance Functor (Map k) where
  fmap f m = case m of
    Tip -> Tip
    Bin sz k x l r -> Bin sz k (f x) (fmap f l) (fmap f r)
instance Ord k => Monoid (Map k a) where
  mempty = Tip
instance Ord k => Semigroup (Map k a) where
  x <> y = foldr (\(k, v) m -> insertWith const k v m) y $ assocs x
instance (Eq k,Eq a) => Eq (Map k a) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)
size m = case m of Tip -> 0 ; Bin sz _ _ _ _ -> sz
bin k x l r = Bin (1 + size l + size r) k x l r
singleton k x = Bin 1 k x Tip Tip

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | sizeL + sizeR <= 1    = Bin sizeX k x l r
  | sizeR >= 4*sizeL  = rotL k x l r
  | sizeL >= 4*sizeR  = rotR k x l r
  | otherwise             = Bin sizeX k x l r
  where
    sizeL = size l
    sizeR = size r
    sizeX = sizeL + sizeR + 1

rotL :: a -> b -> Map a b -> Map a b -> Map a b
rotL k x l r@(Bin _ _ _ ly ry)
  | size ly < 2*size ry = singleL k x l r
  | otherwise               = doubleL k x l r
rotL _ _ _ Tip = error "rotL Tip"

rotR :: a -> b -> Map a b -> Map a b -> Map a b
rotR k x l@(Bin _ _ _ ly ry) r
  | size ry < 2*size ly = singleR k x l r
  | otherwise               = doubleR k x l r
rotR _ _ Tip _ = error "rotR Tip"

singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL _ _ _ Tip = error "singleL Tip"
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR _ _ Tip _ = error "singleR Tip"

doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL _ _ _ _ = error "doubleL"
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR _ _ _ _ = error "doubleR"

insert :: Ord k => k -> a -> Map k a -> Map k a
insert kx x = go
  where
    go Tip = singleton kx x
    go (Bin sz ky y l r) =
        case compare kx ky of
            LT -> balance ky y (go l) r
            GT -> balance ky y l (go r)
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
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) =
  Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)
toAscList = foldrWithKey (\k x xs -> (k,x):xs) []
keys = map fst . toAscList
elems = map snd . toAscList
assocs = toAscList

join :: Ord k => k -> a -> Map k a -> Map k a -> Map k a
join kx x Tip r  = insertMin kx x r
join kx x l Tip  = insertMax kx x l
join kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | 4*sizeL <= sizeR  = balance kz z (join kx x l lz) rz
  | 4*sizeR <= sizeL  = balance ky y ly (join kx x ry r)
  | otherwise             = bin kx x l r

insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balance ky y l (insertMax kx x r)

insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balance ky y (insertMin kx x l) r

merge :: Map k a -> Map k a -> Map k a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | 4*sizeL <= sizeR = balance ky y (merge l ly) ry
  | 4*sizeR <= sizeL = balance kx x lx (merge rx r)
  | otherwise            = glue l r

mfilter :: Ord k => (a -> Bool) -> Map k a -> Map k a
mfilter p m
  = filterWithKey (\_ x -> p x) m

filterWithKey :: Ord k => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey p = go
  where
    go Tip = Tip
    go (Bin _ kx x l r)
          | p kx x    = join kx x (go l) (go r)
          | otherwise = merge (go l) (go r)

glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let ((km,m),l') = deleteFindMax l in balance km m l' r
  | otherwise       = let ((km,m),r') = deleteFindMin r in balance km m l r'

deleteFindMin :: Map k a -> ((k,a),Map k a)
deleteFindMin t
  = case t of
      Bin _ k x Tip r -> ((k,x),r)
      Bin _ k x l r   -> let (km,l') = deleteFindMin l in (km,balance k x l' r)
      Tip             -> (error "Map.deleteFindMin: can not return the minimal element of an empty map", Tip)

deleteFindMax :: Map k a -> ((k,a),Map k a)
deleteFindMax t
  = case t of
      Bin _ k x l Tip -> ((k,x),l)
      Bin _ k x l r   -> let (km,r') = deleteFindMax r in (km,balance k x l r')
      Tip             -> (error "Map.deleteFindMax: can not return the maximal element of an empty map", Tip)
