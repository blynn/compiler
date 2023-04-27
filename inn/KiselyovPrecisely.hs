module Kiselyov where
import Base
import Ast

-- Conversion to De Bruijn indices.
data LC = Ze | Su LC | Pass IntTree | La LC | App LC LC

debruijn n e = case e of
  E x -> Pass $ case x of
    Basic c -> Lf c
    _ -> LfExtra x
  V v -> maybe (Pass $ LfExtra $ Link "" v) id $
    foldr (\h found -> if h == v then Just Ze else Su <$> found) Nothing n
  A x y -> App (debruijn n x) (debruijn n y)
  L s t -> La (debruijn (s:n) t)

-- Kiselyov bracket abstraction.
data IntTree = Lf String | Nd IntTree IntTree | LfExtra Extra
data Sem = Closed IntTree | Need Sem | Weak Sem

instance Show IntTree where
  showsPrec prec = \case
    Lf s -> (s++)
    LfExtra extra -> shows extra
    Nd x y -> showParen (1 <= prec) $ showsPrec 0 x . (' ':) . showsPrec 1 y

x ## y = case x of
  Closed d -> case y of
    Closed dd -> Closed $ Nd d dd
    Need e -> case e of
      Closed (Lf "I") -> Need x
      _ -> Need $ Closed (Nd (Lf "B") d) ## e
    Weak e -> Weak $ Closed d ## e
  Need e -> case e of
    Closed (Lf "I") -> case y of
      Closed d -> Need $ Closed $ Nd (Lf "T") d
      Weak ee -> Need $ Closed (Lf "T") ## ee
      Need ee -> Need $ Closed (Lf "S") ## Closed (Lf "I") ## ee
    _ -> case y of
      Closed d -> Need $ Closed (Nd (Lf "R") d) ## e
      Need ee -> Need $ Closed (Lf "S") ## e ## ee
      Weak ee -> Need $ Closed (Lf "C") ## e ## ee
  Weak e -> case y of
    Closed d -> Weak $ e ## Closed d
    Need ee -> case ee of
      Closed (Lf "I") -> Need e
      _ -> Need $ Closed (Lf "B") ## e ## ee
    Weak ee -> Weak $ e ## ee

babs t = case t of
  Ze -> Need $ Closed $ Lf "I"
  Su x -> Weak $ babs x
  Pass x -> Closed x
  La t -> case babs t of
    Closed d -> Closed $ Nd (Lf "K") d
    Need e -> e
    Weak e -> Closed (Lf "K") ## e
  App x y -> babs x ## babs y

nolam x = (\(Closed d) -> d) $ babs $ debruijn [] x

-- Optimizations.
optim t = case t of
  Nd x y -> go (optim x) (optim y)
  _ -> t
  where
  go (Lf "I") q = q
  go p q@(Lf c) = case c of
    "K" | Lf "B" <- p -> Lf "BK"
    "I" -> case p of
      Lf r -> case r of
        "C" -> Lf "T"
        "B" -> Lf "I"
        "K" -> Lf "KI"
        _ -> Nd p q
      Nd p1 p2 -> case p1 of
        Lf "B" -> p2
        Lf "R" -> Nd (Lf "T") p2
        _ -> Nd p q
      _ -> Nd p q
    "T" | Nd (Lf "B") (Lf r) <- p -> case r of
      "C" -> Lf "V"
      "BK" -> Lf "LEFT"
      _ -> Nd p q
    "V" | Nd (Lf "B") (Lf "BK") <- p -> Lf "CONS"
    _ -> Nd p q
  go p q = Nd p q
