{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Prelude ((+), (-), (*), Char, Int, String, succ, Show)
import Data.Char (chr, ord)
import qualified Prelude
a <= b = if a Prelude.<= b then True else False
(/) = Prelude.div
(%) = Prelude.mod
class Eq a where { (==) :: a -> a -> Bool };
instance Eq Char where { (==) x y = if (x Prelude.== y) then True else False };
instance Eq Int where { (==) x y = if (x Prelude.== y) then True else False };
#include "typically.hs"
