{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Prelude (Char, Int, String, succ, Show)
import Data.Char (chr, ord)
import qualified Prelude
import qualified Data.Map as Map
import Debug.Trace
a <= b = if a Prelude.<= b then True else False
(*) = (Prelude.*) :: Int -> Int -> Int
(+) = (Prelude.+) :: Int -> Int -> Int
(-) = (Prelude.-) :: Int -> Int -> Int
(/) = Prelude.div
(%) = Prelude.mod
instance Eq Char where { (==) x y = if (x Prelude.== y) then True else False };
intEq :: Int -> Int -> Bool
intEq x y = if (x Prelude.== y) then True else False
#include "disassembly.hs"
