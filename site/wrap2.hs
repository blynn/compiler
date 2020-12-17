-- Copyright © 2019 Ben Lynn
-- This file is part of blynn-compiler.

-- blynn-compiler is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, only under version 3 of
-- the License.

-- blynn-compiler is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with blynn-compiler.  If not, see
-- <https://www.gnu.org/licenses/>.
-- GHC wrapper for "barely" and friends.
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Prelude (Char, Int, String, succ, Show, IO)
import Data.Char (chr, ord)
import qualified Prelude
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
(*) = (Prelude.*) :: Int -> Int -> Int
(+) = (Prelude.+) :: Int -> Int -> Int
(-) = (Prelude.-) :: Int -> Int -> Int
(/) = Prelude.div
(%) = Prelude.mod
instance Eq Char where (==) x y = if x Prelude.== y then True else False
instance Ord Char where (<=) x y = if x Prelude.<= y then True else False
intEq :: Int -> Int -> Bool
intEq x y = if x Prelude.== y then True else False
intLE :: Int -> Int -> Bool
intLE x y = if x Prelude.<= y then True else False
ioPure = Prelude.pure :: a -> IO a
ioBind = (Prelude.>>=) :: IO a -> (a -> IO b) -> IO b
#define ffi foreign import ccall
#include "../patty.hs"
