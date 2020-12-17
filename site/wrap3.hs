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
-- GHC wrapper for "guardedly" and friends.
-- $ cc -c stub.c
-- $ ghci wrap3.hs stub.o
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
import Prelude (Bool(..), Char, Int, String, Show, IO)
import Data.Char (chr, ord)
import qualified Prelude
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
import Text.RawString.QQ
(*) = (Prelude.*) :: Int -> Int -> Int
(+) = (Prelude.+) :: Int -> Int -> Int
(-) = (Prelude.-) :: Int -> Int -> Int
(/) = Prelude.div
(%) = Prelude.mod
intEq :: Int -> Int -> Bool
intEq = (Prelude.==)
intLE :: Int -> Int -> Bool
intLE = (Prelude.<=)
charEq :: Char -> Char -> Bool
charEq = (Prelude.==)
charLE :: Char -> Char -> Bool
charLE = (Prelude.<=)
ioPure = Prelude.pure :: a -> IO a
ioBind = (Prelude.>>=) :: IO a -> (a -> IO b) -> IO b
div = Prelude.div
mod = Prelude.mod
succ :: Int -> Int
succ = Prelude.succ
#define ffi foreign import ccall
#define export --
#include "../marginally.hs"
