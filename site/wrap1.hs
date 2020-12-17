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
-- GHC wrapper for "parity" and friends.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Prelude ((+), (-), (*), Char, Int, String, succ, Show, interact)
import Data.Char (chr, ord)
import qualified Prelude
a <= b = if a Prelude.<= b then True else False
(/) = Prelude.div
(%) = Prelude.mod
class Eq a where (==) :: a -> a -> Bool
instance Eq Char where (==) x y = if (x Prelude.== y) then True else False
instance Eq Int where (==) x y = if (x Prelude.== y) then True else False
#include "../parity.hs"
main = interact compile
