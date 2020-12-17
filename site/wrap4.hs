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
-- GHC wrapper for "methodically" and friends.
-- $ cc -c stub.c
-- $ ghci wrap4.hs stub.o
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
import Prelude (Bool(..), Char, Int, Word, String, IO)
import Data.Char (chr, ord)
import qualified Prelude
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
import Text.RawString.QQ

default (Int)
_to64 :: Word -> Word -> Word
_to64 a b = Prelude.fromIntegral a Prelude.+ Prelude.fromIntegral b Prelude.* (2 :: Word)  Prelude.^ 32
_lohi :: Word -> (Word, Word)
_lohi w = (Prelude.fromIntegral r, Prelude.fromIntegral q)
  where (q, r) = w `Prelude.divMod` (2 Prelude.^ 32)

intFromWord :: Word -> Int
intFromWord = Prelude.fromIntegral

word64Add a b c d = _lohi $ _to64 a b Prelude.+ _to64 c d
word64Sub a b c d = _lohi $ _to64 a b Prelude.- _to64 c d
word64Mul a b c d = _lohi $ _to64 a b Prelude.* _to64 c d
word64Div a b c d = _lohi $ _to64 a b `Prelude.div` _to64 c d
word64Mod a b c d = _lohi $ _to64 a b `Prelude.mod` _to64 c d

intAdd :: Int -> Int -> Int
intAdd = (Prelude.+)
intSub :: Int -> Int -> Int
intSub = (Prelude.-)
intMul :: Int -> Int -> Int
intMul = (Prelude.*)
intDiv :: Int -> Int -> Int
intDiv = Prelude.div
intMod :: Int -> Int -> Int
intMod = Prelude.mod
intQuot :: Int -> Int -> Int
intQuot = Prelude.quot
intRem :: Int -> Int -> Int
intRem = Prelude.rem
intEq :: Int -> Int -> Bool
intEq = (Prelude.==)
intLE :: Int -> Int -> Bool
intLE = (Prelude.<=)

wordAdd :: Word -> Word -> Word
wordAdd = (Prelude.+)
wordSub :: Word -> Word -> Word
wordSub = (Prelude.-)
wordMul :: Word -> Word -> Word
wordMul = (Prelude.*)
wordDiv :: Word -> Word -> Word
wordDiv = Prelude.div
wordMod :: Word -> Word -> Word
wordMod = Prelude.mod
wordQuot :: Word -> Word -> Word
wordQuot = Prelude.quot
wordRem :: Word -> Word -> Word
wordRem = Prelude.rem
wordEq :: Word -> Word -> Bool
wordEq = (Prelude.==)
wordLE :: Word -> Word -> Bool
wordLE = (Prelude.<=)
wordFromInt :: Int -> Word
wordFromInt = Prelude.fromIntegral

charEq :: Char -> Char -> Bool
charEq = (Prelude.==)
charLE :: Char -> Char -> Bool
charLE = (Prelude.<=)
ioPure = Prelude.pure :: a -> IO a
ioBind = (Prelude.>>=) :: IO a -> (a -> IO b) -> IO b
#define ffi foreign import ccall
#define export --
#include "../crossly.hs"
instance Prelude.Functor Parser where fmap = fmap
instance Prelude.Applicative Parser where pure = pure ; (<*>) = (<*>)
instance Prelude.Monad Parser where return = return ; (>>=) = (>>=)
instance Prelude.Functor (Either a) where fmap = fmap
instance Prelude.Applicative (Either a) where pure = pure ; (<*>) = (<*>)
instance Prelude.Monad (Either a) where return = return ; (>>=) = (>>=)

-- instance Prelude.Show Pred where showsPrec _ = showPred
-- instance Prelude.Show Type where showsPrec _ = showType
-- instance Prelude.Show Ast where showsPrec _ = showAst False
-- instance Prelude.Show Qual where showsPrec _ = showQual
