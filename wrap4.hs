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
import Prelude (Bool(..), Char, Int, String, Show, IO)
import Data.Char (chr, ord)
import qualified Prelude
import qualified Data.Map as Map
import qualified Data.Word as Word
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
import Text.RawString.QQ

default (Int)
_to64 :: Int -> Int -> Word.Word
_to64 a b = Prelude.fromIntegral a Prelude.+ Prelude.fromIntegral b Prelude.* (2 :: Word.Word)  Prelude.^ 32
_lohi :: Word.Word -> (Int, Int)
_lohi w = (Prelude.fromIntegral r, Prelude.fromIntegral q)
  where (q, r) = w `Prelude.divMod` (2 Prelude.^ 32)

word64Add a b c d = _lohi $ _to64 a b Prelude.+ _to64 c d
word64Sub a b c d = _lohi $ _to64 a b Prelude.- _to64 c d
word64Mul a b c d = _lohi $ _to64 a b Prelude.* _to64 c d
intAdd :: Int -> Int -> Int
intAdd = (Prelude.+)
intSub :: Int -> Int -> Int
intSub = (Prelude.-)
intMul :: Int -> Int -> Int
intMul = (Prelude.*)
intEq :: Int -> Int -> Bool
intEq = (Prelude.==)
intLE :: Int -> Int -> Bool
intLE = (Prelude.<=)
uintLE :: Int -> Int -> Bool
uintLE x y = ((Prelude.fromIntegral x :: Prelude.Word) Prelude.<= (Prelude.fromIntegral y :: Prelude.Word))
charEq :: Char -> Char -> Bool
charEq = (Prelude.==)
charLE :: Char -> Char -> Bool
charLE = (Prelude.<=)
ioPure = Prelude.pure :: a -> IO a
ioBind = (Prelude.>>=) :: IO a -> (a -> IO b) -> IO b
div = Prelude.div
mod = Prelude.mod
#define ffi foreign import ccall
#define export --
#include "crossly.hs"
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
