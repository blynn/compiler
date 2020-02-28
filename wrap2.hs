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
#include "patty.hs"
