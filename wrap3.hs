-- GHC wrapper for "guardedly" and friends.
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Prelude (Bool(..), Char, Int, String, succ, Show, IO)
import Data.Char (chr, ord)
import qualified Prelude
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitSuccess)
import Debug.Trace
(*) = (Prelude.*) :: Int -> Int -> Int
(+) = (Prelude.+) :: Int -> Int -> Int
(-) = (Prelude.-) :: Int -> Int -> Int
(/) = Prelude.div
(%) = Prelude.mod
instance Eq Char where (==) x y = x Prelude.== y
instance Ord Char where (<=) x y = x Prelude.<= y
intEq :: Int -> Int -> Bool
intEq x y = x Prelude.== y
intLE :: Int -> Int -> Bool
intLE x y = x Prelude.<= y
ioPure = Prelude.pure :: a -> IO a
ioBind = (Prelude.>>=) :: IO a -> (a -> IO b) -> IO b
#define ffi foreign import ccall
#include "assembly.hs"
