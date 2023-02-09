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
#include "mutually.hs"
