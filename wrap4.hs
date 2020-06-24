-- GHC wrapper for "methodically" and friends.
-- $ cc -c stub.c
-- $ ghci wrap4.hs stub.o
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
#include "methodically.hs"
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
