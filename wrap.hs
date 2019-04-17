{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
import Prelude ((+), (-), (*), Char, Int, String, succ, Show)
import Data.Char (ord)
import qualified Prelude
a <= b = if a Prelude.<= b then True else False
a == b = if a Prelude.== b then True else False
#include "fixity"
