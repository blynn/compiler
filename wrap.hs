{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
import Prelude ((+), (-), Char, String, succ)
import qualified Prelude
a <= b = if a Prelude.<= b then True else False
a == b = if a Prelude.== b then True else False
#include "parity"
