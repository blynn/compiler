-- GHC-compatible version. Ours is in a file with prefix "true."
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
#define ghc_compatilibity_thing import Text.RawString.QQ --
#include "true.RTS.hs"
