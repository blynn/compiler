-- Replace list with map.
module Unify where

import Base
import Map
import Ast

apply sub t = case t of
  TC v -> t
  TV v -> maybe t id $ mlookup v sub
  TAp a b -> TAp (apply sub a) (apply sub b)

(@@) s1 s2 = foldr (\(k, v) m -> insert k v m) (apply s1 <$> s2) $ toAscList s1

occurs s t = case t of
  TC v -> False
  TV v -> s == v
  TAp a b -> occurs s a || occurs s b

varBind s t = case t of
  TC v -> Right $ singleton s t
  TV v -> Right $ if v == s then Tip else singleton s t
  TAp a b -> if occurs s t then Left "occurs check" else Right $ singleton s t

ufail t u = Left $ ("unify fail: "++) . shows t . (" vs "++) . shows u $ ""

mgu t u = case t of
  TC a -> case u of
    TC b -> if a == b then Right Tip else ufail t u
    TV b -> varBind b t
    TAp a b -> ufail t u
  TV a -> varBind a u
  TAp a b -> case u of
    TC b -> ufail t u
    TV b -> varBind b t
    TAp c d -> mgu a c >>= unify b d

unify a b s = (@@ s) <$> mgu (apply s a) (apply s b)

merge s1 s2 = foldM go s2 $ toAscList s1 where
  go subs (v, t) = case mlookup v s2 of
    Nothing -> Just $ insert v t subs
    Just _  | apply s1 (TV v) == apply s2 (TV v) -> Just subs
            | True -> Nothing

match h t = case h of
  TC a -> case t of
    TC b | a == b -> Just Tip
    _ -> Nothing
  TV a -> Just $ singleton a t
  TAp a b -> case t of
    TAp c d -> case match a c of
      Nothing -> Nothing
      Just ac -> case match b d of
        Nothing -> Nothing
        Just bd -> merge ac bd
    _ -> Nothing
