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
infixr 9 .;
infixr 0 $;
($) f x = f x;
(.) f g x = f (g x);

data Maybe a = Nothing | Just a;
maybe n j m = case m of { Nothing -> n; Just x -> j x };

data Bool = True | False;
ife a b c = case a of { True -> b ; False -> c };
(&&) f g = ife f g False;

foldr c n l = case l of { [] -> n; (:) h t -> c h $ foldr c n t };
any f xs = foldr (\x t -> ife (f x) True t) False xs;
not a = case a of { True -> False; False -> True };

data Nat = Zero | Succ Nat;
foldn f n m = case m of
  { Zero -> n
  ; Succ m' -> f $ foldn f n m'
  };
add = foldn Succ;
mul a = foldn (add a) Zero;
pre n = case n of { Zero -> Nothing ; Succ n' -> Just n' };
sub a b = foldn (maybe Nothing pre) (Just a) b;

subdiv n q = case n of
  { Zero -> True
  ; Succ n' -> case sub n q of
    { Nothing -> False
    ; Just m  -> subdiv m q
    }
  };

twoUp f q = case q of
  { Zero -> False
  ; Succ q' -> case q' of
    { Zero -> False
    ; Succ _ -> f q
    }
  };

downer n = case n of
  { Zero -> []
  ; Succ n' -> n' : downer n'
  };

isPrime n = twoUp (not . any (twoUp (subdiv n)) . downer) n;
psum n p = isPrime p && maybe False isPrime (sub n p);
pp n = any (psum n) $ downer n;
goldbach' n = pp n && goldbach' (Succ $ Succ n);
goldbach = goldbach' $ Succ $ Succ $ Succ $ Succ Zero;
