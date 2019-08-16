infixr 9 .;
infixr 0 $;
($) f x = f x;
(.) f g x = f (g x);

data Maybe a = Nothing | Just a;
maybe n j m = case m of { Nothing -> n; Just x -> j x };

data Bool = True | False;
ife a b c = case a of { True -> b ; False -> c };
(&&) f g = ife f (ife g True False) False;

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
