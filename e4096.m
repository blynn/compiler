edigits = "2." ++ convert (repeat 1)
convert x = mkdigit (hd x'):convert (tl x')
            where x' = norm 2 (0:map (10*) x)
mkdigit n = decode(n + code '0'), if n<10
norm c (d:e:x) = d + e div c: e' mod c : x', if e mod c + 9 < c
               = d + e' div c : e' mod c : x', otherwise
                 where
                 (e':x') = norm (c+1) (e:x)
main = take 4096 edigits
