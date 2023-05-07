= Compilers for contrarians =

link:ioccc.html[An award-winning Haskell compiler], browser edition.

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<textarea id='mod_base' hidden>
include::inn/BasePrecisely.hs[]
</textarea>
<textarea id='mod_map' hidden>
include::inn/Map1.hs[]
</textarea>
<textarea id='mod_system' hidden>
include::inn/SystemWasm.hs[]
</textarea>
<p><span style='cursor:pointer;' onclick='hideshow("pre");'><span id='pre_toggle'>[+] Show</span> modules</span></p>
<div id='pre' style='display:none;'>
<style>#modlist li:hover{text-decoration:underline;}</style>
<ul id='modlist'></ul>
<p>
<textarea spellcheck='false' id='modinp' rows='16' style='box-sizing:border-box;width:100%;'></textarea>
<button onclick="update_module()">Update Module</button>
</p>
</div>

<div id='compiler' style='display:none;'>
<p>
<label for="prog">Program:</label>
<button id="hello">&#127760;</button>
<button id="edigits"><i>e</i></button>
<button id="primes">&#8473;</button>
<button id="queens">&#9819;</button>
<button id="lindon">Lindon</button>
<button id="sort">&#9035;</button>
<button id="hexmaze">&#11042;</button>
<button id="gray">Gray</button>
<button id="hilbert">Hilbert</button>
<button id="douady">Douady</button>
<button id="enigma">Enigma</button>
<button id="sha256">SHA-256</button>
<button id="keccak">Keccak-256</button>
</p>
<p>
<textarea spellcheck='false' rows='12' id="prog" name="prog"
style='box-sizing:border-box;width:100%;'>
</textarea>
</p>
<label for="inp">Input:</label>
<p>
<textarea spellcheck='false' id='inp' rows='2' style='box-sizing:border-box;width:100%;'></textarea>
</p>
<p>
<button onclick="go()">Run</button>
<button onclick="genlink()">Linkify</button>
<span id="msg"></span>
</p>
<label for="out">Output:</label>
<p>
<textarea spellcheck='false' readonly id='out' rows='8' style='box-sizing:border-box;width:100%;'></textarea>
</p>
</div>

<script>
"use strict";
function hideshow(s) {
  const x = document.getElementById(s);
  const xt = document.getElementById(s + "_toggle");
  if (x.style.display === "none") {
    x.style.display = "block";
    xt.innerText = "[-] Hide"
  } else {
    x.style.display = "none";
    xt.innerText = "[+] Show"
  }
}

var blah;
var blahInp, blahInpLen, blahInpCur;
function setInput(s) {
  blahInp = s;
  blahInpLen = s.length;
  blahInpCur = 0;
}
var blahOut;
const modmap = new Map();
modmap.set('Base', document.getElementById('mod_base').value);
modmap.set('Map', document.getElementById('mod_map').value);
modmap.set('System', document.getElementById('mod_system').value);

WebAssembly.instantiateStreaming(fetch('imp.wasm'), { env:
  { getchar: () => {
      if (blahInpCur == blahInpLen) throw "eof";
      blahInpCur++;
      return blahInp.charCodeAt(blahInpCur - 1);
    }
  , putchar: c => blahOut.push(c)
  , eof: () => blahInpCur == blahInpLen
  , get_module: () => {
      const mod = String.fromCharCode.apply(null, blahOut);
      blahOut = [];
      setInput("");
      if (modmap.has(mod)) setInput(modmap.get(mod));
    }
  }}).then(obj => {
    document.getElementById('compiler').style.display = "block";
    blah = obj.instance;
  });

const modlist = document.getElementById('modlist');
const modinp = document.getElementById('modinp');

function populate_modlist() {
  modlist.innerText = "";
  for (const key of modmap.keys()) {
    const x = document.createElement("li");
    const y = document.createTextNode(key);
    x.addEventListener('click', function(){ modinp.value = modmap.get(key); });
    x.appendChild(y);
    modlist.appendChild(x);
  }
}
populate_modlist();

function update_module() {
  setInput(modinp.value);
  blahOut = [];
  blah.exports.single_module();
  const mod = String.fromCharCode.apply(null, blahOut);
  if (mod == "") {
    console.log("module update failed");
  } else {
    modmap.set(mod, blahInp);
    populate_modlist();
  }
}

const msg = document.getElementById("msg");
let lastProg = "";
const program = document.getElementById("prog");
const stdin = document.getElementById("inp");
const stdout = document.getElementById("out");

function compile() {
  msg.innerText = "compiling...";
  stdout.value = "";
  setInput(program.value);
  if (lastProg == blahInp) return new Promise(function(resolve) { resolve() });
  lastProg = blahInp;
  blahOut = [];
  // Timeout so message is displayed. Unreliable.
  return new Promise(function(resolve, reject) {
    setTimeout(function() {
  blah.exports.go();
  if (blahOut[0] != 0) {
    msg.innerText = "compile error: " + String.fromCharCode.apply(null, blahOut);
    reject();
  } else {
    msg.innerText = "";
    resolve();
  }
    }, 0);
  });
}

function run() {
  msg.innerText = "running...";
  const inp = stdin.value;
  const inpLen = inp.length;
  let inpCur = 0;
  stdout.value = "";
  function pc(x) { stdout.value += String.fromCharCode(x); }
  function gc() {
    if (inpCur == inpLen) throw "eof";
    inpCur++;
    return inp.charCodeAt(inpCur - 1);
  }
  function eof() { return inpCur == inpLen; }
  WebAssembly.instantiate(new Uint8Array(blahOut),
      {env:{getchar:gc, putchar:pc, eof:eof}}).then(x => {
    x.instance.exports.main();
    msg.innerText = "";
  });
}

function downloadWasm() {
  compile().then(x => {
    const blob = new Blob([new Uint8Array(blahOut)], {type: "application/octet-stream"});
    const a = document.createElement('a');
    a.style.display = 'none';
    document.body.append(a);
    const url = URL.createObjectURL(blob);
    a.href = url;
    a.download = "out.wasm";
    a.click();
    URL.revokeObjectURL(url);
  });
}
function go() { compile().then(x => { run(); }); }

function genlink() {
  const s = "https://"
    + window.location.hostname
    + window.location.pathname
    + "?a=0&p="
    + encodeURIComponent(program.value)
    + "&i="
    + encodeURIComponent(stdin.value);
  out.value = s;
}
</script>
<div style="display:none;">
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[id="hello.hs"]
------------------------------------------------------------------------
import System
main = putStrLn "Hello, World!"
------------------------------------------------------------------------

[id="edigits.hs"]
------------------------------------------------------------------------
import Base
import System
-- Digits of e. See http://miranda.org.uk/examples.
mkdigit n | n <= 9 = chr (n + ord '0')
norm c (d:e:x)
  | e `mod` c + 10 <= c = d + e  `div` c : e' `mod` c : x'
  | otherwise           = d + e' `div` c : e' `mod` c : x'
  where (e':x') = norm (c+1) (e:x)
convert x = mkdigit h:convert t
  where (h:t) = norm 2 (0:map (10*) x)
edigits = "2." ++ convert (repeat 1)
main = putStr $ take 1024 edigits
------------------------------------------------------------------------

[id="primes.hs"]
------------------------------------------------------------------------
import Base
import System
primes = sieve [2..]
sieve (p:x) = p : sieve [n | n <- x, n `mod` p /= 0]
main = print $ take 100 $ primes
------------------------------------------------------------------------

[id="queens.hs"]
------------------------------------------------------------------------
-- Eight queens puzzle. See http://miranda.org.uk/examples.
import Base
import System
safe q b = and[not $ q==p || abs(q-p)==i|(p,i) <- zip b [1..]]
queens sz = go sz where
  go 0 = [[]]
  go n = [q:b | b <- go (n - 1), q <- [1..sz], safe q b]
main = print $ queens 8
------------------------------------------------------------------------

[id="lindon.hs"]
------------------------------------------------------------------------
-- King, are you glad you are king?
import Base
import System
main = interact $ unwords . reverse . words
------------------------------------------------------------------------

[id="sort.hs"]
------------------------------------------------------------------------
import Base
import System
main = interact $ unwords . sorta . words
sorta [] = []
sorta (x:xt) = sorta (filter (<= x) xt) ++ [x] ++ sorta (filter (> x) xt)
------------------------------------------------------------------------

[id="hexmaze.hs"]
------------------------------------------------------------------------
-- https://fivethirtyeight.com/features/can-you-escape-this-enchanted-maze/
import Base
import Map
import System
maze = fromList $ concat $ zipWith row [0..]
  [ "."
  , "IF"
  , " BLUE"
  , "Z ASKS"
  , "AMY EE"
  , "DANCES"
  , " QUEEN"
  , "   Z O"
  , "     O"
  ]
  where
  row r s = concat $ zipWith (cell r) [0..] s
  cell r c x | x /= ' '  = [((r, c), x)]
             | otherwise = []
dirs = [(1, 0), (0, -1), (-1, -1), (-1, 0), (0, 1), (1, 1)]
turn f x = take 2 $ tail $ dropWhile (/= x) $ cycle $ f dirs
data Hex = Hex (Int, Int) (Int, Int) String
step (Hex (x, y) (xd, yd) path) =
  [Hex pos' (xd', yd') (c:path) | (xd', yd') <- next (xd, yd),
    let pos' = (x + xd', y + yd'), member pos' maze]
  where
  c = maze!(x, y)
  next = turn $ if elem c "AEIOUY" then id else reverse

bfs moves = case asum $ won <$> moves of
  Nothing -> bfs $ step =<< moves
  Just soln -> reverse soln
  where
  won (Hex pos _ path)
    | maze!pos == '.' && elem 'M' path = Just path
    | otherwise = Nothing

main = putStrLn $ bfs [Hex (5, 0) (1, 1) ""]
------------------------------------------------------------------------

[id="gray.hs"]
------------------------------------------------------------------------
-- Gray code.
import Base
import System
gray 0 = [""]
gray n = ('0':) <$> gray (n - 1) <|> reverse (('1':) <$> gray (n - 1))
main = putStrLn $ unwords $ gray 4
------------------------------------------------------------------------

[id="hilbert.hs"]
------------------------------------------------------------------------
-- Theorem prover based on a Hilbert system.
-- https://crypto.stanford.edu/~blynn/compiler/hilsys.html
import Base
import System
include::hilsys.inc[]
------------------------------------------------------------------------

[id="douady.hs"]
------------------------------------------------------------------------
-- Based on https://sametwice.com/4_line_mandelbrot.
import Base
import System
prec :: Int
prec = 16384
infixl 7 #
x # y = x * y `div` prec
sqAdd (x, y) (a, b) = (a#a - b#b + x, 2*(a#b) + y)
norm (x, y) = x#x + y#y
douady p = null . dropWhile (\z -> norm z < 4*prec) . take 30 $ iterate (sqAdd p) (0, 0)
main = putStr $ unlines
  [[if douady (616*x - 2*prec, 1502*y - 18022)
    then '*' else ' ' | x <- [0..79]] | y <- [0..23]]
------------------------------------------------------------------------

[id="enigma.hs"]
------------------------------------------------------------------------
-- https://crypto.stanford.edu/~blynn/haskell/enigma.html
import Base
import System
wI   = ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", "Q")
wII  = ("AJDKSIRUXBLHWTMCQGZNPYFVOE", "E")
wIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", "V")
wIV  = ("ESOVPZJAYQUIRHXLNFTGKDCMWB", "J")
wV   = ("VZBRGITYUPSDNHLXAWMJQOFECK", "Z")
ukwA = "EJMZALYXVBWFCRQUONTSPIKHGD"
ukwB = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
ukwC = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

abc = ['A'..'Z']
abc2 = abc ++ abc
sub p x   = maybe x id $ lookup x $ zip abc p
unsub p x = maybe x id $ lookup x $ zip p abc
shift k   = sub   $ dropWhile (/= k) $ abc2
unshift k = unsub $ dropWhile (/= k) $ abc2
conjugateSub p k = unshift k . sub p . shift k
rotorSubs gs = zipWith conjugateSub (fst <$> rotors) gs
rotors = [wI, wII, wIII]
zap gs = unsub p . sub ukwB . sub p where
  p = foldr1 (.) (rotorSubs gs) <$> abc
turn gs@[_, g2, g3] = zipWith (bool id $ shift 'B') bs gs where
  [_, n2, n3] = snd <$> rotors
  bs = [g2 `elem` n2, g2 `elem` n2 || g3 `elem` n3, True]
enigma grundstellung = zipWith zap $ tail $ iterate turn grundstellung
main = interact $ enigma "AAA"
------------------------------------------------------------------------

[id="sha256.hs"]
------------------------------------------------------------------------
-- SHA-256.
--
-- To make this more fun, we compute the algorithm's constants ourselves.
-- They are the first 32 bits of the fractional parts of the square roots
-- and cube roots of primes and hence are nothing-up-my-sleeve numbers.
module Main where
import Base
import System

-- Fixed-point arithmetic with scaling 1/2^40.
-- We break the ring laws but get away with it.
denom = 2^40
data Fixie = Fixie Integer deriving Eq
instance Ring Fixie where
  Fixie a + Fixie b = Fixie (a + b)
  Fixie a - Fixie b = Fixie (a - b)
  Fixie a * Fixie b = Fixie (a * b `div` denom)
  fromInteger = Fixie . (denom *)

properFraction (Fixie f) = (q, Fixie $ f - q) where q = div f denom
truncate (Fixie f) = div f denom
instance Field Fixie where
  recip (Fixie f) = Fixie $ denom*denom `div` f

-- Square roots and cube roots via Newton-Raphson.
-- In theory, the lowest bits may be wrong since we approach the root from one
-- side, but everything turns out fine for our constants.
newton f f' = iterate $ \x -> x - f x / f' x
agree (a:t@(b:_)) = if a == b then a else agree t
fracBits n = (`mod` 2^n) . agree . map (truncate . (2^n*))

primes = sieve [2..] where sieve (p:t) = p : sieve [n | n <- t, n `mod` p /= 0]
rt2 n = newton (\x -> x^2 - n) (\x -> 2*x)   1
rt3 n = newton (\x -> x^3 - n) (\x -> 3*x^2) 1

initHash :: [Word]
initHash = fromIntegral . fracBits 32 . rt2 . fromIntegral <$> take 8  primes
roundKs  :: [Word]
roundKs  = fromIntegral . fracBits 32 . rt3 . fromIntegral <$> take 64 primes

-- Swiped from `Data.List.Split`.
chunksOf i ls = map (take i) (go ls) where
  go [] = []
  go l  = l : go (drop i l)

-- Big-endian conversions and hex dumping for 32-bit words.
be4 n = [div n (256^k) `mod` 256 | k <- reverse [0..3]]
unbe4 cs = sum $ zipWith (*) cs $ (256^) <$> reverse [0..3]
hexdigit n = chr $ n + (if n <= 9 then ord '0' else ord 'a' - 10)
hex32 n = [hexdigit $ fromIntegral $ div n (16^k) `mod` 16 | k <- reverse [0..7]]

-- SHA-256, at last.
sha256 s = concatMap hex32 $ foldl chunky initHash $ chunksOf 16 ws where
  l = length s
  pad = 128 : replicate (4 + mod (-9 - l) 64) 0 ++ be4 (fromIntegral l * 8)
  ws = map unbe4 $ chunksOf 4 $ map (fromIntegral . fromEnum) s ++ pad

chunky h c = zipWith (+) h $ foldl hashRound h $ zipWith (+) roundKs w where
  w = c ++ foldr1 (zipWith (+)) [w, s0, drop 9 w, s1] where
    s0 = foldr1 (zipWith xor) $ map (<$> tail w) [ror 7, ror 18, shr 3]
    s1 = foldr1 (zipWith xor) $ map (<$> drop 14 w) [ror 17, ror 19, shr 10]
    shr = flip shiftR
    ror = flip rotateR

hashRound [a,b,c,d,e,f,g,h] kw = [t1 + t2, a, b, c, d + t1, e, f, g] where
  s1 = foldr1 xor $ map (rotateR e) [6, 11, 25]
  ch = (e .&. f) `xor` (complement e .&. g)
  t1 = h + s1 + ch + kw
  s0 = foldr1 xor $ map (rotateR a) [2, 13, 22]
  maj = (a .&. b) `xor` (a .&. c) `xor` (b .&. c)
  t2 = s0 + maj

main = interact sha256
------------------------------------------------------------------------

[id="keccak.hs"]
------------------------------------------------------------------------
-- https://keccak.team/keccak_specs_summary.html
-- https://en.wikipedia.org/wiki/SHA-3
--
-- This is the hash function used by Ethereum.
-- To get the SHA-3 256 standard hash, in the `pad` function,
-- change 0x81 to 0x86 and 0x01 to 0x06.
import Base
import System

-- Swiped from `Data.List.Split`.
chunksOf i ls = map (take i) (go ls) where
  go [] = []
  go l  = l : go (drop i l)

-- We lack the instance needed for the fancier `drop n <> take n`.
drta n xs = drop n xs <> take n xs
onHead f (h:t) = (f h:t)

kRound :: [[Word64]] -> Word64 -> [[Word64]]
kRound a rc = onHead (onHead $ xor rc) chi where
  c = foldr1 (zipWith xor) a
  d = zipWith xor (drta 4 c) (map (`rotateL` 1) $ drta 1 c)
  theta = map (zipWith xor d) a
  b = [[rotateL ((theta!!i)!!x) $ rotCon x i | i <- [0..4], let x = (3*j+i) `mod` 5] | j <- [0..4]]
  chi = zipWith (zipWith xor) b $ zipWith (zipWith (.&.)) (map (map complement . drta 1) b) $ map (drta 2) b

rotCon 0 0 = 0
rotCon x y = t `mod` 64 where
  Just t = lookup (x, y) hay
  hay = zip (iterate go (1, 0)) tri
  go (x, y) = (y, (3*y + 2*x) `mod` 5)
  tri = 1 : zipWith (+) tri [2..]

rcs :: [Word64]
rcs = take 24 $ go $ iterate lfsr 1 where
  go xs = sum (zipWith setOdd as [0..]) : go bs where
    (as, bs) = splitAt 7 xs
  setOdd n m = if mod n 2 == 1 then 2^(2^m - 1) else 0

lfsr :: Int -> Int
lfsr n
  | n < 128   = 2*n
  | otherwise = xor 0x71 $ 2*(n - 128)

keccak256 s = concatMap bytes $ take 4 $ head final where
  final = foldl go blank $ map fives $ chunksOf 17 $ word64s $ pad s
  go a b = foldl kRound (zipWith (zipWith xor) a b) rcs
  bytes n = take 8 $ chr . fromIntegral . (`mod` 256) <$> iterate (`div` 256) n

fives = iterate (drop 5) . (++ repeat 0)
blank = replicate 5 $ replicate 5 0
pad s = (s++) $ if n == 1 then ['\x81'] else '\x01':replicate (n - 2) '\x00' ++ ['\x80'] where
  n = 136 - mod (length s) 136

word64s :: String -> [Word64]
word64s [] = []
word64s xs = foldr go 0 <$> chunksOf 8 xs where
  go d acc = fromIntegral (fromEnum d) + 256*acc

hex c = (hexit q:) . (hexit r:) where (q, r) = divMod (ord c) 16
hexit c = chr $ c + (if c < 10 then 48 else 87)
xxd = concatMap (`hex` "")
main = interact $ xxd . keccak256
------------------------------------------------------------------------

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The above compiles a Haskell program to a WebAssembly binary [+++<a href='#'
onclick='downloadWasm()'>download it!</a>+++], then runs it on the given input.
Several language features are missing.

Source: https://github.com/blynn/compiler[https://github.com/blynn/compiler]

== Best of the worst ==

In 2000, I took the Comprehensive Exams given by the Stanford University
Computer Science department. In the Compilers exam, I got the top score...of
those who failed.

It didn't matter because I scraped through the Databases exam instead.
But how could I fail Compilers? I had sailed through my undergrad compilers
course, and written a few toy compilers for fun. I resolved to one day unravel
the mystery.

Since then, I have sporadically read about various compiler topics. Did my
younger self deserve to fail? Maybe. There were certainly gaps in that
guy's knowledge (which are only a shade narrower now). On the other hand,
there are equally alarming gaps in my textbooks, so maybe I shouldn't have
failed.

Or maybe I'm still bitter about that exam. In any case, here is a dilettante's
guide to writing compilers while thumbing your nose at the establishment.

(I also flunked AI, Networks, and Numerical Analysis. After reading John L.
Gustafson, _The End of Error: Unum Computing_, I'm glad I'm not an expert on
the stuff they asked in that Numerical Analysis exam. But that's a topic for
another day.)

== See also ==

https://github.com/chrisdone/duet[Duet is another tiny implementation of a
subset of Haskell].


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script>
function inscribe(s, inp) {
  document.getElementById("prog").value = s;
  document.getElementById("inp").value = inp;
  document.getElementById("out").value = "";
}

function setup(name, inp) {
  document.getElementById(name).addEventListener("click", (event) =>
    inscribe(document.getElementById(name + ".hs").textContent.trim(), inp));
}

setup("hello", "");
setup("edigits", "");
setup("primes", "");
setup("queens", "");
setup("lindon", "you can cage a swallow can't you");
setup("sort", "James while John had had had had had had had had had had had a better effect on the teacher");
setup("hexmaze", "");
setup("gray", "");
setup("hilbert", "");
setup("douady", "");
setup("enigma", "ATTACKATDAWN");
setup("sha256", "");
setup("keccak", "");

const params = (new URL(window.location.href)).searchParams;
const a = params.get("a");
if (params.get("a")) {
  if (a == "0") {
    inscribe(params.get("p"), params.get("i"));
  } else if (a == "1") {
    document.getElementById(params.get("p")).click();
    document.getElementById("inp").value = params.get("i");;
  }
} else {
  document.getElementById("hello").click();
}
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
