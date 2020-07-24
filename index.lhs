= Compilers for contrarians =

link:ioccc.html[An award-winning Haskell compiler], browser edition.

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<p><span style='cursor:pointer;' onclick='hideshow("pre");'><span id='pre_toggle'>[+] Show</span> Prelude</span></p>
<p>
<textarea spellcheck='false' readonly id='pre' rows='32' style='display:none;box-sizing:border-box;width:100%;'>
include::wasm/blah.pre[]
</textarea>
</p>
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

<script>
"use strict";
function hideshow(s) {
  var x = document.getElementById(s);
  var xt = document.getElementById(s + "_toggle");
  if (x.style.display === "none") {
    x.style.display = "block";
    xt.innerHTML = "[-] Hide"
  } else {
    x.style.display = "none";
    xt.innerHTML = "[+] Show"
  }
}

var blah;
var blahInp, blahInpLen, blahInpCur;
var blahOut;

function setup() {
  function gc() {
    if (blahInpCur == blahInpLen) throw "eof";
    blahInpCur++;
    return blahInp.charCodeAt(blahInpCur - 1);
  }
  function pc(x) { blahOut.push(x); }
  function eof() { return blahInpCur == blahInpLen; }
  WebAssembly.instantiateStreaming(fetch('blah.wasm'), {
      env:{ getchar:gc, putchar:pc, eof:eof }
    }).then(obj => { blah = obj.instance; });
}
setup();

var msg = document.getElementById("msg");
var lastProg = "";
var prelude = document.getElementById("pre");
var program = document.getElementById("prog");
var stdin = document.getElementById("inp");
var stdout = document.getElementById("out");

function compile() {
  msg.innerHTML = "compiling...";
  stdout.value = "";
  blahInp = prelude.value + program.value;
  if (lastProg == blahInp) return new Promise(function(resolve) { resolve() });
  blahInpLen = blahInp.length, blahInpCur = 0;
  blahOut = [];
  // Timeout so message is displayed. Unreliable.
  return new Promise(function(resolve, reject) {
    setTimeout(function() {
  blah.exports.compile();
  if (blahOut[0] != 0) {
    msg.innerHTML = "compile error: " + String.fromCharCode.apply(null, blahOut);
    reject();
  } else {
    lastProg = blahInp;
    msg.innerHTML = "";
    resolve();
  }
    }, 0);
  });
}

function run() {
  msg.innerHTML = "running...";
  var inp = stdin.value;
  var inpLen = inp.length, inpCur = 0;
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
    x.instance.exports.fun();
    msg.innerHTML = "";
  });
}

function downloadWasm() {
  compile().then(x => {
    var blob = new Blob([new Uint8Array(blahOut)], {type: "application/octet-stream"});
    var a = document.createElement('a');
    a.style.display = 'none';
    document.body.append(a);
    var url = URL.createObjectURL(blob);
    a.href = url;
    a.download = "out.wasm";
    a.click();
    URL.revokeObjectURL(url);
  });
}
function go() { compile().then(x => { run(); }); }

function genlink() {
  var s = "https://"
    + window.location.hostname
    + window.location.pathname
    + "?a=0&p="
    + encodeURIComponent(program.value)
    + "&i="
    + encodeURIComponent(stdin.value);
  out.value = s;
}

var params = (new URL(window.location.href)).searchParams;
function parm(k) { var r = params.get(k); if (r) return r; else return ""; }

</script>
<script src='index.js'></script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<div style="display:none;">
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[id="hello.hs"]
------------------------------------------------------------------------
main = putStrLn "Hello, World!"
------------------------------------------------------------------------

[id="edigits.hs"]
------------------------------------------------------------------------
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
primes = sieve [2..]
sieve (p:x) = p : sieve [n | n <- x, n `mod` p /= 0]
main = print $ take 100 $ primes
------------------------------------------------------------------------

[id="queens.hs"]
------------------------------------------------------------------------
-- Eight queens puzzle. See http://miranda.org.uk/examples.
safe q b = and[not $ q==p || abs(q-p)==i|(p,i) <- zip b [1..]]
queens sz = go sz where
  go 0 = [[]]
  go n = [q:b | b <- go (n - 1), q <- [1..sz], safe q b]
main = print $ queens 8
------------------------------------------------------------------------

[id="lindon.hs"]
------------------------------------------------------------------------
-- King, are you glad you are king?
main = interact $ unwords . reverse . words
------------------------------------------------------------------------

[id="sort.hs"]
------------------------------------------------------------------------
main = interact $ unwords . sorta . words
sorta [] = []
sorta (x:xt) = sorta (filter (<= x) xt) ++ [x] ++ sorta (filter (> x) xt)
------------------------------------------------------------------------

[id="hexmaze.hs"]
------------------------------------------------------------------------
-- https://fivethirtyeight.com/features/can-you-escape-this-enchanted-maze/
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
dirs = [(1, 0), (0, 0-1), (0-1, 0-1), (0-1, 0), (0, 1), (1, 1)]
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
gray 0 = [""]
gray n = ('0':) <$> gray (n - 1) <|> reverse (('1':) <$> gray (n - 1))
main = putStrLn $ unwords $ gray 4
------------------------------------------------------------------------

[id="hilbert.hs"]
------------------------------------------------------------------------
-- Theorem prover based on a Hilbert system.
-- https://crypto.stanford.edu/~blynn/compiler/hilsys.html
include::hilsys.inc[]
------------------------------------------------------------------------

[id="douady.hs"]
------------------------------------------------------------------------
-- Based on https://sametwice.com/4_line_mandelbrot.
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

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The above compiles a Haskell program to a WebAssembly binary [+++<a href='#'
onclick='downloadWasm()'>download it!</a>+++], then runs it on the given input.

Many language features are missing, including checking for certain errors. The
parser is fussy, and the error messages are cryptic. The only primitive type
is `Int`, which are signed 32-bit integers.

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
Gustafson, 'The End of Error: Unum Computing', I'm glad I'm not an expert on
the stuff they asked in that Numerical Analysis exam. But that's a topic for
another day.)

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<div id='ui' style='display:none;'>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

\begin{code}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}
import Control.Monad (void)
import Data.Char (isSpace)
import Haste.DOM
import Haste.Events
import Haste.Foreign (ffi)
import Haste.JSString

main :: IO ()
main = withElems ["prog", "inp", "out"] $ \[pEl, iEl, oEl] -> do
  let
    setup button inp = do
      Just b <- elemById button
      void $ b `onEvent` Click $ const $ go button inp
    go button inp = do
      Just grandparent <- elemById $ button ++ ".hs"
      Just parent <- getFirstChild grandparent
      Just p <- getFirstChild parent
      prog <- dropWhile isSpace <$> getProp p "textContent"
      inscribe prog inp
    inscribe prog inp = do
      setProp pEl "value" prog
      setProp iEl "value" inp
      setProp oEl "value" ""

  setup "hello" ""
  setup "edigits" ""
  setup "primes" ""
  setup "queens" ""
  setup "lindon" "you can cage a swallow can't you"
  setup "sort" "James while John had had had had had had had had had had had a better effect on the teacher"
  setup "hexmaze" ""
  setup "gray" ""
  setup "hilbert" ""
  setup "douady" ""
  setup "enigma" "ATTACKATDAWN"
  go "hello" ""

  let parm = ffi "parm" :: JSString -> IO JSString
  parm "a" >>= \case
    "0" -> do
      prog <- parm "p"
      inp <- parm "i"
      inscribe (unpack prog) (unpack inp)
    "1" -> do
      preset <- parm "p"
      inp <- parm "i"
      go (unpack preset) (unpack inp)
    _ -> pure ()
\end{code}

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
