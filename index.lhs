= Compilers for contrarians =

link:ioccc.html[An award-winning Haskell compiler], browser edition.

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<p><span style='cursor:pointer;' onclick='hideshow("pre");'><span id='pre_toggle'>[+] Show</span> Prelude</span></p>
<p>
<textarea readonly id='pre' rows='32' style='display:none;box-sizing:border-box;width:100%;'>
include::wasm/blah.pre[]
</textarea>
</p>
<p>
<label for="prog">Program:</label>
<button id="hello">&#128075;</button>
<button id="edigits"><i>e</i></button>
<button id="queens">&#9819;</button>
<button id="lindon">Lindon</button>
<button id="sort">&#9035;</button>
<button id="hexmaze">&#11042;</button>
</p>
<textarea rows='12' id="prog" name="prog"
style='box-sizing:border-box;width:100%;'>
</textarea>
</p>
<p>
<button onclick="go()">Run</button> <span id="msg"></span>
</p>
<label for="inp">Input:</label>
<p>
<textarea id='inp' rows='2' style='box-sizing:border-box;width:100%;'></textarea>
</p>
<label for="out">Output:</label>
<p>
<textarea readonly id='out' rows='8' style='box-sizing:border-box;width:100%;'></textarea>
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
    xt.innerHTML = "[-] Show"
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
    }).then(obj => { blah = obj.instance; console.log("DONE"); });
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
  lastProg = blahInp;
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
</script>
<script src='index.js'></script>

<div id="hello.hs" style="display:none;">
main = putStrLn "Hello, World!\n";
</div>

<div id="edigits.hs" style="display:none;">
-- Digits of e. See http://miranda.org.uk/examples.
mkdigit n | n <= 9 = chr (n + ord '0');
norm c (d:(e:x))
  | e `mod` c + 10 <= c = d + e  `div` c : e' `mod` c : x'
  | otherwise           = d + e' `div` c : e' `mod` c : x'
  where { (e':x') = norm (c+1) (e:x) };
convert x = let { x' = norm 2 (0:map (10*) x) } in mkdigit (head x'):convert (tail x');
edigits = "2." ++ convert (repeat 1);
main = putStr $ take 1024 edigits;
</div>

<div id="queens.hs" style="display:none;">
-- Eight queens puzzle. See http://miranda.org.uk/examples.
checks q b i = q==b!!i || abs(q-b!!i)==i+1;
index x = let
  { f n [] = []
  ; f n (a:x) = n:f(n+1)x
  } in f 0 x;
safe q b = and $ map (not . checks q b) $ index b;

-- List comprehensions and ranges are on the to-do list.
-- For now, desugar [q:b | b <- go (n - 1), q <- [1..sz], safe q b]
queens sz = go sz where
  { go 0 = [[]]
  ; go n = go (n - 1) >>= \b -> range 1 sz >>= \q -> guard (safe q b) >> [q:b]
  };
range m n | m <= n = m:range (m+1) n
          | otherwise = [];
main = print $ queens 8;
</div>

<div id="lindon.hs" style="display:none;">
-- King, are you glad you are king?
main = interact $ unwords . reverse . words;
</div>

<div id="sort.hs" style="display:none;">
main = interact $ unwords . sorta . words;
sorta [] = [];
sorta (x:xt) = sorta (filter (<= x) xt) ++ [x] ++ sorta (filter (> x) xt);
</div>

<div id="hexmaze.hs" style="display:none;">
-- https://fivethirtyeight.com/features/can-you-escape-this-enchanted-maze/
nats = iterate (1+) 0;
maze = fromList $ concat $ zipWith row nats
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
  { row r s = concat $ zipWith (cell r) nats s
  ; cell r c x | x /= ' '  = [((r, c), x)]
               | otherwise = []
  }
  ;
dirs = [(1, 0), (0, 0-1), (0-1, 0-1), (0-1, 0), (0, 1), (1, 1)];
turn f x = take 2 $ tail $ dropWhile (/= x) $ cycle $ f dirs;
data Hex = Hex (Int, Int) (Int, Int) String;
step (Hex (x, y) (xd, yd) path) =
  next (xd, yd) >>= \(xd', yd') -> let
  { pos' = (x + xd', y + yd')
  } in guard (member pos' maze) >> [Hex pos' (xd', yd') (c:path)]
  where
  { c = maze!(x, y)
  ; next = turn $ if elem c "AEIOUY" then id else reverse
  };

bfs moves = case asum $ won <$> moves of
  { Nothing -> bfs $ step =<< moves
  ; Just soln -> reverse soln
  } where
  { won (Hex pos _ path)
    | maze!pos == '.' && elem 'M' path = Just path
    | otherwise = Nothing
  };

main = putStrLn $ bfs [Hex (5, 0) (1, 1) ""];
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The above compiles a Haskell program to a WebAssembly binary [+++<a href='#'
onclick='downloadWasm()'>download it!</a>+++], then runs it on the given input.

Many language features are missing, including checking for certain errors. The
parser is fussy, and the error messages are cryptic. The only primitive type
is `Int`: signed 32-bit integers.

[https://github.com/blynn/compiler[git repo]]

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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
import Control.Monad (void, when)
import Data.Char (isSpace)
import Haste.DOM
import Haste.Events

main :: IO ()
main = withElems ["prog", "inp"] $ \[pEl, iEl] -> do
  let
    setup button inp = do
      Just b <- elemById button
      Just p <- elemById $ button ++ ".hs"
      let
        go = do
          prog <- getProp p "textContent"
          prog <- pure $ filter (/= '\r') $ dropWhile isSpace prog
          setProp pEl "value" prog
          setProp iEl "value" inp
      void $ b `onEvent` Click $ const go
      when (button == "hello") go

  setup "hello" ""
  setup "edigits" ""
  setup "queens" ""
  setup "lindon" "you can cage a swallow can't you"
  setup "sort" "James while John had had had had had had had had had had had a better effect on the teacher"
  setup "hexmaze" ""
\end{code}

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
</div>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
