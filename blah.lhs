= Blah =

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<p><span style='cursor:pointer;' onclick='hideshow("pre");'><span id='pre_toggle'>[+] Show</span> Prelude</span></p>
<p>
<textarea readonly id='pre' rows='32' style='display:none;box-sizing:border-box;width:100%;'>
include::wasm/blah.pre[]
</textarea>
</p>
<label for="prog">Program:</label>
<p>
<p>
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
<button onclick='downloadWasm()'>Download</button>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

[pass]
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

var params = (new URL(window.location.href)).searchParams;
var msg = document.getElementById("msg");
var wasmArr;
var lastProg = "";

function parm(k) { var r = params.get(k); if (r) return r; else return ""; }

function run(program, stdin) {
  if (lastProg == program) {
    interact(stdin);
    return;
  }
  lastProg = program;
  var inp = program, inpLen = inp.length, inpCur = 0;
  document.getElementById("out").value = "";
  function gc() {
    if (inpCur == inpLen) throw "eof";
    inpCur++;
    return inp.charCodeAt(inpCur - 1);
  }
  function eof() { return inpCur == inpLen; }
  var out = [];
  function pc(x) { out.push(x); }
  msg.innerHTML = "compiling...";
  WebAssembly.instantiateStreaming(fetch('blah.wasm'), {
      env:{ getchar:gc, putchar:pc, eof:eof }
    }).then(obj => {
    obj.instance.exports.compile();
    wasmArr = out;
    if (wasmArr[0] != 0) {
      msg.innerHTML = "compile error: " + String.fromCharCode.apply(null, wasmArr);
    } else {
      msg.innerHTML = "";
      interact(stdin);
    }
  });
}

function interact(inp) {
  var out = document.getElementById("out");
  out.value = "";
  msg.innerHTML = "running...";
  function pc(x) { out.value += String.fromCharCode(x); }

  var inpLen, inpCur;
  function gc() {
    if (inpCur == inpLen) throw "eof";
    inpCur++;
    return inp.charCodeAt(inpCur - 1);
  }
  function eof() { return inpCur == inpLen; }
  inpCur = 0;
  inpLen = inp.length;
  WebAssembly.instantiate(new Uint8Array(wasmArr),
      {env:{getchar:gc, putchar:pc, eof:eof}}).then(x => {
    x.instance.exports.fun();
    msg.innerHTML = "";
  });
}

function downloadWasm() {
  var blob = new Blob([new Uint8Array(wasmArr)], {type: "application/octet-stream"});
  var a = document.createElement('a');
  a.style.display = 'none';
  document.body.append(a);
  var url = URL.createObjectURL(blob);
  a.href = url;
  a.download = "out.wasm";
  a.click();
  URL.revokeObjectURL(url);
}

var prelude = document.getElementById("pre");
var program = document.getElementById("prog");
var stdin = document.getElementById("inp");

if (parm("action") == "Test") {
  program.value = banishBadLuck(parm("prog"));
  stdin.value = banishBadLuck(parm("inp"));
  run(program.value, stdin.value);
}

function go() { run(prelude.value + program.value, stdin.value); }
</script>
<script src='blah.js'></script>

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

\begin{code}
{-# LANGUAGE QuasiQuotes #-}
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
