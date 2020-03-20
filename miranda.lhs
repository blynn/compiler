= Miranda =

Haskell nearly never existed.
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf[They
originally planned to build on David Turner's Miranda language] rather than
invent a new one.

Released in the 1980s, Miranda compiles a Haskell-like language to a certain
set of combinators which a C program interprets. This is precisely what we do!

In January 2020, http://miranda.org.uk/downloads[Miranda's source] was
released. Its approach to compilation has remained unchanged through the years,
yielding an excellent opportunity for an exhibition match.

&#9654; Download the files from this contest: link:cmpmira.tar.gz[cmpmira.tar.gz].

== These go to eleven ==

We bump up a Miranda example that solves the
https://en.wikipedia.org/wiki/Eight_queens_puzzle[eight queens puzzle] to 11
queens:

\begin{code}
include::q11.m[]
\end{code}

On my laptop, to build `mira`, I had to first edit the `Makefile` and change
`quotehostinfo` to `./quotehostinfo`. Afterwards:

------------------------------------------------------------------------
$ mira -make q11.m
$ time mira -exec q11.m > /dev/null

real    0m8.132s
user    0m8.111s
sys     0m0.020s
------------------------------------------------------------------------

We translate it for our `assembly` compiler. We need much more code as we lack
a standard library:

++++++++++
<script>
function hideshow(s) {
  var x = document.getElementById(s);
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
}
</script>
<p><a onclick='hideshow("q11");'>&#9654; Toggle `q11.hs`</a></p>
<div id='q11' style='display:none'>
++++++++++

\begin{code}
include::q11.hs[]
\end{code}

++++++++++
</div>
++++++++++

On my laptop:

------------------------------------------------------------------------
$ (cat rts.c;./assembly < q11.hs) > q11.c
$ cc -O2 q11.c -o q11
$ time ./q11 > /dev/null

real    0m6.783s
user    0m6.734s
sys     0m0.048s
------------------------------------------------------------------------

I can't help feeling proud. Miranda uses
https://arxiv.org/pdf/1510.03794v1.pdf[Turner's bracket abstraction algorithm],
which pushes Schönfinkel's classic approach about as far as it can go. It has a
large set of combinators, including dedicated combinators for map and fold.
And surely its runtime system must be expertly tuned.

Our program, on the other hand, compiles to a handful of basic combinators,
uses the Scott encoding for all data types except unsigned ints, and the source
to its hastily designed virtual machine prizes brevity over efficiency: it
began life as link:ioccc.html[an IOCCC entry] after all. Indeed, simple
changes boost its speed by 10%, which we shall examine in depth another day.

But really its performance has little to do with my prowess. The credit goes to
Oleg Kiselyov's bracket abstraction algorithm (with minor tweaks from a few
syntactic rewrites).

++++++++++
<p><a onclick='hideshow("combo");'>&#9654; Toggle combinators</a></p>
<div id='combo' style='display:none'>
++++++++++

------------------------------------------------------------------------
% = Q %;
/ = Q /;
* = Q *;
- = Q -;
+ = Q +;
unsafePerformIO = C (T ?) K;
exitSuccess = .;
fail# = unsafePerformIO .;
writeIORef = w;
readIORef = r;
newIORef = n;
ioPure = B C T;
ioBind = C;
succ = T (1 +);
ord = I;
chr = I;
() = K;
if = I;
intLE = Q L;
intEq = Q =;
shows = T I;
>>= = T (K I);
return = T K;
<*> = T (K I);
pure = T K;
fmap = T I;
<= = T I;
== = T I;
putChar = T FFI_0;
, = B C T;
|, = I;
: = B (B K) (B C T);
[] = K;
|[]|: = I;
False = K I;
True = K;
|True|False = I;
{Eq Int} = T intEq;
{Ord Int} = T intLE;
>> = B (R K) (B (B B) (>>=));
$ = I;
. = B;
id = I;
flip = C;
abs = S (S (R 2147483647 ((<=) {Ord Int})) I) ((-) 0);
not = R K (T False);
|| = T K;
&& = R False;
flst = I;
foldr = B (S (B C T)) (S (B B (B C (B B))) foldr);
++ = C (foldr (:));
concat = foldr (++) K;
map = C (B foldr (B (:))) K;
concatMap = B (B concat) map;
and = foldr (&&) K;
undefined = undefined;
!! = R (S (B C (B (B B) (R 0 ((==) {Eq Int})))) (B (C (!!)) (R 1 (-)))) (B B (T undefined));
checks = S (B S (B (B S) (B (B (B (||))) (R (!!) (B B (B B ((==) {Eq Int}))))))) (B (R (R 1 (+))) (B (B S) (B (B (B ((==) {Eq Int}))) (B (B (B abs)) (R (!!) (B B (B B (-))))))));
index = Y (B (B (S (T K))) (B (B (B K)) (B (B (B K)) (B (B (C (T fail#))) (B (B K) (B (S (B B (:))) (R (R 1 (+)) B))))))) 0;
safe = B (B and) (R index (B S (B (B map) (B (B (B not)) checks))));
range = B (R K) (S (B S ((<=) {Ord Int})) (S (B B (:)) (B range (R 1 (+)))));
queens = S (R ((:) K K) ((==) {Eq Int} 0)) (B (concatMap (R (range 1 11) (B concatMap (S (B S (B (B concatMap) (B (B K) (B (R K) (B (B (:)) (C (:))))))) (B (R K) (B (R ((:) K K)) (C safe))))))) (B queens (R 1 (-))));
{Applicative IO} = C (T ioPure) (R (R (B ioPure) (B B C)) (B B C));
{Monad IO} = C (T ioPure) C;
{Functor IO} = T (B ((<*>) {Applicative IO}) ioPure);
mapM_ = B (C (B C (B (B foldr) (B B (>>))))) (R K pure);
putStr = mapM_ {Applicative IO} {Monad IO} putChar;
showInt' = S (R I ((==) {Eq Int} 0)) (S (B B (B showInt' (R 10 (/)))) (B (:) (B ((+) 48) (R 10 (%)))));
{Shows Int} = T (S (R ((:) 48) ((==) {Eq Int} 0)) showInt');
intersperse = B (C (T K)) (B (C (B B (:))) (R K (B foldr (B (B (++)) (R (R K (:)) (B B (:)))))));
{Shows ([] a)} = B T (B (B (B ((:) 91))) (B (R ((:) 93)) (B (B B) (B (B (foldr B I)) (B (B (intersperse ((:) 44))) (B map shows))))));
main = putStr (shows ({Shows ([] a)} ({Shows ([] a)} {Shows Int})) (queens 11) "");
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== A second opinion ==

The difference is even more pronounced for another example that computes 'e' to
4096 decimal places:

------------------------------------------------------------------------
$ time ./e4096 > /dev/null

real    0m6.532s
user    0m6.471s
sys     0m0.060s
$ time mira -exec e4096.m > /dev/null

real    0m14.350s
user    0m14.321s
sys     0m0.013s
------------------------------------------------------------------------

The Miranda original:

\begin{code}
include::e4096.m[]
\end{code}

Our version:

++++++++++
<p><a onclick='hideshow("e4096");'>&#9654; Toggle e4096.hs</a></p>
<div id='e4096' style='display:none'>
++++++++++

\begin{code}
include::e4096.hs[]
\end{code}

++++++++++
</div>
++++++++++
