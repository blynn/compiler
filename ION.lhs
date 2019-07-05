= The ION machine =

One of the many problems with link:lambda.html[our "ultimate" compiler] is that
we used a rich powerful language based on lambda calculus to build a shoddy
compiler for bare-bones lambda calculus.

Let us begin anew from C, which is so close to the metal that we can see how we
could start from assembly language if we had to. Also, C compilers are
available on countless platforms.

We take a page from Knuth's 'The Art of Computer Programming' and introduce a
mythical computer: the International Obfuscated Nonce (ION) machine, so named
because it's based on a one-time random design used in link:ioccc.html[an entry
to the 26th International Obfuscated C Code Contest].

The basic unit of data is a 32-bit word. Memory is an array of 32-bit words,
indexed by 32-bit words. Among the registers are the heap pointer HP and the
stack pointer SP. The instruction set consists of the usual suspects: loading
and storing words, arithmetic, jumps. The contents of memory are initially
undefined.

Our compiler generates C code that simulates this machine, though we first
describe it at a higher level. By abuse of notation, `Int` means the type of
32-bit words. (Using `Word32` requires more imports and conversions here and
there.)

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
<p><a onclick='hideshow("ugh");'>&#9654; Toggle extensions and imports</a></p>
<div id='ugh' style='display:none'>
++++++++++

\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
module ION where
import Data.Char (chr, ord)
import qualified Data.Map as M
import Data.Map (Map, (!))
import System.IO
\end{code}

++++++++++
</div>
++++++++++

\begin{code}
data VM = VM { sp :: Int, hp :: Int, mem :: Map Int Int }
\end{code}

We introduce a base type for 32-bit words, so we can take advantage of native
32-bit words. We also introduce combinators that correspond to the native
artihmetic instructions.

Let `n` be a 32-bit word. If `n` lies in [0..127], `n` represents a combinator,
otherwise `n` represents the application of the expression represented by
`load n` to the expression represented by `load (n + 1)`. There is an
exception to this rule: if `load n = 35` (the ASCII code of "#") then `n`
represents the 32-bit constant returned by `load (n + 1)`.

This implies we can never refer to an application held in the first 128 memory
locations, and also implies we have limited ourselves to at most 128 different
combinators.

When possible, combinators are represented by the ASCII code of a related
character. For example, `66` is the B combinator, since `ord 'B' == 66`
and `42` is the multiply combinator, since `ord '*' == 42`.

We initialize HP to 128, the bottom of the heap, which grows upwards.
We set SP to the highest memory address, the bottom of the stack, which
grows downwards. The stack keeps track of where we've been as we walk up and
down the left spine of the tree representing the combinatory logic term being
evaluated.

\begin{code}
new :: VM
new = VM maxBound 128 mempty

load :: Int -> VM -> Int
load k vm = mem vm ! k

store :: Int -> Int -> VM -> VM
store k v vm = vm{mem = M.insert k v $ mem vm}

app :: Int -> Int -> VM -> (Int, VM)
app x y vm@VM{hp} = (hp, store hp x $ store (hp + 1) y $ vm { hp = hp + 2 })

push :: Int -> VM -> VM
push n vm@VM{sp} = store (sp - 1) n $ vm{sp = sp - 1}
\end{code}

== Sharing economy ==

We earlier noted that applying the S combinator causes two nodes to refer to
the same subterm. This sharing saves room, but we can save more. Our `reduce`
function is copy-on-write, by which we mean if two nodes X and Y refer to the
same subterm, reducing X causes X to point to a new subterm, while preserving
Y. Instead, we should implement "don't-bother-copying-on-write", and modify
the same subterm whether X or Y is being reduced, a strategy known as 'lazy
evaluation'.

We might think of this as different to normal order, because we're reducing
both X and Y even if they are not the two left-most subterms.

== The numbers game ==

For primitve functions, we use a trick described in depth by Naylor and
Runciman, "The Reduceron reconfigured and re-evaluated": we reduce `#nf` to
`f(#n)`. For example, the term `(I#2)(K(#3)S)(+))` reduces to `(+)(#3)(#2)`.

Observe first two arguments of `(+)` are normalized integers, so our code for
reducing `(+)(#m)(#n)` simply pulls out the words `m` and `n` from certain
locations in memory and returns `#s` where `s == m + n` modulo 2^32.

We support the operations `+ - / * % = L`. The first 5 have the same meaning
they do in C, while the last 2 are equivalent to C's `(==)` and `(<=)`.

We add a couple of useful macros: the `R` combinator (equivalent to `CC`) and
the `(:)` combinator (equivalent to `B(BK)(BCT)`).

Below, the I combinator is not lazy: for instance, every time we encounter the
subterm `I(I(Ix))` we must reduce three I combinators. link:ioccc.html[The
competition version of this code] does lazily evaluate `I` combinators, but as
a result, we must tread carefully near the top of the stack.

\begin{code}
arg' :: Int -> VM -> Int
arg' n vm = load (load (sp vm + n) vm + 1) vm

arg :: Int -> VM -> (Int, VM)
arg n vm = (arg' n vm, vm)

app' :: (VM -> (Int, VM)) -> (VM -> (Int, VM)) -> VM -> (Int, VM)
app' f g vm = let
  (x, vm1) = f vm
  (y, vm2) = g vm1
  in app x y vm2

apparg :: Int -> Int -> VM -> (Int, VM)
apparg m n vm = app (arg' m vm) (arg' n vm) vm

wor n = (,) n
com = wor . ord

lazy n f g vm = let
  (p, vm1) = f vm
  (q, vm2) = g vm1
  dst = load (sp vm + n) vm
  in store dst p (store (dst + 1) q vm2) { sp = sp vm + n}

numberArg n vm = load (fst (arg n vm) + 1) vm

builtin :: Int -> VM -> VM
builtin c vm = case chr c of
  'I' -> store (sp vm + 1) (fst $ arg 1 vm) vm { sp = sp vm + 1 }
  'K' -> lvm 2 (com 'I') (arg 1)
  'Y' -> lvm 1 (arg 1) (wor $ load (sp vm + 1) vm)
  'T' -> lvm 2 (arg 2) (arg 1)
  'S' -> lvm 3 (apparg 1 3) (apparg 2 3)
  'B' -> lvm 3 (arg 1) (apparg 2 3)
  'C' -> lvm 3 (apparg 1 3) (arg 2)
  'R' -> lvm 3 (apparg 2 3) (arg 1)
  '#' -> lvm 2 (arg 2) (wor $ load (sp vm + 1) vm)
  ':' -> lvm 4 (apparg 4 1) (arg 2)
  '=' | num 1 == num 2 -> lvm 2 (com 'I') (com 'K')
      | otherwise      -> lvm 2 (com 'K') (com 'I')
  'L' | num 1 <= num 2 -> lvm 2 (com 'I') (com 'K')
      | otherwise      -> lvm 2 (com 'K') (com 'I')
  '*' -> lvm 2 (com '#') (wor $ num 1 * num 2)
  '+' -> lvm 2 (com '#') (wor $ num 1 + num 2)
  '-' -> lvm 2 (com '#') (wor $ num 1 - num 2)
  '/' -> lvm 2 (com '#') (wor $ num 1 `div` num 2)
  '%' -> lvm 2 (com '#') (wor $ num 1 `mod` num 2)
  '?' -> error "?"
  where
  num n = numberArg n vm
  lvm n f g = lazy n f g vm
\end{code}

== ION I/O ==

For us, a program `P` is a function from a string to a string, where strings
are Scott-encoded lists of characters.

To run `P` on standard input and output, we initialize the VM with
`P(0?)(.)(T1)` then repeatedly reduce until asked to reduce the `(.)`
combinator.

The `0` combinator takes one argument, say `x`. The term `0x` reduces to `IK`
at the end of input or `(:)(#c)(0?)` where `c` is the next input character.
The unused argument `x` is a consequence of the ION machine's peculiar encoding
of applications and the need to ensure there is at most one `0` combinator
in the heap.

The `1` combinator takes two arguments, say `x` and `y`. The first argument `x`
should be an integer constant, that is, `#c` for some `c`. Then the character
with ASCII code `c` is printed on standard output, and the expression
`1xy` is reduced to `y(.)(T1)`.

(In ION assembly, symbols and letters are equivalent. In particular, the
`(.)` combinator is not a binary operator representing function composition, but
just another combinator.)

\begin{code}
eval :: Monad m => (Int -> VM -> m (Maybe VM)) -> VM -> m VM
eval exts vm@VM{sp} = let n = load sp vm in
  if n >= 128 then eval exts $ store (sp - 1) (load n vm) vm { sp = sp - 1 }
  else if n == ord '.' then pure vm
  else eval exts =<< (maybe (builtin n vm) id <$> exts n vm)

extsIO :: Int -> VM -> IO (Maybe VM)
extsIO n vm = case chr n of
  '0' -> do
    b <- isEOF
    if b then lvm 1 (com 'I') (com 'K')
    else do
      c <- getChar
      lvm 1 (app' (com ':') (app' (com '#') (com c))) (app' (com '0') (com '?'))
  '1' -> do
    putChar $ chr $ numberArg 1 vm
    lvm 2 (app' (arg 2) (com '.')) (app' (com 'T') (com '1'))
  _ -> pure Nothing
  where
  lvm n f g = pure $ Just $ lazy n f g vm

evalIO :: VM -> IO VM
evalIO = eval extsIO
\end{code}

== Machine shop ==

https://www.youtube.com/watch?v=zhj_tUMwTe0[Edward Kmett's talk "Combinators Revisited"] contains many relevant references and ideas.

Other choices for implementing lambda calculus include:

  * https://en.wikipedia.org/wiki/SECD_machine[SECD machine]
  * http://matt.might.net/articles/cek-machines/[CEK machine]
  * https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/[G-machine] (Chapter 3)
  * https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial/[TIM machine] (Chapter 4)
  * https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf[Spineless Tagless G-machine]
  * https://github.com/grin-compiler/grin[GRIN: Graph Reduction Intermediate Notation]
