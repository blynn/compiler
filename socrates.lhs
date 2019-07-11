= Ancient Greek computer science =

To understand X, Socrates began by asking: "What is X?". For example:
"What is justice?", or "What is virtue?"

His interlocutor typically gave an impressive but imprecise definition, to
which Socrates asked a series of follow-up questions. Sooner or later, he'd
find a contradiction. Both parties gained a deeper understanding of the topic,
but even after several iterations, the main conclusion most often seemed to be
"X is hard to define". Awfully little to extract from such a long process.

I encountered an optimization in a book by Douglas Adams: answer
mathematically. If asked "What is the answer to the meaning of life, the
universe, and everything?", we reply "42" and avoid those meddlesome
contradictions. For if mathematics is flawed, then so is logical reasoning,
and hence the Socratic method itself.

Now the problem is finding a mathematical definition that reflects what we have
in mind. This can be difficult, but luckily, for some of computer science,
mathematical definitions work well, and even guide the implementation.

== What is a compiler? ==

We define a compiler to be a translator from one programming language to
another. More mathematically, a function from programs in the source language
to programs in the target language:

------------------------------------------------------------------------
compile :: Language -> TargetLanguage
------------------------------------------------------------------------

But not just any function will do. A correct compiler must preserve the
meaning of the source program.

== What is a programming language? ==

We view running a program as evaluating a function that takes an input value to
an output value. Thus we define a programming language to be a set of programs
and a function, called an interpreter, that takes a program to a function
mapping input values to output values:

------------------------------------------------------------------------
interpret :: Language -> (Value -> Value)
interpretTarget :: TargetLanguage -> (Value -> Value)
------------------------------------------------------------------------

By convention, the arrow `(->)` associates to the right so we instead write:

------------------------------------------------------------------------
interpret :: Language -> Value -> Value
interpretTarget :: TargetLanguage -> Value -> Value
------------------------------------------------------------------------

Our condition for a compiler correctness is that for any program `p` in
`Language`, and for any `input` in `Value`:

------------------------------------------------------------------------
(interpret p) input == (interpretTarget (compile p)) input
------------------------------------------------------------------------

By convention, juxtaposition denotes function application and associates to the
left, which plays nicely with our other convention, so we write:

------------------------------------------------------------------------
interpret p input == interpretTarget (compile p) input
------------------------------------------------------------------------

So far so good, but there ought to be some indication that these interpreters
are running on computers.

== What is a computer? ==

We could leave it as is:

------------------------------------------------------------------------
compute :: Value -> Value
------------------------------------------------------------------------

This works if we say the input includes the entire state of the computer: every
register, every single bit of memory, and so on. However, we've bitten off far
more than we can chew; how can we reason about a complicated gigantic function?

What if we shrink the domain and range?

------------------------------------------------------------------------
compute :: Char -> Char
------------------------------------------------------------------------

Here, the set `Char` is relatively small, say the set of all 32-bit words.

A list of `Char` can hold inputs and outputs of arbitrary length:

------------------------------------------------------------------------
type Value = [Char]
------------------------------------------------------------------------

Unfortunately, the only thing our `compute` function can do to such a list is
to perform the same operation to each element of the list. We have modeled
something like a circuit, which performs the same task on a fixed number of
bits at a time.

== Deterministic Finite Automata ==

Humans and computers are more than mere circuits because we retain information
from one step to use in future steps. Even though we can only read a few words
at a time, we can absorb a complex story from a book. This is because after
starting in one state of mind, processing a few bits can put us in another
state of mind.

This leads us to consider deterministic finite automata, or finite state
machines:

\begin{code}
type State = Int
type Dfa = (State, State -> Char -> State, State -> Bool)

runDfa :: Dfa -> [Char] -> Bool
runDfa (start, step, accepts) s = go start s where
  go state s = case s of
    ""     -> accepts state
    c:rest -> go (step state c) rest
\end{code}

Briefly, the machine begins in some `start` state, then updates its state
according to the current state and the next input `Char`, and repeats until the
entire input has been processed. If it ends up in a state that is defined to
be an accepting state, we say the machine accepts the input.

For example, the following DFA accepts strings consisting of an odd number
of 'x's:

\begin{code}
oddXs :: Dfa
oddXs = (0, step, \state -> state == 1) where
  step 0 'x' = 1
  step 1 'x' = 0
  step _  _  = 2

-- This should return True.
demoOddXs :: Bool
demoOddXs = runDfa oddXs "xxxxx"
\end{code}

We could extend the above to spit out characters as it computes; see
https://en.wikipedia.org/wiki/Moore_machine[Moore machines] and
https://en.wikipedia.org/wiki/Mealy_machine[Mealy machines], but we already
learn enough it is. We quickly find DFAs are incapable of elementary tasks such
as checking if a bunch of parentheses are balanced, or if some number of As has
been followed by an equal number of Bs.

== Deterministic Push-Down Automata ==

DFAs are hobbled by lack of memory. Let's fix this. Perhaps the simplest kind
of memory is a stack, as evidenced by the popularity of stack-based virtual
machines. We model such machines with 'deterministic push-down automata':

\begin{code}
type Diff = (State, [Char])
data DpdaOp = Reject | BlindPush Diff | Push (Char -> Diff)
type Dpda = (State, Char, State -> Char -> DpdaOp, State -> Bool)

runDpda :: Dpda -> [Char] -> Bool
runDpda (start, stackInit, step, accepts) s = go start [stackInit] s where
  go state stack s = case stack of
    [] -> False
    top:rest -> case step state top of
      Reject -> False
      BlindPush next -> apply next s
      Push f -> case s of
        "" -> accepts state
        c:cs -> apply (f c) cs
      where apply (nextState, stackDiff) = go nextState (stackDiff ++ rest)
\end{code}

Informally, the machine starts in a given state with a given symbol on the
stack. On each step, it examines the stack. If empty, then the machine
rejects, otherwise it pops off the top of the stack. The current state and the
popped-off symbol determine the action for this step, which is one of:

 * `Reject`: halt the machine and reject the input.

 * `BlindPush` ignore the input, enter a given state and push a given
sequence of symbols on the stack.

 * `Push`: if at the end of input, then halt, accepting if in a given set of
states. Otherwise consume one character of the input, which may be used to
determine the next state and the symbols to push.

For example, the following DPDA accepts strings consisting of balanced
parentheses. As it processes characters from the input string, it records the
current nesting depth in unary on the stack.

\begin{code}
balanced :: Dpda
balanced = (0, '-', step, \state -> state == 0) where
  step 0 '-' = Push (\c -> case c of
    '(' -> (1, "1-")
    _   -> (2, "")
    )
  step 1 '-' = BlindPush (0, "-")
  step 1 '1' = Push (\c -> case c of
    '(' -> (1, "11")
    ')' -> (1, "")
    _   -> (2, "")
    )
  step _ _   = Reject

-- This should return True.
demoBalanced :: Bool
demoBalanced = runDpda balanced "(()())"
\end{code}

It turns out DPDAs are incapable of basic tasks such as checking if the input
is a palindrome, or if some number of As has been followed by an equal
number of Bs followed by an equal number of Cs.

== Turing Machines ==

DPDAs flounder because the values popped off the stack are lost. What if we
could save them, say, on a second stack?

Also, the design of DPDAs smells. We have `Char` values on a stack, and another
bunch of `Char` values in our input string. The former tells us whether to
bother reading from the latter. There are two edge cases to worry about: the
empty stack, and the end of input. More than one condition causes the machine
to halt. Can we simplify?

Let's try starting with two stacks that initially hold the input. We add
a current state and a current `Char` value that determine our next move,
which is either to halt, or to do all of the following:

  * push a `Char` to one of the stacks;
  * pop off a `Char` from the other, which becomes our next current value; and
  * enter the next state.

This seems promising, but we have limited memory because each push corresponds
to a pop. We could permit pushing several values at once, but this makes our
machine uglier.

Better to allow "bottomless" stacks. Rather than rejecting on an empty stack, we
instead define the result of popping an empty stack to be a special `Char`
value that we call a 'blank'.

We have just described 'Turing machines'.

Usually, the two stacks and current value are explained in terms of a movable
read/write head on a tape that is infinitely long in either direction, which is
disconcerting since it's impossible to cram an infinite tape into a real
computer, or for that matter, into our universe. Nonetheless we adopt
mainstream nomenclature in our code. For example, we call a stack a one-way
tape.

\begin{code}
data OneWayTape = Char :> OneWayTape
type Tape = (OneWayTape, Char, OneWayTape)
data Direction = L | R
data TMOp = Halt | Step (State, (Char, Direction))
type TM = (State, State -> Char -> TMOp)

runTM :: TM -> Tape -> Tape
runTM (start, step) tape = go start tape where
  go state (l, c, r) = case step state c of
    Halt -> (l, c, r)
    Step (nextState, (c', dir)) -> go state (case dir of
      L -> case l of x :> xs -> (xs     , x, c' :> r)
      R -> case r of x :> xs -> (c' :> l, x, xs     )
      )
\end{code}

We represent a blank with the space character. We encode the input string
by pushing its characters on one of the stacks, or in conventional terms,
we write the string immediately to the right of tape head. We expect the
output to be in the same format.

\begin{code}
blanks :: OneWayTape
blanks = ' ' :> blanks

toTape :: [Char] -> Tape
toTape s      = (blanks, ' ', go s) where
  go ""       = blanks
  go (c:rest) = c :> go rest

fromTape :: Tape -> [Char]
fromTape (_, _, xs) = go xs where
  go (x :> xt) = if x == ' ' then "" else x:go xt

evalTM :: TM -> String -> String
evalTM p s = fromTape (runTM p (toTape s))
\end{code}

We ditch the notion of accepting states; we could trivially support them by
tweaking any given TM to write a symbol representing acceptance before halting.

https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis[Turing machines
are a decent mathematical answer to "What is a computer?"]
Thus we revise our definition of an interpreter, which in turn defines a
programming language:

------------------------------------------------------------------------
interpret :: Language -> TM
------------------------------------------------------------------------

== What is a compiler? ==

Let's put all the pieces together. A language is defined by a function
that maps a given program to a Turing machine. Given two language
specifications:

------------------------------------------------------------------------
interpret :: Language -> TM
interpretTarget :: TargetLanguage -> TM
------------------------------------------------------------------------

a compiler from the first language to the second is a function:

------------------------------------------------------------------------
compile :: Language -> TargetLanguage
------------------------------------------------------------------------

such that for all source programs `p` and for all strings `s`:

------------------------------------------------------------------------
evalTM (interpret p) s == evalTM (interpretTarget (compile p)) s
------------------------------------------------------------------------

== What else is there? ==

Researchers have long used interpreters to define programming languages.
https://aiplaybook.a16z.com/reference-material/mccarthy-1960.pdf[McCarthy's paper on LISP] and
https://fi.ort.edu.uy/innovaportal/file/20124/1/22-landin_correspondence-between-algol-60-and-churchs-lambda-notation.pdf[Landin's paper on ALGOL 60]
are classics.
This technique grew more mathematical over time, evolving into
https://en.wikipedia.org/wiki/Semantics_(computer_science)[formal semantics].
Knuth vividly recounts the history in
https://www.win.tue.nl/~mvdbrand/courses/seminar/0809/papers/ag-genesis.pdf[a
paper on attribute grammars]. (Attribute grammars are a venerable branch of
formal semantics; see Backhouse,
'https://link.springer.com/content/pdf/10.1007/3-540-46002-0_11.pdf[A
Functional Semantics of Attribute Grammars]' for a modern take.)

Incredibly, formal semantics exist for a substantial subset of C (provided
we ignore https://blog.regehr.org/archives/232[parts of the C standard that are
abject nonsense]), and there is even http://compcert.inria.fr/[a certified C
compiler], that is, a C compiler that a computer can prove is correct.

Cryptographers eventually joined the fun in 1982, when Goldwasser and Micali
defined https://en.wikipedia.org/wiki/Semantic_security[semantic security].
Canetti later defined
https://en.wikipedia.org/wiki/Universal_composability[universal composability].

https://www.youtube.com/watch?v=bmKYiUOEo2A[Conal Elliot gave an inspiring talk
on 'denotational design']. He advocates mathematical thinking beyond its
traditional strongholds, and in particular shows how it applies to manipulating
images. See also
https://www.youtube.com/watch?v=j3Q32brCUAI[Functional Reactive Programming]
and
https://www.youtube.com/watch?v=teRC_Lf61Gw[Functional Reactive Programming,
more elegantly specified].

Even
https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems-final.pdf[build
systems] and operating systems
(https://www.sigops.org/s/conferences/sosp/2009/papers/klein-sosp09.pdf[seL4],
http://flint.cs.yale.edu/certikos/[CertiKOS]) can be defined mathematically.

https://en.wikipedia.org/wiki/Montague_grammar[Richard Montague] believed that
with enough research, we can show natural languages are no different to formal
languages, so perhaps there exists a non-mathematical path to precision.

Decades ago,
http://www.cs.umd.edu/~gasarch/BLOGPAPERS/social.pdf[some argued against
getting too formal with programming languages]. Later,
https://eprint.iacr.org/2004/152.pdf[some cryptographers argued similarly
against provable security]. This stance seems incompatible with Montague's.
