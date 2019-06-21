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
contradictions. For a contradiction in mathematics in particular invalidates
logical reasoning, the foundation of the Socratic method.

Now the problem is finding a mathematical definition that reflects what we
have in mind. This can be difficult, but luckily, for parts of computer
science, mathematical definitions work well, and even guide the
implementation.

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

We define a programming language mathematically with an interpreter. We view
running a program as evaluating a function that takes a given input value to
an output value. Then an interpreter is a function taking a program to such a
function:

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

By convention, we use juxtaposition to denote function application, and it
associates to the left, which plays nicely with our other convention, so
we write:

------------------------------------------------------------------------
interpret p input == interpretTarget (compile p) input
------------------------------------------------------------------------

So far so good, but there ought to be some indication that these interpreters
are running on computers.

== What is a computer? ==

We could try:

------------------------------------------------------------------------
compute :: Input -> Output
------------------------------------------------------------------------

This works if we say the input includes the entire state of the computer: every
register value, every single bit of memory, and so on. However, we've bitten
off far more than we can chew; how can we hope to reason about a complicated
gigantic function?

What if we shrink the domain and range?

------------------------------------------------------------------------
compute :: Char -> Char
------------------------------------------------------------------------

Here, the set `Char` is relatively small, say the set of all 32-bit words.
We make the input and output sets are identical so we only need to deal with
one type of `Char`; working around this restriction is left as an exercise.

A list of `Char` represents inputs and outputs of arbitrary length.

------------------------------------------------------------------------
type Value = [Char]
------------------------------------------------------------------------

The only thing our `compute` function can do to such a list is to perform the
same operation to each element of the list. We have modeled something like a
circuit, which performs the same task on a certain number of bits at a time.

== Deterministic Finite Automata ==

Humans and computers are so much more than mere circuits because we retain
something from one step to use in future steps. Even though we can only read a
few words at a time, we can absorb a complex story from a book. This is because
after starting in one state of mind, processing a few bits can put us in
another state of mind.

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

Briefly, we begin in some `start` state, then we update our state according to
the current state and the next input `Char`, and repeat until the entire input
has been processed. Certain states are considered accepting states, and we say
we accept the input if we wind up in one.

For example, the following DFA accepts strings consisting of an odd number
of 'x's (try `runDfa oddXs "xxxxx"`):

\begin{code}
oddXs :: Dfa
oddXs = (0, step, \state -> state == 1) where
  step 0 'x' = 1
  step 1 'x' = 0
  step _  _  = 2
\end{code}

By the way, we could have written:

------------------------------------------------------------------------
runDfa (start, step, accepts) = accepts . foldr (flip step) start
------------------------------------------------------------------------

but we prefer to avoid getting overly mixed up in Haskell library functions so
it looks more like we're starting from first principles.

We could extend the above to output characters as it computes (see
https://en.wikipedia.org/wiki/Moore_machine[Moore machines] and
https://en.wikipedia.org/wiki/Mealy_machine[Mealy machines]). But
traditionally, we start with nothing but a Boolean at the end because we
already learn a lot from this simple case. We quickly find DFAs are incapable
of elementary tasks such as checking if a bunch of parentheses are balanced, or
if some number of As has been followed by an equal number of Bs.

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

It turns out DPDAs are incapable of basic tasks such as checking if the input
is a palindrome, or if some number of As has been followed by an equal
number of Bs followed by an equal number of Cs.

== Turing Machines ==

Replacing the stack memory with a tape with a movable read/write head
leads to Turing Machines:

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

The tape starts with the input string surrounded on both sides by endless
blanks, with the read/write head pointing at the first character. After the
machine halts, we expect it to leave the output encoded in the same fashion.

We represent a blank with the space character, though in practice we'd
probably invent a special `Char` for this purpose.

\begin{code}
blanks :: OneWayTape
blanks = ' ' :> blanks

toTape :: [Char] -> Tape
toTape ""       = (blanks, ' ', blanks)
toTape (c:rest) = (blanks, c  , go rest) where
  go ""       = blanks
  go (c:rest) = c :> go rest

fromTape :: Tape -> [Char]
fromTape (_, c, rest) = go c rest where
  go c (x :> xs) = c:if x == ' ' then "" else go x xs

evalTM :: TM -> String -> String
evalTM p s = fromTape (runTM p (toTape s))
\end{code}

To simplify, we have ditched the notion of accepting states; we could trivially
support them by tweaking our TM to write a symbol representing acceptance
before halting.

https://en.wikipedia.org/wiki/Church%E2%80%93Turing_thesis[Turing machines
are a good mathematical answer to "What is a computer?"]
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
See
https://aiplaybook.a16z.com/reference-material/mccarthy-1960.pdf[McCarthy's paper on LISP], or
https://fi.ort.edu.uy/innovaportal/file/20124/1/22-landin_correspondence-between-algol-60-and-churchs-lambda-notation.pdf[Landin's paper on ALGOL 60].
This technique grew more mathematical over time, evolving into
https://en.wikipedia.org/wiki/Denotational_semantics[denotational semantics].

Incredibly, formal semantics exist for a substantial subset of C, and there
is even http://compcert.inria.fr/[a certified C compiler], that is, a C
compiler that a computer can prove is correct. Of course,
https://blog.regehr.org/archives/232[like all practical C compilers, they
ignored parts of the C standard that are abject nonsense].

Cryptographers eventually joined the fun in 1982, when Goldwasser and Micali
defined https://en.wikipedia.org/wiki/Semantic_security[semantic security].

https://www.youtube.com/watch?v=bmKYiUOEo2A[Conal Elliot gave an inspiring talk
on 'denotational design']. He advocates mathematical thinking beyond its
traditional strongholds, and in particular shows how it applies to manipulating
images. See also
https://www.youtube.com/watch?v=j3Q32brCUAI[Functional Reactive Programming]
and
https://www.youtube.com/watch?v=teRC_Lf61Gw[Functional Reactive Programming,
more elegantly specified].

https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems-final.pdf[Build systems can be defined mathematically].

Even operating systems have embraced mathematics. See
https://www.sigops.org/s/conferences/sosp/2009/papers/klein-sosp09.pdf[seL4]
and http://flint.cs.yale.edu/certikos/[CertiKOS].

https://en.wikipedia.org/wiki/Richard_Montague[Richard Montague] believed that
with enough research, we can show natural languages are no different to formal
languages, so perhaps there exists a non-mathematical path to precision.
