= Client-side compiler =

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
++++++++++

We build a browser-based edition of our compiler. To get it off the ground as
quickly as possible, we settle for an environment that only provides input
and output strings, that is, the only impure functions at our disposal are
`putchar`, `getchar`, and `eof`.

== Cross-compiler ==

We add an option to our compiler to produce C that targets WebAssembly:

  * We add a minimal `malloc` implementation.

  * We add a check in `rts_reduce` to call `rts_init` the first time it is
  invoked.

This compiler also supports top-level type annotations, though only partially
checks the predicates if a context is supplied.

We rename `div` and `mod` to `quot` and `rem`, then introduce wrappers for
`div` and `mod`. Now our divisions behave correctly, though it is sad that
`div` and `mod` need more instructions. (FORTRAN set an unfortunate precedent
of truncating division to zero, ultimately
https://github.com/WebAssembly/design/issues/250[forcing languages like C and
WebAssembly and even hardware to conform].)

++++++++++
<p><a onclick='hideshow("crossly");'>&#9654; Toggle `crossly.hs`</a></p>
<div id='crossly' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::crossly.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

== WasmArmyKnife ==

It'd be nice to write code to generate wasm, but for expedience, we compile our
runtime system to a wasm, then modify the binary. WebAssembly turns out to be
pleasantly malleable. A binary breaks up into independent sections, and we can
add, delete, or modify sections before stitching them together again. We write
a tool to do this.

++++++++++
<p><a onclick='hideshow("WasmArmyKnife");'>&#9654; Toggle `WasmArmyKnife.hs`</a></p>
<div id='WasmArmyKnife' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::wasm/WasmArmyKnife.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

As its name suggests, our WasmArmyKnife is no substitute for a dedicated tool
but it's handy for quick and dirty jobs.

== Browser-based compiler ==

When given the `demo` command, our `crossly` compiler generates a specialized
RTS:

------------------------------------------------------------------------
$ ./crossly demo
------------------------------------------------------------------------

  * We remove all mallocs and hardcode particular memory addresses for various
  arrays.

  * We export a function named `fun` that reduces a particular address on the heap.

  * Instead of the C library, we expect 3 imports:

    1. `env.putchar`: Programs call this to output characters.
    2. `env.getchar`: Unlike C, but like Haskell, this should throw
    an error if called on end of input.
    3. `env.eof`: Returns 1 at the end of input. Unlike C, but like Haskell,
    if at the end of input, this should indicate the end of input before
    any attempt to read past the end (which would throw).

After compiling to wasm, it remains to add a data section describing the
initial heap contents. Our WasmArmyKnife extracts the relevant sections to
embed in our client-side compiler.

++++++++++
<p><a onclick='hideshow("section");'>&#9654; Toggle `section`</a></p>
<div id='section' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::wasm/section[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++

We append the output of this script to a variant of our compiler with a few changes:

  * We switch out the C code generator with some code that pastes together wasm
  sections and inserts a data section describing the heap contents.
  The `main` function is replaced with exports.

  * We replace all the foreign imports with imports corresponding to the
  imports above: `putChar`, `getChar`, and `isEOFInt`. Since we use Church
  booleans, we cannot directly use the `env.eof` import and instead define a
  thin `isEOF` wrapper around it. We rewrite `getContents` to match.

  * We also pre-declare `putChar`, `getChar`, and `isEOFInt` so our input
  programs have no need to declare any foreign imports.

A `sed` script in the `Makefile` extracts the parts of `crossly.hs` we need
and appends the following.

++++++++++
<p><a onclick='hideshow("blah");'>&#9654; Toggle `blah.hs`</a></p>
<div id='blah' style='display:none'>
++++++++++

------------------------------------------------------------------------
include::wasm/blah.hs[]
------------------------------------------------------------------------

++++++++++
</div>
++++++++++
