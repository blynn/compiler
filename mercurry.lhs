= Mercurry =

Now is the time for all programming languages to insist on
Read-Eval-Print Loops (REPLs).

https://www.youtube.com/watch?v=8Ab3ArE8W3s[The edit-compile-run cycle is an
vestige of punched cards and human operators]. Computers have long been
powerful enough to provide an interactive environment. Indeed, decades ago, my
very first 8-bit home computer had a BASIC interpreter. I learned quickly from
https://tll.mit.edu/teaching-resources/assess-learning/how-to-give-feedback/[high-quality
immediate feedback].

Can we improve on old-school shells? Perhaps not always. I still fire up
https://en.wikipedia.org/wiki/Bash_(Unix_shell)[bash] for basic tasks.
Sometimes all I want is the simple and effective
https://en.wikipedia.org/wiki/Request%E2%80%93response[request-response] model.

However, the mass adoption of spreadsheets suggests that a richer interface may
often be better. Spreadsheets break free from the strict linear structure of a
command-line session. While spreadsheets are popularly associated with less
technical users, it seems programmers of all stripes could benefit from being
able to nimbly hop around while manipulating snippets of code and data.

Indeed, https://jupyter.org/[Jupyter Notebooks] show that sprucing up a humble
REPL can reap huge rewards. Their example inspires us to build a similar
web-based interactive development environment for our Haskell compiler, which
we call Mercurry.

Our Haskell compiler runs in the browser, so unlike Jupyter, Mercurry is small
and self-contained. There is no server doing the actual computation, which is
convenient, but also limits our power.

\begin{code}
fibs = 0:1:zipWith(+) fibs (tail fibs)

fibs!!10
fibs!!(fibs!!10)
\end{code}

Unlike GHCi, rather than one definition or statement or expression at a time,
we accept an arbitrary mix of them. This is almost the same, but it does mean
that if there is a parse error anywhere then nothing happens.

== AsciiDoctor ==

For marking up text, we use AsciiDoc instead of Markdown.

We rely on `asciidoctor.min.js` from

 * https://github.com/asciidoctor/asciidoctor.js/releases

By trial and error, I figured out that after including this script, the
following should be called:

----------------------------------------------------------------
Asciidoctor$$module$build$asciidoctor_browser();
----------------------------------------------------------------

and then
https://mrduguo.github.io/asciidoctor.org/docs/install-and-use-asciidoctorjs/[the
official instructions] work as advertised. For example:

----------------------------------------------------------------
element.innerHTML = Opal.Asciidoctor.$convert(src, Opal.hash2(['attributes'], {'attributes': ['showtitle']}));
----------------------------------------------------------------

== MathJax ==

When running an AsciiDoc cell, after calling AsciiDoctor, we call MathJax to
render equations.

I prefer indicating equations with just backslashes and parentheses or square
brackets to AsciiDoctor's more verbose syntax, but sometimes this causes issues
with unwanted AsciiDoc substitutions. When needed, we work around this issue
with AsciiDoc's passthrough mechanism.

When our page loads, our `init()` function renders AsciiDoc snippets with the
JavaScript port of AsciiDoctor, and calls MathJax to render any equations. We
ensure MathJax has already been initialized by calling `init()` after MathJax
is ready.

----------------------------------------------------------------
window.MathJax = {
  startup: {
    ready: () => {
      MathJax.startup.defaultReady();
      init();
    }
  }
};
----------------------------------------------------------------

However, another race arises. These lines only work if evaluated before loading
MathJax. On other pages, we load MathJax in the head section, so the browser
can fetch it in parallel as early as possible. For pages like this one, we load
MathJax last.

== Nonstandard Output ==

Tired of boring text output? Then call `jsEval` to, say, modify the HTML DOM.
The JavaScript variable `repl.outdiv` refers to a DIV element suitable for
placing output, as it is refreshed every run.

\begin{code}
jsEval [r|repl.outdiv.innerHTML = `<svg version="1.1" height="100" width="100" xmlns="http://www.w3.org/2000/svg"><circle r="45" cx="50" cy="50" fill="peachpuff" /></svg>`;|]
\end{code}
