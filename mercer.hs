module Main where
import Base
import System

beginAdoc = [r|<div class="cell" data-type="AsciiDoc"><div style="display:flex;">
<span class="inlabel"></span>
<pre class="incode" spellcheck="false" contenteditable="" style="display: inline-block;">|]

beginCode = [r|<div class="cell" data-type="Code"><div style="display:flex;">
<span class="inlabel">[<span class="runcounter"> </span>]:</span>
<pre class="incode" spellcheck="false" contenteditable="" style="display: inline-block;">|]

endCell = "</div></div>"

header = [r|
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<div id="topmenu" style="border-bottom: solid 1px black;">
<dialog id="importdialog" class="topdialog">Import:
  <input id="importfile" type="file" /><button id="importOKButton">OK</button>
</dialog>
</div>
<div id="convo" class="convo">|]

footer = [r|</div>
<div style="display:none;" id="cellmenuclipboard">
<!-- https://stackoverflow.com/questions/6040005/relatively-position-an-element-without-it-taking-up-space-in-document-flow -->
<div style="float:right;height:0;" id="cellmenu">
<div style="position:relative;right:0.5em;bottom:1.5em;
background-color:white;padding:2px;
border:1px solid lightgrey;border-radius:4px;
font-family:'Open Sans',sans-serif;
">
<div id="cellmenubuttons"></div>

<div id="popuptypemenu" class="popup">
Select type:
<hr>
</div>

</div>
</div>
</div>

<script src="../asciidoctor.min.js"></script>
<script src="../compiler/merc-main.js"></script>
<script>
include::../compiler/reply.js[]

async function init() {
  await mercInit
    ( ev => { loadConvo(localStorage.getItem("content")); }
    , ev => { localStorage.setItem("content", saveConvo()); }
    );
  // Remove text nodes.
  [...convo.childNodes].forEach(c => c.nodeType != 1 && c.remove());
  for (const c of convo.childNodes) {
    c.addEventListener('click', ev => {select(c);});
    cursor = c;
    runOnly();
  }
  cursor = undefined;
}
window.MathJax = {
  startup: {
    ready: () => {
      MathJax.startup.defaultReady();
      init();
    }
  }
};
</script>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
|]

main = do
  interact $ unlines . merc . lines
  putStr footer

merc ls@(h:_) = duph1 h $ (header:) $ go beginAdoc ls

duph1 = \case
  h@('=':' ':_) -> (h:)
  _ -> id

go opener = \case
  [] -> [endCell]
  h : t
    | h == "[cloak]" -> dashesOpen (endCell ++ beginCode) t
    | h == "\\begin{code}" -> go (endCell ++ beginCode) t
    | h == "\\end{code}" -> go (endCell ++ beginAdoc) t
    | opener == "" -> h : go opener t
    | h == "" -> go opener t
    | otherwise -> (opener ++ h) : go "" t

dashesOpen opener = \case
  h : t | all (== '-') h -> opener : dashesClose (length h) t
  s -> go (endCell ++ beginAdoc) s

dashesClose n = \case
  h : t
    | length h == n, all (== '-') h -> go (endCell ++ beginAdoc) t
    | otherwise -> h : dashesClose n t
  [] -> [endCell]
