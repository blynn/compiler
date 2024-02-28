// Why must I run this? What does it do?
Asciidoctor$$module$build$asciidoctor_browser();

let cursor;
let runCount = 0;

function addcellmenu(s, f) {
  const span = document.createElement("span");
  span.classList.add("cellbutton");
  span.innerHTML = s;
  cellmenubuttons.appendChild(span);
  span.addEventListener("click", ev => { f(); ev.stopPropagation(); });
}

addcellmenu("?", ev => {
  popuptypemenu.style.display = "block";
  document.addEventListener("click", ev => {
    popuptypemenu.style.display = "none";
    ev.preventDefault();
  }, {once:true});
});
const cellmenutype = cellmenubuttons.lastChild;
cellmenutype.style["text-align"] = "left";
cellmenutype.style.width = "6em";

const tyCode = "Code";
const tyRaw = "Raw"
const tyQuack = "AsciiDoc";

function setType(x, t) {
  if (t == x.getAttribute("data-type")) return;
  x.setAttribute("data-type", t);
  if (t == tyRaw || t == tyQuack) {
    x.getElementsByClassName("inlabel")[0].innerHTML = "";
    const out = x.getElementsByClassName("output")[0];
    if (out) out.remove();
  } else if (t == tyCode) {
    x.getElementsByClassName("inlabel")[0].innerHTML = `[<span class="runcounter"> </span>]:`;
  }
}

function addType(t) {
  const div = document.createElement("div");
  div.classList.add("typemenuitem");
  div.innerHTML = t;
  div.addEventListener("click", ev => setType(cursor, t));
  popuptypemenu.appendChild(div);
}

addType(tyCode);
addType(tyRaw);
addType(tyQuack);

addcellmenu("&#x29c9;", ev => {
  cellmenuclipboard.appendChild(cellmenu);
  const n = newCell();
  n.innerHTML = cursor.innerHTML;
  cursor.after(n);
  select(n);
  document.activeElement.blur();
});
addcellmenu("&#x2b06;", ev => {
  const prev = cursor.previousSibling;
  if (prev) prev.before(cursor);
});
addcellmenu("&#x2b07;", ev => {
  const next = cursor.nextSibling;
  if (next) next.after(cursor);
});
addcellmenu("&#x1f446;", ev => insertCell());
addcellmenu("&#x1f447;", ev => appendCell());
// &#x1f5d1; is long.
addcellmenu("&#10006;", ev => {
  cellmenuclipboard.appendChild(cellmenu);
  const next = cursor.nextSibling;
  if (next) {
    cursor.remove();
    select(next);
  } else {
    const prev = cursor.previousSibling;
    cursor.remove();
    if (prev) {
      select(prev);
    } else {
      select(newCell());
      convo.appendChild(cursor);
    }
  }
});

function insertCell() {
  const n = newCell();
  cursor.before(n);
  select(n);
}
function appendCell() {
  const n = newCell();
  cursor.after(n);
  select(n);
}

function select(cell) {
  if (cursor) cursor.classList.remove("selectedcell");
  cursor = cell;
  cursor.classList.add("selectedcell");
  cellmenutype.innerHTML = cursor.getAttribute("data-type");
const tri = document.createElement("div");
tri.innerHTML = "&#x25BE;";
tri.style.float = "right";
cellmenutype.appendChild(tri);

  cursor.prepend(cellmenu);
}

function newCell() {
  const div = document.createElement("div");
  div.classList.add("cell");
  div.setAttribute("data-type", tyCode);
  div.innerHTML =
`<div style="display:flex;">
<span class="inlabel">[<span class="runcounter"> </span>]:</span>
<pre class="incode" spellcheck=false contenteditable></pre>
</div>`
  div.addEventListener('click', ev => {select(div);});
  const incode = div.getElementsByClassName("incode")[0];

  incode.addEventListener('copy', function(e){
  e.clipboardData.setData('text/plain', window.getSelection().toString());
  e.preventDefault();
});
  return div;
}

let repl;

function outhtml(s) {
  const div = document.createElement("div");
  div.style["margin-left"] = "4em";
  div.innerHTML = s;
  repl.outdiv.appendChild(div);
}

function runOnly() {
  if (!cursor) return;
  const ty = cursor.getAttribute("data-type");
  if (ty == tyRaw) return;
  const incode = cursor.getElementsByClassName("incode")[0];
  const s = incode.innerText;
  incode.textContent = s;
  const out = cursor.getElementsByClassName("output");
  if (out.length != 0) out[0].remove();
  if (s == "") return;

  const div = document.createElement("div");
  repl.outdiv = div;
  div.classList.add("output");
  cursor.appendChild(div);

  if (ty == tyQuack) {
    // https://mrduguo.github.io/asciidoctor.org/docs/install-and-use-asciidoctorjs/
    div.innerHTML = `<span class="outlabel"></span>`;
    const adoc = document.createElement("div");
    adoc.classList.add("adoc");
    adoc.innerHTML = Opal.Asciidoctor.$convert(s, Opal.hash2(['attributes'], {'attributes': ['showtitle']}));
    MathJax.typeset([adoc]);
    div.appendChild(adoc);
    incode.style.display = "none";
    return;
  }

  const r = repl.run("chat", ["Main"], s + "\n");
  runCount++;
  cursor.getElementsByClassName("runcounter")[0].innerText = runCount;
  document.activeElement.blur();
  if (r.buf[0] == "error") {
    div.classList.add("errmsg");
    div.textContent = r.out;
    return;
  }
  if (r.out != "") {
    div.innerHTML =
`<span class="outlabel">[` + runCount + `]:</span>
<pre class="outtext"></pre>`;
    div.getElementsByClassName("outtext")[0].innerText = r.out;
  }
}

function runThenSelect() {
  runOnly();
  const next = cursor.nextSibling;
  if (next) select(next); else appendCell();
}
function saveConvo() {
  const p = cellmenu.parentElement;
  cellmenuclipboard.appendChild(cellmenu);
  const s = convo.innerHTML;
  p.prepend(cellmenu);
  return s;
}
function loadConvo(s) {
  cellmenuclipboard.appendChild(cellmenu);
  convo.innerHTML = s.trim();
  const cells = convo.getElementsByClassName("cell");
  for (const c of cells) {
    c.addEventListener('click', function(ev){select(c);});
    if (c.classList.contains("selectedcell")) cursor = c;
  }
  MathJax.typeset();
}

function addTopButton(s, f) {
  const b = document.createElement("span");
  b.classList.add("topbutton");
  b.innerHTML = s;
  b.addEventListener('click', f);
  topmenu.appendChild(b);
  return b;
}

async function mercInit(loadFun, saveFun) {
  repl = await mkRepl();
  select(newCell());
  convo.appendChild(cursor);
  addTopButton("&#x1F4BE;", saveFun);
  addTopButton("&#x1F4C2;", loadFun);
  addTopButton("&#x21BA;", async function(ev){
    await repl.reset();
    runCount = 0;
  });
  addTopButton("&#x23f5;", ev => {
    const bak = cursor;
    const cells = convo.getElementsByClassName("cell");
    for (const c of cells) {
      cursor = c;
      runOnly();
    }
    cursor = bak;
  });
  addTopButton("&#x2b71;", ev => {
    importdialog.showModal();
    ev.stopPropagation();
    document.addEventListener("click", ev => {
      if (ev.target == importdialog) {
        importdialog.close();
      }
    });
  });
  importOKButton.addEventListener('click', async function(ev) {
    const [file] = importfile.files;
    if (file) loadConvo(await file.text());
    importdialog.close();
  });
  addTopButton("&#x2b73;", ev => {
    const file = new Blob([saveConvo()], {type: "application/octet-stream"});
    const a = document.createElement("a"), url = URL.createObjectURL(file);
    a.href = url;
    a.download = "export.merc";
    document.body.appendChild(a);
    a.click();
    setTimeout(function() {
        document.body.removeChild(a);
        window.URL.revokeObjectURL(url);
    }, 0);
  });
  document.addEventListener('keydown', ev => {
    if (!cursor) return;
    switch(ev.keyCode) {
    case 13:
      if (ev.ctrlKey) {
        runOnly();
        ev.preventDefault();
      } else if (ev.shiftKey) {
        runThenSelect();
        ev.preventDefault();
      } else if (ev.altKey) {
        runOnly();
        appendCell();
        ev.preventDefault();
      } else {
        if (document.activeElement === document.body) {
          const incode = cursor.getElementsByClassName("incode")[0];
          if (cursor.getAttribute("data-type") == tyQuack) {
            cursor.getElementsByClassName("output")[0].remove();
            incode.style.display = "inline-block";
          }
          incode.focus();
          ev.preventDefault();
        }
      }
      break;
    case 27:
      if (document.activeElement !== document.body) {
        document.activeElement.blur();
        ev.preventDefault();
      }
      break;
    case 38:
      if (document.activeElement === document.body) {
        const prev = cursor.previousSibling;
        if (prev) {
          if (ev.ctrlKey && ev.shiftKey) {
            prev.before(cursor);
          } else {
            select(prev);
          }
          cursor.scrollIntoView(true);
        }
        ev.preventDefault();
      }
      break;
    case 40:
      if (document.activeElement === document.body) {
        const next = cursor.nextSibling;
        if (next) {
          if (ev.ctrlKey && ev.shiftKey) {
            next.after(cursor);
          } else {
            select(next);
          }
          cursor.scrollIntoView(false);
        }
        ev.preventDefault();
      }
      break;
    case 65:
      if (document.activeElement === document.body) {
        insertCell();
        ev.preventDefault();
      }
      break;
    case 66:
      if (document.activeElement === document.body) {
        appendCell();
        ev.preventDefault();
      }
      break;
    }
  });
}
