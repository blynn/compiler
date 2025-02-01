let runme_out;

let curl_module;

async function run_runmes() {
  const stash = document.createElement("div");
  const cellmenu = document.createElement("div");
  cellmenu.classList.add("cellmenu");
  cellmenu.innerHTML = `<div class="cellmenukid"><div id="cellmenubuttons">
</div> </div>`;

  const repl = await mkRepl();
  let prom = undefined;
  curl_module = function(url) { prom = repl.fetch_module(url); }
  repl.runCount = 0;
  async function run(runme) {
    if (prom) {
      await prom;
      prom = undefined;
    }
    const s = runme.getElementsByClassName("incode")[0].innerText;
    const oe = runme.getElementsByClassName("output")[0];
    oe.innerHTML = "";
    runme_out = oe;
    const r = repl.run("chat", ["Main"], s + '\n');
    if (r.buf[0] == "error") {
      oe.innerHTML = "<div class='errmsg'></div>";
      oe.getElementsByClassName("errmsg")[0].textContent = r.out;
      return 1;
    } else if (r.buf[0] == "ok") {
      repl.runCount++;
      runme.getElementsByClassName("runcounter")[0].innerText = repl.runCount;
      if (r.out != "") {
        const div = document.createElement("div");
        div.style.display = "flex";
        div.innerHTML = `<span class="outlabel">[<span class="runcounter">` + repl.runCount + `</span>]:</span>
<pre class="outtext"></pre>`;
        oe.appendChild(div);
        oe.getElementsByClassName("outtext")[0].textContent = r.out;
      }
    }
    return 0;
  }

  async function interpret(runme) {
    const s = runme.innerText;
    runme.innerHTML = `<div style="display:flex;">
<span class="inlabel">[<span class="runcounter"> </span>]:</span>
<div class="incode" contenteditable="plaintext-only" spellcheck="false"></div>
</div>
<div class="output"></div>
`;
    function init_incode(runme, s) {
      const inco = runme.getElementsByClassName("incode")[0];
      inco.innerText = s;
      inco.addEventListener('focus', ev => {
        runme.before(cellmenu);
        document.getElementById("cellmenubuttons").innerHTML =
`<span class='topbutton' id='playbutton'>&#x25b6;</span>`;
        document.getElementById("playbutton").addEventListener('click', ev => {
          stash.appendChild(cellmenu);
          run(runme);
          ev.preventDefault();
        });
      });
      inco.addEventListener('keydown', ev => {
        switch(ev.keyCode) {
        case 13:
          if (ev.ctrlKey) {
            ev.preventDefault();
            run(runme);
            /*
          } else if (ev.shiftKey) {
            // Shift + Enter
            ev.preventDefault();
            */
          } else if (ev.altKey) {
            run(runme).then((status) => {if (!status) {
              const x = runme.cloneNode(true);
              x.getElementsByClassName("output")[0].innerHTML = "";
              x.getElementsByClassName("runcounter")[0].innerText = " ";
              init_incode(x, "");
              runme.after(x);
              x.getElementsByClassName("incode")[0].focus();
            }});
            ev.preventDefault();
          }
          break;
        }
      });
    }
    init_incode(runme, s);
    await run(runme);
  }

  let runmeClass = "runme";
  for(;;) {
    const runthese = document.getElementsByClassName(runmeClass);
    if (runthese.length == 0) {
      break;
    }
    for (const runme of runthese) await interpret(runme);
    runmeClass += "'";
  }
}

run_runmes();
