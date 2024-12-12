async function run_runmes() {
  const stash = document.createElement("div");
  const cellmenu = document.createElement("div");
  cellmenu.classList.add("cellmenu");
  cellmenu.innerHTML = `<div class="cellmenukid"><div id="cellmenubuttons">
<span class='topbutton' id='playbutton'>&#x25b6;</span>
</div> </div>`;

  const repl = await mkRepl();
  repl.runCount = 0;
  function run(runme) {
    const s = runme.getElementsByClassName("incode")[0].innerText;
    const r = repl.run("chat", ["Main"], s);
    const oe = runme.getElementsByClassName("output")[0];
    oe.innerHTML = "";
    if (r.buf[0] == "error") {
      oe.innerHTML = "<div class='errmsg'></div>";
      oe.getElementsByClassName("errmsg")[0].textContent = r.out;
      return 1;
    } else if (r.buf[0] == "ok") {
      repl.runCount++;
      runme.getElementsByClassName("runcounter")[0].innerText = repl.runCount;
      if (r.out != "") {
        const oe = runme.getElementsByClassName("output")[0];
        oe.innerHTML = `<div style="display:flex;">
<span class="outlabel">[<span class="runcounter">` + repl.runCount + `</span>]:</span>
<pre class="outtext"></pre>
</div>
`;
        oe.getElementsByClassName("outtext")[0].textContent = r.out;
      }
    }
    return 0;
  }

  const runthese = document.getElementsByClassName("runme");
  for (const runme of runthese) {
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
        const b = document.getElementById("playbutton");
        b.addEventListener('click', ev => {
          stash.appendChild(cellmenu);
          run(runme);
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
            const status = run(runme);
            if (!status) {
              const x = runme.cloneNode(true);
              x.getElementsByClassName("output")[0].innerHTML = "";
              x.getElementsByClassName("runcounter")[0].innerText = " ";
              init_incode(x, "");
              runme.after(x);
              x.getElementsByClassName("incode")[0].focus();
            }
            ev.preventDefault();
          }
          break;
        }
      });
    }
    init_incode(runme, s);
    run(runme);
  }
}

run_runmes();
