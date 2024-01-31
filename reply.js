"use strict";
function teen(s) { return (new TextEncoder()).encode(s); }
function tede(s) { return (new TextDecoder()).decode(s); }
function tedea(a) { return (new TextDecoder()).decode(Uint8Array.from(a)); }

async function mkRepl() {
  const repl = {};
  repl.runBlob = function(f, args, blob) {
      repl.args = args.map(teen);
      repl.buf = [];
      repl.out = [];
      repl.inp = blob;
      repl.cursor = 0;
      repl.instance.exports[f]();
      repl.eval_in = [], repl.eval_out = [];
      return {
        buf : repl.buf.map(tedea),
        out : tedea(repl.out),
      }
    };
  repl.run = function(f, args, s) { return repl.runBlob(f, args, teen(s)); }
  repl.instance = (await WebAssembly.instantiateStreaming(fetch('../compiler/doh.wasm'), {env:
    { putchar: c  => repl.out.push(c)
    , eof    : () => repl.cursor == repl.inp.length
    , getchar: () => repl.inp[repl.cursor++]
    , nextout: () => { repl.buf.push(repl.out); repl.out = []; }
    , argc   : () => repl.args.length
    , argvlen: i => repl.args[i].length
    , argvat : (i, j) => repl.args[i][j]

    , eval_put : c  => repl.eval_in.push(c)
    , eval_run : () => {
        repl.eval_out = teen(eval(tedea(repl.eval_in)));
        repl.eval_in = [];
      }
    , eval_size: () => repl.eval_out.length
    , eval_at:   i  => repl.eval_out[i]
    }})).instance;

  repl.run("chat_new", ["Main"], "");
  return repl;
}
