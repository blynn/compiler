.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para logic differ atp fol pattern hilsys miranda

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.js eq.js differ.js atp.js douady.wasm douady.html *.mjs fol.wasm fol.lhs browser cmpmira.tar.gz

%.js: %.lhs ; -mv Main.jsmod /tmp; hastec --opt-all -Wall $^; closure-compiler $@ > $@.clo; mv $@.clo $@

menu.html: menu; cobble menu menu

%.html: %.lhs menu.html; cobble mathbook menu $<
%:%.c;clang -O3 $^ -o $@

vm:vm.c
raw:vm;./vm > raw
lonely.c:vm effectively.hs lonely.hs rts.c raw;(cat rts.c && ./vm run effectively.hs < lonely.hs) > lonely.c
lonely:lonely.c
patty.c:lonely patty.hs rts.c;(cat rts.c && time ./lonely < patty.hs) > $@
patty:patty.c
guardedly.c:patty guardedly.hs rts.c;(cat rts.c && time ./patty < guardedly.hs) > $@
guardedly:guardedly.c
assembly.c:guardedly assembly.hs rts.c assembly.coda.c;(cat rts.c && time ./guardedly < assembly.hs && cat assembly.coda.c) > $@
assembly:assembly.c
mutually.c:assembly mutually.hs rts.c assembly.coda.c;(cat rts.c && time ./assembly < mutually.hs && cat assembly.coda.c) > $@
mutually:mutually.c
virtually.c:mutually virtually.hs rts.c assembly.coda.c;(cat rts.c && time ./mutually < virtually.hs && cat assembly.coda.c) > $@
virtually:virtually.c
uniquely.c:mutually uniquely.hs rts.c assembly.coda.c;(cat rts.c && time ./mutually < uniquely.hs && cat assembly.coda.c) > $@
uniquely:uniquely.c
hilsys.c:guardedly hilsys.lhs rts.c;(cat rts.c && sed '/\\begin{code}/,/\\end{code}/!d;//d' hilsys.lhs | ./guardedly) > $@
test/mandelbrot.c:test/mandelbrot.hs lonely;(cat rts.c && ./lonely < $<) > $@
test/mandelbrot:test/mandelbrot.c

WCC=clang -O3 -c --target=wasm32 -Wall
WLD=wasm-ld-8 --export-dynamic --allow-undefined --no-entry
wasm/douady.c:wasm/douady.hs lonely;(cat rts.c && ./lonely < $<) > $@
wasm/douady.o:wasm/douady.c;$(WCC) $^ -o $@
wasm/std.o:wasm/std.c;$(WCC) $^ -o $@
douady.wasm:wasm/std.o wasm/douady.o wasm/grow_memory_to.o;$(WLD) $^ -o $@
douady.html:douady.txt menu.html;cobble mathbook menu $<

site: $(SITE)

sync: site ; rsync -R -r $(SITE) crypto.stanford.edu:www/compiler/

clean: ; -rm $(SITE)

fol.mjs fol.wasm: fol.lhs; mkdir -p fol-asterius && cp fol.cabal fol.lhs fol-asterius/ && docker run -it --rm -v $(PWD):/mirror -w /mirror terrorjack/asterius ./build-fol && cp -r fol-asterius/browser fol-asterius/fol.wasm fol-asterius/*.mjs .

cmpmira.tar.gz: e4096.hs e4096.m q11.hs q11.m assembly.c rts.c; tar cfz $@ $^
