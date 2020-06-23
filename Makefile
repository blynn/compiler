.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para logic differ atp fol pattern hilsys miranda Hol HolPro

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.js eq.js differ.js atp.js douady.wasm douady.html *.mjs fol.wasm fol.lhs cmpmira.tar.gz blah.wasm index.js

%.js: %.lhs ; -mv Main.jsmod /tmp; hastec --opt-all -Wall $^; closure-compiler $@ > $@.clo; mv $@.clo $@

menu.html: menu; cobble menu menu

%.html: %.lhs menu.html; cobble mathbook menu $<
%:%.c;clang -O3 $^ -o $@

vm:vm.c
raw:vm;./vm > raw
lonely.c:vm effectively.hs lonely.hs rts.c raw;(cat rts.c && ./vm run effectively.hs < lonely.hs) > lonely.c
lonely:lonely.c

define rtsup
$(1).c: $(2) $(1).hs rts.c;(cat rts.c && time ./$(2) < $(1).hs) > $$@
endef

$(call rtsup,internally,uniquely)
$(call rtsup,patty,lonely)
$(call rtsup,guardedly,patty)
$(call rtsup,assembly,guardedly)
$(call rtsup,mutually,assembly)
$(call rtsup,uniquely,mutually)
$(call rtsup,virtually,uniquely)
marginally.c:marginally.hs virtually;./virtually < $< > $@
methodically.c:methodically.hs marginally;./marginally < $< > $@

hilsys.c:guardedly hilsys.lhs rts.c;(cat rts.c && sed '/\\begin{code}/,/\\end{code}/!d;//d' hilsys.lhs | ./guardedly) > $@
test/mandelbrot.c:test/mandelbrot.hs lonely;(cat rts.c && ./lonely < $<) > $@
test/mandelbrot:test/mandelbrot.c

WCC=clang -O3 -c --target=wasm32 -Wall
ifeq ($(WASMLINK),)
WASMLINK=wasm-ld-10
endif
WLD=$(WASMLINK) --export-dynamic --allow-undefined --no-entry
wasm/douady.c:wasm/douady.hs lonely;(cat rts.c && ./lonely < $<) > $@
wasm/douady.o:wasm/douady.c;$(WCC) $^ -o $@
wasm/std.o:wasm/std.c;$(WCC) $^ -o $@
douady.wasm:wasm/std.o wasm/douady.o wasm/grow_memory_to.o;$(WLD) $^ -o $@
douady.html:douady.txt menu.html;cobble mathbook menu $<

wasm/env.o:wasm/env.c;$(WCC) $^ -c -o $@
wasm/env.wasm:wasm/env.o;$(WLD) --initial-memory=41943040 --global-base=0 --no-gc-sections $^ -o $@
wasm/cross.c:wasm/cross.hs methodically;./methodically < $< > $@
wasm/blah.c:wasm/blah.hs wasm/cross wasm/env.wasm wasm/section;cd wasm && (cat blah.hs && ./section < env.wasm) | ./cross > blah.c
wasm/blah.o:wasm/blah.c;$(WCC) $^ -c -o $@
blah.wasm:wasm/blah.o;$(WLD) --initial-memory=41943040 --global-base=0 --no-gc-sections $^ -o $@
index.html:index.lhs wasm/blah.pre blah.wasm;cobble mathbook menu $<

site: $(SITE)

sync: site ; rsync -R -r $(SITE) crypto.stanford.edu:www/compiler/

clean: ; -rm $(SITE)

fol.mjs fol.wasm: fol.lhs; mkdir -p fol-asterius && cp fol.cabal fol.lhs fol-asterius/ && docker run -it --rm -v $(PWD):/mirror -w /mirror terrorjack/asterius ./build-fol && cp -r fol-asterius/fol.wasm fol-asterius/*.mjs .

cmpmira.tar.gz: e4096.hs e4096.m q11.hs q11.m assembly.c rts.c; tar cfz $@ $^
