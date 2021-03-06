.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para logic differ atp fol pattern hilsys miranda Hol HolPro mvp module web

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.js eq.js differ.js atp.js douady.wasm douady.html fol.js fol.wasm fol.lhs cmpmira.tar.gz blah.wasm index.js

%.js: %.lhs ; -mv Main.jsmod /tmp; hastec --opt-all -Wall $^ && closure-compiler $@ > $@.clo && mv $@.clo $@

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
marginally.c:marginally.hs virtually;time ./virtually < $< > $@
methodically.c:methodically.hs marginally;time ./marginally < $< > $@
party.c:party.hs methodically;time ./methodically < $< > $@

define party
$(1).c: $(2) $(addsuffix .hs, $(addprefix inn/, $3));cat $(addsuffix .hs, $(addprefix inn/, $3)) | time ./$(2) > $$@
endef

define cat
cat-$(1).hs: $(addsuffix .hs, $(addprefix inn/, $2));cat $(addsuffix .hs, $(addprefix inn/, $2)) > $$@
endef

$(call party,multiparty,party,true.Base System Ast Map Parser Kiselyov Unify RTS Compiler party)
$(call party,party1,multiparty,true.Base System Ast Map Parser Kiselyov Unify RTS Compiler1 party)
$(call party,party2,party1,true.Base System1 Ast1 Map Parser1 Kiselyov Unify RTS1 Compiler2 party)
$(call party,party3,party2,true.Base1 System2 Ast2 Map Parser2 Kiselyov Unify RTS2 Compiler3 party1)
$(call party,party4,party3,true.Base1 System2 Ast3 Map Parser3 Kiselyov Unify RTS2 Compiler4 party2)

$(call cat,party1,true.Base System Ast Map Parser Kiselyov Unify RTS1 Compiler1 party)

crossly.c:crossly.hs methodically;time ./methodically < $< > $@
precisely.c:precisely.hs crossly;time ./crossly < $< > $@

hilsys.c:hilsys.lhs methodically;sed '/\\begin{code}/,/\\end{code}/!d;//d' $< | ./methodically > $@
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

wasm/env.c:crossly;./$< blah > $@
wasm/env.o:wasm/env.c;$(WCC) $^ -c -o $@
wasm/env.wasm:wasm/env.o;$(WLD) --initial-memory=41943040 --global-base=0 --no-gc-sections $^ -o $@

wasm/tmp.hs:wasm/blah.hs crossly wasm/env.wasm wasm/section; \
	(sed -n '/infix/,/Code generation/p' crossly.hs | sed '/^getContents =/d' \
	&& ./crossly coms && cd wasm && cat blah.hs && ./section < env.wasm) > $@
wasm/blah.c:wasm/tmp.hs crossly; ./crossly wasm < $< > $@
wasm/blah.o:wasm/blah.c;$(WCC) $^ -c -o $@
blah.wasm:wasm/blah.o;$(WLD) --initial-memory=41943040 --global-base=0 --no-gc-sections $^ -o $@
index.html:index.lhs index.js wasm/blah.pre blah.wasm hilsys.inc menu;cobble mathbook menu $<
hilsys.inc:hilsys.lhs;sed '1,/\\end{code}/d' $< | sed '/\\begin{code}/,/\\end{code}/!d;//d' > $@

site: $(SITE)

sync: site ; rsync -R -r $(SITE) crypto.stanford.edu:www/compiler/

clean: ; -rm $(SITE)

# One day, we might want to pin to a particular image: https://asterius.netlify.app/images.html
fol.js fol.wasm: fol.lhs
	mkdir -p fol-asterius
	cp fol.cabal fol.lhs fol-asterius/
	podman run -it --rm -v $(PWD)/fol-asterius/:/mirror -w /mirror terrorjack/asterius ahc-link --bundle --browser --input-hs fol.lhs
	cd fol-asterius && cp fol.js fol.wasm ..

cmpmira.tar.gz: e4096.hs e4096.m q11.hs q11.m assembly.c rts.c; tar cfz $@ $^
