.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para logic differ atp fol pattern hilsys miranda Hol HolPro mvp module web

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.js eq.js differ.js atp.js douady.wasm douady.html fol.js fol.wasm fol.lhs cmpmira.tar.gz webby.wasm imp.wasm index.js

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
$(1): $(2) $(addsuffix .hs, $(addprefix inn/, $3));cat $(addsuffix .hs, $(addprefix inn/, $3)) | time ./$(2) > $$@
endef

define cat
$(1): $(addsuffix .hs, $(addprefix inn/, $2));cat $(addsuffix .hs, $(addprefix inn/, $2)) > $$@
endef

$(call party,multiparty.c,party,Base0 System Ast Map Parser Kiselyov Unify RTS Typer party)
$(call party,party1.c,multiparty,Base0 System Ast Map Parser Kiselyov Unify RTS Typer1 party)
$(call party,party2.c,party1,Base0 System Ast1 Map Parser1 Kiselyov Unify RTS1 Typer2 party)
$(call party,party3.c,party2,Base1 System1 Ast2 Map Parser2 Kiselyov1 Unify RTS2 Typer3 party1)
$(call party,crossly.c,party3,Base1 System1 Ast3 Map Parser3 Kiselyov1 Unify RTS3 Typer4 party2)
$(call party,precisely.c,crossly,BasePrecisely System1 AstPrecisely Map ParserPrecisely Kiselyov1 Unify RTS3 TyperPrecisely party2)
$(call party,traced.c,crossly,BasePrecisely System1 AstPrecisely Map ParserPrecisely Kiselyov1 Unify RTSTrace TyperPrecisely party2)

$(call party,check.c,precisely,BasePrecisely System1 AstPrecisely Map ParserPrecisely Kiselyov1 Unify RTS3 TyperPrecisely party2)

$(call party,webby.c,precisely,BasePrecisely System1 AstPrecisely Map ParserPrecisely Kiselyov1 Unify RTS3 TyperPrecisely Webby WartsBytes)
$(call party,webby.wasm,webby,BasePrecisely SystemWasm AstPrecisely Map ParserPrecisely Kiselyov1 Unify RTS3 TyperPrecisely Webby WartsBytes)

$(call party,imp.c,precisely wasm,BasePrecisely SystemWasm AstPrecisely Map ParserPrecisely Kiselyov1 Unify RTS3 TyperPrecisely Imp WartsBytes)
imp.o:imp.c;$(WCC) $^ -c -o $@
imp.wasm:imp.o;$(WLD) --initial-memory=41943040 --global-base=0 --no-gc-sections $^ -o $@

$(call cat,cat-party1.hs,Base0 System Ast Map Parser Kiselyov Unify RTS1 Typer1 party)
$(call cat,tmp.hs,BasePrecisely System1 AstPrecisely Map ParserPrecisely Kiselyov1 Unify RTS3 TyperPrecisely party2)

warts.c:crossly;cat inn/Base1.hs inn/SystemWasm.hs | ./crossly warts > $@
warts.o:warts.c;$(WCC) $^ -c -o $@
warts.wasm:warts.o;$(WLD) --initial-memory=41943040 --global-base=0 --no-gc-sections $^ -o $@
$(call party,warts2hs.c,crossly,Base1 System1 warts2hs)
inn/WartsBytes.hs:warts2hs warts.wasm;./$^ < warts.wasm > $@
$(call party,tabby.c,precisely,BasePrecisely System1 ../tabby)

hilsys.c:hilsys.lhs methodically;sed '/\\begin{code}/,/\\end{code}/!d;//d' $< | ./methodically > $@
test/mandelbrot.c:test/mandelbrot.hs lonely;(cat rts.c && ./lonely < $<) > $@
test/mandelbrot:test/mandelbrot.c

WCC=clang -O3 -c --target=wasm32 -Wall
ifeq ($(WASMLINK),)
WASMLINK=wasm-ld-11
endif
WLD=$(WASMLINK) --export-dynamic --allow-undefined --no-entry
wasm/douady.c:wasm/douady.hs lonely;(cat rts.c && ./lonely < $<) > $@
wasm/douady.o:wasm/douady.c;$(WCC) $^ -o $@
wasm/std.o:wasm/std.c;$(WCC) $^ -o $@
douady.wasm:wasm/std.o wasm/douady.o wasm/grow_memory_to.o;$(WLD) $^ -o $@
douady.html:douady.txt menu.html;cobble mathbook menu $<

index.html:index.lhs index.js imp.wasm hilsys.inc menu;cobble mathbook menu $<
hilsys.inc:hilsys.lhs;sed '1,/\\end{code}/d' $< | sed '/\\begin{code}/,/\\end{code}/!d;//d' > $@

site: $(SITE)

sync: site ; rsync -R -r $(SITE) crypto.stanford.edu:www/compiler/

clean: ; -rm $(SITE)

# One day, we might want to pin to a particular image: https://asterius.netlify.app/images.html
fol.js fol.wasm: fol.lhs
	mkdir -p fol-asterius
	cp fol.cabal fol.lhs fol-asterius/
	podman run -it --rm -v $(PWD)/fol-asterius/:/mirror -w /mirror docker.io/terrorjack/asterius ahc-link --bundle --browser --ghc-option -O --input-hs fol.lhs
	cd fol-asterius && cp fol.js fol.wasm ..

cmpmira.tar.gz: e4096.hs e4096.m q11.hs q11.m assembly.c rts.c; tar cfz $@ $^
