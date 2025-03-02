.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para logic differ atp fol pattern hilsys miranda Hol HolPro mvp module web mercurry

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.wasm eq.js differ.wasm atp.wasm douady.wasm douady.html fol.js fol.wasm fol.lhs webby.wasm imp.wasm doh.wasm merc-main.js merc.css runme.css runme.js reply.js \
     cmpmira.tar.gz \
     Charser.ob Map.ob

BCS_HS=inn/BasePrecisely.hs inn/SystemWasm.hs inn/Charser.hs

para.c: para.lhs precisely $(BCS_HS); (./unlit < para.lhs && cat $(BCS_HS)) | ./precisely wasm > $@
differ.c: differ.lhs precisely $(BCS_HS); (./unlit < differ.lhs && cat $(BCS_HS)) | ./precisely wasm > $@
atp.c: atp.lhs precisely $(BCS_HS); (./unlit < atp.lhs && cat $(BCS_HS)) | ./precisely wasm > $@

%.wasm.o: %.c; clang --target=wasm32 -O2 -ffreestanding -c $^ -o $@
%.wasm: %.wasm.o; wasm-ld --import-undefined --no-entry --initial-memory=838860800 $^ -o $@

eq.js: eq.lhs ; -mv Main.jsmod /tmp; hastec --opt-all -Wall $^ && closure-compiler $@ > $@.clo && mv $@.clo $@

menu.html: menu; ./stitch menu menu

%.html: %.merc;MERC=1 stitch book menu $<
%.html: %.lhs menu.html; ./stitch book menu $<
%.html: %.txt menu.html; ./stitch book menu $<
%:%.c;clang -O3 $^ -o $@
webby:webby.c;clang -O3 $^ -o $@ -lm
precisely:precisely.c;clang -O3 $^ -o $@ -lm
check:check.c;clang -O3 $^ -o $@ -lm
reply:reply.c;clang -O3 $^ -o $@ -lm

singularity.boot:singularity bootsingularity.sh;./bootsingularity.sh > $@
vm:vm.c
raw:vm singularity.boot;./vm > raw
lonely.c:vm effectively.hs lonely.hs rts.c raw;(cat rts.c && ./vm run effectively.hs < lonely.hs) > lonely.c
lonely:lonely.c

define rtsup
$(1).c: $(2) $(1).hs rts.c;(cat rts.c && time ./$(2) < $(1).hs) > $$@
endef

COMMON=AstPrecisely BasePrecisely KiselyovPrecisely Map1 ParserPrecisely RTSPrecisely TyperPrecisely Unify1 Charser Obj
COMMONHS=$(addprefix inn/, $(addsuffix .hs, $(COMMON)))
COMMONOB=$(addsuffix .ob, Ast Base Kiselyov Map Parser RTS Typer Unify Charser Obj)

SHELL=/usr/bin/env bash

%.ob:precisely inn/%.hs;sh <(cat $(COMMONHS) | ./precisely obj)

reply.c: inn/System.hs inn/ReplyImports.hs inn/ObjMapImports.hs inn/reply.hs inn/reply-native.hs $(COMMONOB); (cat $^ | ./precisely ; cat inn/introspect.c; cat inn/System.hs inn/ReplyImports.hs inn/ObjMapImports.hs Base.ob | ./precisely objc) > $@

DOHSYS=inn/SystemWasm.hs inn/SystemArg.hs inn/ReplyImports.hs inn/ObjMapImports.hs

doh.c: $(DOHSYS) inn/reply.hs inn/reply-wasm.hs $(COMMONOB); (cat $^ | ./precisely wasm ; cat inn/introspect.c ; cat $(DOHSYS) Base.ob | ./precisely objc) > $@

doh.html:doh.txt menu.html;./stitch book menu $<

chat.html:chat.txt menu.html;./stitch book menu $<

$(call rtsup,patty,lonely)
$(call rtsup,guardedly,patty)
$(call rtsup,assembly,guardedly)
$(call rtsup,mutually,assembly)
$(call rtsup,virtually,mutually)
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
$(call party,party1.c,multiparty,Base0 System Ast1 Map Parser1 Kiselyov Unify1 RTS Typer1 party)
$(call party,party2.c,party1,Base1 System Ast2 Map Parser2 Kiselyov Unify1 RTS1 Typer2 party1)
$(call party,crossly.c,party2,Base1 System Ast3 Map Parser3 Kiselyov Unify1 RTS2 Typer3 party2)
$(call party,crossly1.c,crossly,Base2 System AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely Obj Charser precisely)
$(call party,precisely.c,crossly1,BasePrecisely System AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely Obj Charser precisely)

$(call party,check.c,precisely,BasePrecisely System AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely precisely)

webby.c:inn/System.hs inn/Webby.hs inn/WartsBytes.hs $(COMMONOB) | precisely; cat $^ | ./precisely > $@
webby.wasm:inn/SystemWasm.hs inn/Webby.hs inn/WartsBytes.hs $(COMMONOB) | webby; cat $^ | ./webby > $@

imp.c:inn/SystemWasm.hs inn/Imp.hs inn/WartsBytes.hs $(COMMONOB); cat $^ | ./precisely wasm > $@

warts.c:precisely;cat inn/BasePrecisely.hs inn/SystemWasm.hs | ./precisely warts > $@
$(call party,warts2hs.c,crossly,Base1 System warts2hs)
inn/WartsBytes.hs:warts2hs warts.wasm;./$^ < warts.wasm > $@
$(call party,tabby.c,precisely,BasePrecisely System ../tabby)

hilsys.c:hilsys.lhs methodically;sed '/\\begin{code}/,/\\end{code}/!d;//d' $< | ./methodically > $@
test/mandelbrot.c:test/mandelbrot.hs lonely;(cat rts.c && ./lonely < $<) > $@
test/mandelbrot:test/mandelbrot.c

WCC=clang -O3 -c --target=wasm32 -Wall
ifeq ($(WASMLINK),)
WASMLINK=wasm-ld
endif
WLD=$(WASMLINK) --import-undefined --no-entry
wasm/douady.c:wasm/douady.hs lonely;(cat rts.c && ./lonely < $<) > $@
wasm/douady.o:wasm/douady.c;$(WCC) $^ -o $@
wasm/std.o:wasm/std.c;$(WCC) $^ -o $@
douady.wasm:wasm/std.o wasm/douady.o wasm/grow_memory_to.o;$(WLD) $^ -o $@
douady.html:douady.txt menu.html;./stitch book menu $<

index.html:index.lhs imp.wasm hilsys.inc menu;./stitch book menu $<
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

%.merc: %.lhs mercer;sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g;' $<|../compiler/mercer>$@
mercurry.html:mercurry.merc

mercer.c:inn/BasePrecisely.hs inn/System.hs mercer.hs;cat $^ | ./precisely > $@
mercer:mercer.c;clang -O3 $^ -o $@ -lm
