.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para logic differ atp fol pattern hilsys miranda Hol HolPro mvp module web

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.wasm eq.js differ.wasm atp.wasm douady.wasm douady.html fol.js fol.wasm fol.lhs cmpmira.tar.gz webby.wasm imp.wasm

BCS_HS=inn/BasePrecisely.hs inn/SystemWasm.hs inn/Charser.hs

para.c: para.lhs precisely $(BCS_HS); (./unlit < para.lhs && cat $(BCS_HS)) | ./precisely wasm > $@
differ.c: differ.lhs precisely $(BCS_HS); (./unlit < differ.lhs && cat $(BCS_HS)) | ./precisely wasm > $@
atp.c: atp.lhs precisely $(BCS_HS); (./unlit < atp.lhs && cat $(BCS_HS)) | ./precisely wasm > $@

%.wasm.o: %.c; clang --target=wasm32 -O2 -ffreestanding -c $^ -o $@
%.wasm: %.wasm.o; wasm-ld --import-undefined --no-entry --initial-memory=838860800 $^ -o $@

eq.js: eq.lhs ; -mv Main.jsmod /tmp; hastec --opt-all -Wall $^ && closure-compiler $@ > $@.clo && mv $@.clo $@

menu.html: menu; ./stitch menu menu

%.html: %.lhs menu.html; ./stitch book menu $<
%:%.c;clang -O3 $^ -o $@
webby:webby.c;clang -O3 $^ -o $@ -lm
precisely:precisely.c;clang -O3 $^ -o $@ -lm
reply:reply.c;clang -O3 $^ -o $@ -lm
reply-precompile:reply-precompile.c;clang -O3 $^ -o $@ -lm

vm:vm.c
raw:vm;./vm > raw
lonely.c:vm effectively.hs lonely.hs rts.c raw;(cat rts.c && ./vm run effectively.hs < lonely.hs) > lonely.c
lonely:lonely.c

define rtsup
$(1).c: $(2) $(1).hs rts.c;(cat rts.c && time ./$(2) < $(1).hs) > $$@
endef

REPLYHS=inn/AstPrecisely.hs inn/BasePrecisely.hs inn/KiselyovPrecisely.hs inn/Map1.hs inn/ParserPrecisely.hs inn/RTSPrecisely.hs inn/TyperPrecisely.hs inn/Unify1.hs inn/reply.hs

reply.c: reply-precompile inn/System.hs inn/ReplyImports.hs $(REPLYHS) inn/reply-native.hs; (cat inn/System.hs inn/ReplyImports.hs $(REPLYHS) inn/reply-native.hs | ./precisely ; cat inn/BasePrecisely.hs inn/System.hs inn/ReplyImports.hs | ./reply-precompile | fold -s; cat inn/introspect.c) > $@

reply-precompile.c: precisely inn/System.hs inn/ReplyImports.hs $(REPLYHS) inn/reply-precompile.hs inn/introspect.c; ((cat inn/System.hs inn/ReplyImports.hs $(REPLYHS) inn/reply-precompile.hs) | ./precisely ; cat inn/introspect.c) > $@

DOHSYS=inn/SystemWasm.hs inn/SystemArg.hs
doh.c: reply-precompile $(DOHSYS) inn/ReplyImports.hs $(REPLYHS) inn/reply-wasm.hs; ((cat $(DOHSYS) inn/ReplyImports.hs $(REPLYHS) inn/reply-wasm.hs) | ./precisely wasm ; cat inn/BasePrecisely.hs $(DOHSYS) inn/ReplyImports.hs | ./reply-precompile | fold -s ; cat inn/introspect.c) > $@

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
$(call party,crossly1.c,crossly,Base2 System AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely precisely)
$(call party,precisely.c,crossly1,BasePrecisely System AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely precisely)

$(call party,check.c,precisely,BasePrecisely System AstPrecisely Map1 ParserPrecisely Kiselyov Unify1 RTSPrecisely TyperPrecisely precisely)

$(call party,webby.c,precisely,BasePrecisely System AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely Webby WartsBytes)
$(call party,webby.wasm,webby,BasePrecisely SystemWasm AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely Webby WartsBytes)

$(call party,imp.c,precisely wasm,BasePrecisely SystemWasm AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely Imp WartsBytes)

$(call cat,tmp.hs,BasePrecisely System AstPrecisely Map1 ParserPrecisely KiselyovPrecisely Unify1 RTSPrecisely TyperPrecisely precisely)

#warts.c:crossly;cat inn/Base1.hs inn/SystemWasm.hs | ./crossly warts > $@
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
