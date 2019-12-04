.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para logic differ atp

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.js eq.js differ.js atp.js

%.js: %.lhs ; -mv Main.jsmod /tmp; hastec --opt-all -Wall $^; closure-compiler $@ > $@.clo; mv $@.clo $@

menu.html: menu; cobble menu menu

%.html: %.lhs menu.html; cobble mathbook menu $<

vm:vm.c;cc -O2 $^ -o $@
lonely.c:vm effectively.hs lonely.hs body;(cat body && ./vm run effectively.hs < lonely.hs) > lonely.c
lonely:lonely.c;cc -O2 $^ -o $@
test/mandelbrot.c:test/mandelbrot.hs lonely;(cat body && ./lonely < $<) > $@
test/mandelbrot:test/mandelbrot.c;cc -O2 $^ -o $@

site: $(SITE)

sync: site ; rsync -R -r $(SITE) blynn@crypto.stanford.edu:www/compiler/

clean: ; -rm $(SITE)
