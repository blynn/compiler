.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para logic

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.js eq.js

%.js: %.lhs ; -mv Main.jsmod /tmp; hastec --opt-all -Wall $^; closure-compiler $@ > $@.clo; mv $@.clo $@

menu.html: menu; cobble menu menu

%.html: %.lhs menu.html; cobble mathbook menu $<

site: $(SITE)

sync: site ; rsync -R -r $(SITE) blynn@crypto.stanford.edu:www/compiler/

clean: ; -rm $(SITE)
