.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION asm quest sing sem grind ioccc golf type c eq para

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES)) para.js

para.js: para.lhs; hastec para.lhs

menu.html: menu; cobble menu menu

%.html: %.lhs menu.html; cobble mathbook menu $<

site: $(SITE)

sync: site ; rsync -R -r $(SITE) blynn@crypto.stanford.edu:www/compiler/

clean: ; -rm $(SITE)
