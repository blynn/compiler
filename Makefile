.PHONY: site sync clean target

target: site

NAMES=index socrates lambda scott ION parse quest sing sem grind ioccc golf type c

SITE=$(addsuffix .html, $(NAMES)) $(addsuffix .lhs, $(NAMES))

menu.html: menu; cobble menu menu

%.html: %.lhs menu.html; cobble mathbook menu $<

site: $(SITE)

sync: site ; rsync -R -r $(SITE) blynn@xenon.stanford.edu:www/compiler/

clean: ; -rm $(SITE)
