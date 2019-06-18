.PHONY: site sync clean target

target: site

FILES=$(addsuffix .html, index socrates lambda scott ION parse quest sing sem grind ioccc type)

menu.html: menu; cobble menu menu

%.html: %.lhs menu.html; cobble mathbook menu $<

site: $(FILES)

sync: site ; rsync -R -r $(FILES) blynn@xenon.stanford.edu:www/compiler/

clean: ; -rm $(FILES)
