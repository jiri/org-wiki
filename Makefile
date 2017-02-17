FILES = ./org-wiki.el        \
	./mousetrap.min.js \
	./style.css        \
	./org-wiki-pkg.el

VERSION = 0.3

package:
	mkdir -p "org-wiki-$(VERSION)"
	cp $(FILES) "org-wiki-$(VERSION)/"
	tar -cvf "org-wiki-$(VERSION).tar" "org-wiki-$(VERSION)"/*

clean:
	rm -rf "org-wiki-$(VERSION)/"
	rm -f  "org-wiki-$(VERSION).tar"
